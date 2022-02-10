let train input_path output_path =
  let open Pgm in
  let x = Data.binary_matrix input_path in
  let cl = Chow_liu.create x in
  let out_ch = open_out output_path in
  let out_buf = Bi_outbuf.create_channel_writer out_ch in
  Model_j.write_cl out_buf cl;
  Bi_outbuf.flush_channel_writer out_buf;
  close_out out_ch

let complete data_file_path model_file_path verbose =
  let x = Pgm.Data.binary_matrix data_file_path in
  let cl =
    let s = Util.string_of_file model_file_path in
    Pgm.Model_j.cl_of_string s
  in
  let open Pgm in
  let m, _ = Bitm.dims x in
  let pr =
    if verbose then
      Printf.printf "%0.3e %+0.3f\n"
    else
      (fun _ _ -> ())
  in

  let lls = Util.fold_range m (
    fun i lls ->
      let x_i = Bitm.row x i in
      let ll = Chow_liu.eval cl x_i in
      let p, ll =
        match ll with
        | None -> 0.0, neg_infinity
        | Some ll -> exp ll, ll
      in
      pr p ll;
      ll :: lls
  ) [] in
  let mean_lls =
    let sum = List.fold_left ( +. ) 0. lls in
    sum /. (float m)
  in
  Printf.printf "mean LL = %f\n" mean_lls


let sample model_path k =
  let rng = PRNG.Chacha.Pure.make [| 1999; 5; 17 |] in
  let rand1 r =
    PRNG.Chacha.Pure.float 1.0 r
  in

  let cl =
    let s = Util.string_of_file model_path in
    Pgm.Model_j.cl_of_string s
  in

  let rng = Pgm.Util.fold_range k (
    fun _ rng ->
      let assignment, rng = Pgm.Chow_liu.sample rng rand1 cl in
      let ass_seq = Pgm.Util.IntMap.to_seq assignment in
      Util.seq_iter_sep
        ~f:(fun (_id, v) -> print_char (if v then '1' else '0'))
        ~sep:(fun () -> print_char ','
        ) ass_seq;
      print_newline ();
      rng
  ) rng in
  ignore rng


open Cmdliner

let _ =
  let main_cmd =
    let doc = "train and evaluate Chow-Liu models" in
    Term.(pure ()), Term.info "cl" ~doc
  in
  let train_cmd =
    let doc = "train a Chow-Liu model" in
    let input_path =
      let doc = "path of training set file" in
      Arg.(required & opt (some file) None & info ["i";"input"] ~docv:"FILE" ~doc)
    in
    let output_path =
      let doc = "path of output model file" in
      Arg.(required & opt (some string) None & info ["o";"output";"m";"model"]
             ~docv:"FILE" ~doc)
    in

    Term.(pure train $ input_path $ output_path ), Term.info "train" ~doc
  in

  let input_model_path =
    let doc = "path of model file" in
    Arg.(required & opt (some file) None & info ["m";"model"] ~docv:"FILE" ~doc)
  in

  let eval_cmd =
    let doc = "apply complete inference to a Chow-Liu model" in
    let data_file =
      let doc = "path of data file" in
      Arg.(required & opt (some file) None & info ["i";"input"] ~docv:"FILE" ~doc)
    in
    let verbose =
      let doc = "print every probability estimate and its log-likelihood" in
      let info = Arg.info ["v"; "verbvose"] ~doc in
      Arg.value (Arg.flag info)
    in
    Term.(pure complete $ data_file $ input_model_path $ verbose ), Term.info "complete" ~doc
  in

  let sample_cmd =
    let doc = "sample random assigments from Chow-Liu model" in
    let k =
      let doc = "number of samples" in
      Arg.(required & opt (some int) None & info ["k";"count"] ~docv:"FILE" ~doc)
    in
    Term.(pure sample $ input_model_path $ k), Term.info "sample" ~doc
  in

  let commands = [ train_cmd; eval_cmd; sample_cmd ] in
  let exit_code =
    match Term.eval_choice ~catch:false main_cmd commands with
    | `Error _ -> 1
    | _ -> 0
  in
  exit exit_code
