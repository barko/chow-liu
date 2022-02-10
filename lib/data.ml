type error = [ `Empty | `NotBinary | `NotDense | `Irregular ]
exception Err of error

let binary_arrays path =
  let ch = open_in path in
  (* determine number of rows, [m] *)
  let m =
    match Cosovo.IO.of_channel ~no_header:true ch with
    | Error err -> failwith (Cosovo.IO.string_of_error err)
    | Ok (_, row_seq) ->
      Seq.fold_left (fun i _row -> i + 1) 0 row_seq
  in
  close_in ch;

  if m = 0 then
    raise (Err `Empty);

  let matrix = Array.make m [| |] in
  let ch = open_in path in
  match Cosovo.IO.of_channel ~no_header:true ch with
  | Error err -> failwith (Cosovo.IO.string_of_error err)
  | Ok (_, row_seq) ->
    let _m = Seq.fold_left (
      fun i row ->
        (match row with
         | Error err -> failwith (Cosovo.IO.string_of_error err)
         | Ok `Dense r ->
           let bools_rev = List.rev_map (
             function
             | `Int 0 -> false
             | `Int 1 -> true
             | `Int _
             | `Float _
             | `String _ -> raise (Err `NotBinary)
           ) r in
           matrix.(i) <- Array.of_list (List.rev bools_rev);
         | Ok `Sparse _ -> raise (Err `NotDense)
        );
        i + 1
    ) 0 row_seq in
    close_in ch;
    let n = Array.length matrix.(0) in
    for i = 1 to m-1 do
      if Array.length matrix.(i) <> n then
        raise (Err `Irregular)
    done;
    m, n, matrix

let length_of_row = function
  | Error err -> failwith (Cosovo.IO.string_of_error err)
  | Ok (`Dense r) -> List.length r
  | Ok (`Sparse _) -> raise (Err `NotDense)


let binary_matrix path =
  let ch = open_in path in
  (* determine number of rows [m] and columns [n] *)
  let m, n =
    match Cosovo.IO.of_channel ~no_header:true ch with
    | Error err -> failwith (Cosovo.IO.string_of_error err)
    | Ok (_, row_seq) ->
      match row_seq () with
      | Seq.Cons (first_row, row_seq) ->
        let n = length_of_row first_row in
        let m = Seq.fold_left (
          fun i row ->
            if length_of_row row != n then
              raise (Err `Irregular);
            i + 1
        ) 1 row_seq in
        m, n
      | Seq.Nil ->
        raise (Err `Empty);
  in
  close_in ch;

  assert ( m > 0 && n > 0 );

  let matrix = Bitm.create ~m ~n false in
  let ch = open_in path in
  match Cosovo.IO.of_channel ~no_header:true ch with
  | Error err -> failwith (Cosovo.IO.string_of_error err)
  | Ok (_, row_seq) ->
    let _m = Seq.fold_left (
      fun i row ->
        (match row with
         | Error err -> failwith (Cosovo.IO.string_of_error err)
         | Ok `Dense r ->
           List.iteri (
             fun j v ->
               match v with
               | `Int 0 -> Bitm.set matrix ~i ~j false
               | `Int 1 -> Bitm.set matrix ~i ~j true
               | `Int _
               | `Float _
               | `String _ -> raise (Err `NotBinary)
           ) r;
         | Ok `Sparse _ -> raise (Err `NotDense)
        );
        i + 1
    ) 0 row_seq in
    close_in ch;
    matrix

let print ch ~m ~n x =
  for i = 0 to m-1 do
    let x_i = x.(i) in
    for j = 0 to n-1 do
      let x_ij = x_i.(j) in
      output_char ch (if x_ij then '1' else '0');
    done;
    print_newline ()
  done

