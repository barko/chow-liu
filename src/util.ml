(* read contents of a file into a string *)
let string_of_file path =
  let ch = open_in path in
  let b = Buffer.create 1000 in
  (try
     while true do
       Buffer.add_channel b ch 1000;
     done
   with End_of_file ->
     ()
  );
  close_in ch;
  Buffer.contents b

let seq_iter_sep =
  let open Seq in
  let rec loop ~f ~sep seq =
    match seq () with
    | Cons ( a, x ) -> (
      match x () with
        | Nil ->
          f a
        | (Cons ( _, _ ) as s) ->
          f a;
          sep ();
          loop ~f ~sep (fun () -> s)
    )
    | Nil -> ()
  in
  loop

