module IntSet = Set.Make(Int)
module IntMap = Map.Make(Int)

let fold_range =
  let rec loop n f x i =
    if i = n then
      x
    else
      let x = f i x in
      loop n f x (i + 1)
  in
  fun n f x ->
    loop n f x 0

let mean x =
  let m = Array.length x in
  let sum = Array.fold_left ( +. ) 0. x in
  sum /. (float m)

(** negative log-likelihood *)
let nll p =
  -.( log p )

(** [box_cox] is an approximation of [log], but where [box_cox 0.] = [-lambda] *)
let box_cox lambda x =
  (x ** lambda -. 1.) /. lambda

(** negative log-likelihood based on [box_cox] *)
let bcnll ?(alpha=1e-2) p =
  assert ( alpha > 0. );
  -.(box_cox alpha p)

