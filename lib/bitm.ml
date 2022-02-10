(** bit matrix *)
type t = {
  m : int;
  (** number of rows *)

  n : int;
  (** number of columns *)

  v : Bitv.t;
  (** row-major bit vector *)
}

let create ~m ~n value =
  let k = m * n in
  if k <= 0 then
    raise (Invalid_argument "create")
  else
    { m; n; v = Bitv.create k value }

let dims {m; n; _} =
  m, n

let raise_oob () =
  raise (Invalid_argument "index out of bounds")

let get { m; n; v } ~i ~j =
  if i < 0 || i >= m || j < 0 || j >= n then raise_oob ();
  let k = i * n + j in
  Bitv.get v k

let set { m; n; v } ~i ~j value =
  if i < 0 || i >= m || j < 0 || j >= n then raise_oob ();
  let k = i * n + j in
  Bitv.set v k value

let of_arrays ~m ~n x =
  let t = create ~m ~n false in
  for i=0 to m-1 do
    let x_i = x.(i) in
    for j=0 to n-1 do
      set t ~i ~j x_i.(j)
    done;
  done;
  t

let to_arrays t =
  let x = Array.make_matrix t.m t.n false in
  for i=0 to t.m-1 do
    let x_i = x.(i) in
    for j=0 to t.n-1 do
      x_i.(j) <- get t ~i ~j
    done
  done;
  x

let iter {m; n; v } f =
  for i = 0 to m-1 do
    for j = 0 to n-1 do
      let x_ij = Bitv.get v (i * n + j) in
      f ~i ~j x_ij
    done;
  done

let row t i =
  Array.init t.n (fun j -> get t ~i ~j)

let col t j =
  Array.init t.m (fun i -> get t ~i ~j)



let fold {m; n; v } f a0 =
  let a = ref a0 in
  for i = 0 to m-1 do
    for j = 0 to n-1 do
      let x_ij = Bitv.get v (i * n + j) in
      a := f !a ~i ~j x_ij
    done;
  done;
  !a


let iter_row { m; n; v } f i =
  if i < 0 || i >= m then raise_oob ();
  let i_n = i * n in
  for j = 0 to n-1 do
    let x_ij = Bitv.get v (i_n + j) in
    f x_ij
  done

let iter_col {m; n; v } f j =
  if j < 0 || j >= n then raise_oob ();
  for i = 0 to m-1 do
    let x_ij = Bitv.get v (i * n + j) in
    f x_ij
  done

let fold_row {m; n; v } f i a0 =
  if i < 0 || i >= m then raise_oob ();
  let i_n = i * n in
  let a = ref a0 in
  for j = 0 to n-1 do
    let x_ij = Bitv.get v (i_n + j) in
    a := f !a x_ij
  done;
  !a

let fold_col {m; n; v } f j a0 =
  if j < 0 || j >= n then raise_oob ();
  let a = ref a0 in
  for i = 0 to m-1 do
    let x_ij = Bitv.get v (i * n + j) in
    a := f !a x_ij
  done;
  !a

let fold2_row {m; n; v } f ~i1 ~i2 a0 =
  if i1 < 0 || i1 >= m || i2 < 0 || i2 >= m then raise_oob ();
  let i1_n = i1 * n in
  let i2_n = i2 * n in
  let a = ref a0 in
  for j = 0 to n-1 do
    let x_i1_j = Bitv.get v (i1_n + j) in
    let x_i2_j = Bitv.get v (i2_n + j) in
    a := f !a x_i1_j x_i2_j
  done;
  !a

let fold2_col {m; n; v } f ~j1 ~j2 a0 =
  if j1 < 0 || j1 >= n || j2 < 0 || j2 >= n then raise_oob ();
  let a = ref a0 in
  for i = 0 to m-1 do
    let x_i_j1 = Bitv.get v (i * n + j1) in
    let x_i_j2 = Bitv.get v (i * n + j2) in
    a := f !a x_i_j1 x_i_j2
  done;
  !a




