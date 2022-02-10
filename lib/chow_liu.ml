(** implementation of Chow-Liu algorithm *)

(* returns mutual information between variables [a] and [b] based on
   binary data matrix [x] *)
let mutual_information_of_pair x ~a ~b =
  let m, c00, c01, c10, c11 = Bitm.fold2_col x (
    fun (i, c00, c01, c10, c11) x_i_a x_i_b ->
      match x_i_a, x_i_b with
      | false, false -> i + 1, c00 + 1, c01    , c10    , c11
      | false, true  -> i + 1, c00    , c01 + 1, c10    , c11
      | true,  false -> i + 1, c00    , c01    , c10 + 1, c11
      | true,  true  -> i + 1, c00    , c01    , c10    , c11 + 1
  ) (0, 0, 0, 0, 0) ~j1:a ~j2:b in
  let a0 = c00 + c01 in
  let a1 = c10 + c11 in
  let b0 = c00 + c10 in
  let b1 = c01 + c11 in
  assert ( a0 + a1 = m );
  assert ( b0 + b1 = m );
  let m = float m in
  let p00 = (float c00) /. m in
  let p01 = (float c01) /. m in
  let p10 = (float c10) /. m in
  let p11 = (float c11) /. m in

  let p0_ = (float a0) /. m in
  let p1_ = (float a1) /. m in
  let p_0 = (float b0) /. m in
  let p_1 = (float b1) /. m in

  (if p00 > 0. then
     p00 *. log (p00 /. (p0_ *. p_0))
   else
     0.
  ) +. (
    if p01 > 0. then
      p01 *. log (p01 /. (p0_ *. p_1))
    else
      0.
  ) +. (
    if p10 > 0. then
      p10 *. log (p10 /. (p1_ *. p_0))
    else
      0.
  ) +. (
    if p11 > 0. then
      p11 *. log (p11 /. (p1_ *. p_1))
    else
      0.
  )

let weighted_mutual_information_of_pair w x ~a ~b =
  let _, m, c00, c01, c10, c11 = Bitm.fold2_col x (
    fun (i, m, c00, c01, c10, c11) x_i_a x_i_b ->
      let w_i = w.(i) in
      match x_i_a, x_i_b with
      | false, false -> i + 1, m +. w_i, c00 +. w_i, c01       , c10       , c11
      | false, true  -> i + 1, m +. w_i, c00       , c01 +. w_i, c10       , c11
      | true,  false -> i + 1, m +. w_i, c00       , c01       , c10 +. w_i, c11
      | true,  true  -> i + 1, m +. w_i, c00       , c01       , c10       , c11 +. w_i
  ) (0, 0., 0., 0., 0., 0.) ~j1:a ~j2:b in
  let a0 = c00 +. c01 in
  let a1 = c10 +. c11 in
  let b0 = c00 +. c10 in
  let b1 = c01 +. c11 in
  (*
  assert ( a0 + a1 = m );
  assert ( b0 + b1 = m );
  *)
  let p00 = c00 /. m in
  let p01 = c01 /. m in
  let p10 = c10 /. m in
  let p11 = c11 /. m in

  let p0_ = a0 /. m in
  let p1_ = a1 /. m in
  let p_0 = b0 /. m in
  let p_1 = b1 /. m in

  (if p00 > 0. then
     p00 *. log (p00 /. (p0_ *. p_0))
   else
     0.
  ) +. (
    if p01 > 0. then
      p01 *. log (p01 /. (p0_ *. p_1))
    else
      0.
  ) +. (
    if p10 > 0. then
      p10 *. log (p10 /. (p1_ *. p_0))
    else
      0.
  ) +. (
    if p11 > 0. then
      p11 *. log (p11 /. (p1_ *. p_1))
    else
      0.
  )

let mi x n f =
  let pairs = Util.fold_range n (
    fun a pairs ->
      Util.fold_range a (
        fun b pairs ->
          (a, b, f x ~a ~b) :: pairs
      ) pairs
  ) [] in
  pairs

let mutual_information x n =
  mi x n mutual_information_of_pair

let weighted_mutual_information x n w =
  mi x n (weighted_mutual_information_of_pair w)

module Int = struct
  include Int
  let hash = Hashtbl.hash
end

module G = struct
  module Float = struct
    include Float
    let default = nan
  end
  include Graph.Persistent.Digraph.ConcreteLabeled(Int)(Float)
end

module W = struct
  type edge = G.E.t
  type t = G.E.label
  let weight = G.E.label
  let zero = Float.zero
  let add = Float.add
  let sub = Float.sub
  let compare = Float.compare
end

module MWST = Graph.Kruskal.Make(G)(W)
module IG = Graph.Persistent.Graph.Concrete(Int)

let log_freq_smooth =
  let alpha = 0.01 in
  fun numer denom ->
    let numer' = (float numer) +. 2. *. alpha in
    let denom' = (float denom) +. 4. *. alpha in
    Some ((log numer') -. (log denom'))

let log_freq numer denom =
  if numer = 0 then
    None
  else
    let () = assert ( denom > 0 ) in
    Some ((log (float numer)) -. (log (float denom)))


module CPD = struct

  (* [CPD.t] models the conditional probability distribution [p(a|b) =
     p(a,b)/p(b)] over binary variables [a] and [b]. *)
  type t = {
    a : int;
    (* index of variable [a] *)

    b : int;
    (* index of variable [b] *)

    p_00 : float option;
    (* p(a=0|b=0) *)

    p_10 : float option;
    (* p(a=1|b=0) = 1 - p(a=0|b=0) *)

    p_01 : float option;
    (* p(a=0|b=1) *)

    p_11 : float option;
    (* p(a=1|b=1) = 1 - p(a=0|b-1) *)
  }

  let create x ~a ~b =
    let m, c00, c01, c10, c11 = Bitm.fold2_col x (
      fun (i, c00, c01, c10, c11) x_i_a x_i_b ->
        match x_i_a, x_i_b with
        | false, false -> i + 1, c00 + 1, c01    , c10    , c11
        | false, true  -> i + 1, c00    , c01 + 1, c10    , c11
        | true,  false -> i + 1, c00    , c01    , c10 + 1, c11
        | true,  true  -> i + 1, c00    , c01    , c10    , c11 + 1
    ) (0, 0, 0, 0, 0) ~j1:a ~j2:b in
    let a0 = c00 + c01 in
    let a1 = c10 + c11 in
    let b0 = c00 + c10 in
    let b1 = c01 + c11 in
    assert ( a0 + a1 = m );
    assert ( b0 + b1 = m );
    let p_00 = log_freq c00 b0 in
    let p_10 = log_freq c10 b0 in
    let p_01 = log_freq c01 b1 in
    let p_11 = log_freq c11 b1 in
    { a; b; p_00; p_01; p_10; p_11 }

  let p_00 { p_00; _ } = p_00
  let p_01 { p_01; _ } = p_01
  let p_10 { p_10; _ } = p_10
  let p_11 { p_11; _ } = p_11

  let eval t ~a ~b =
    match a, b with
    | false, false -> p_00 t
    | false, true  -> p_01 t
    | true,  false -> p_10 t
    | true,  true  -> p_11 t

end

type t = {
  root : int;
  p_1 : float option;
  p_0 : float option;
  cpds : CPD.t list
}

module IM = Util.IntMap
module IS = Util.IntSet

(* collected edges of a DFS-traversal of a graph *)
let fold_dfs =
  let rec loop f x graph v visited_set =
    let visited_set = IS.add v visited_set in
    let kids = IG.succ graph v in
    List.fold_left (
      fun (x, visited_set) k ->
        if IS.mem k visited_set then
          x, visited_set
        else
          let x = f v k x in
          loop f x graph k visited_set
    ) (x, visited_set) kids
  in
  fun f x graph v ->
    let x, _ = loop f x graph v IS.empty in
    x

let create ?w x =
  let m, n = Bitm.dims x in
  let f_mi =
    match w with
    | None -> mutual_information_of_pair
    | Some w ->
      assert (Array.length w = m);
      weighted_mutual_information_of_pair w
  in

  let graph = Util.fold_range n (
    fun a graph ->
      Util.fold_range a (
        fun b graph ->
          let mi_a_b = f_mi x ~a ~b in
          let edge_a_b = G.E.create a (-. mi_a_b) b in
          G.add_edge_e graph edge_a_b
      ) graph
  ) G.empty in
  let edges = MWST.spanningtree graph in
  let ig = List.fold_left (
    fun ig (src, _mi, dst) ->
      IG.add_edge ig src dst
  ) IG.empty edges in

  (* arbitrarily pick a root node; (perhaps counter-intuitively), any
     choice leads to an equivalent model *)
  let root = 0 in
  let cpds = fold_dfs (
    fun b a cpds ->
      let cpd = CPD.create x ~a ~b in
      cpd :: cpds
  ) [] ig root in

  let c1 = Bitm.fold_col x (
    fun c1 x_i ->
      c1 + (if x_i then 1 else 0)
  ) 0 root in
  let c0 = m - c1 in
  let p_1 = log_freq c1 m in
  let p_0 = log_freq c0 m in
  { root; cpds; p_1; p_0 }

let eval t x =
  let p_root =
    if x.(t.root) then
      t.p_1
    else
      t.p_0
  in
  List.fold_left (
    fun p cpd ->
      let p_a_b = CPD.eval cpd ~a:x.(cpd.a) ~b:x.(cpd.b) in
      match p, p_a_b with
      | None, _
      | _, None -> None (* zero probability *)
      | Some p, Some p_a_b -> Some (p +. p_a_b)
  ) p_root t.cpds

let sample =
  let sample_cpd rng rand1 { CPD.a; b; p_10; p_11; _ } assignment =
    let b = IM.find b assignment in
    let p_1 =
      match b with
      | true -> p_11
      | false -> p_10
    in
    let v, rng =
      match p_1 with
      | None -> false, rng
      | Some p_1 ->
        let v, rng = rand1 rng in
        if v <= exp p_1 then
          true, rng
        else
          false, rng
    in
    let assignment = IM.add a v assignment in
    assignment, rng
  in
  fun rng rand1 { root; p_1; cpds; _ } ->
    let v, rng =
      match p_1 with
      | None -> false, rng
      | Some p_1 ->
        let v, rng = rand1 rng in
        if v <= exp p_1 then
          true, rng
        else
          false, rng
    in
    let assignment = IM.add root v IM.empty in
    List.fold_left (
      fun (assignment, rng) cpd ->
        sample_cpd rng rand1 cpd assignment
    ) (assignment, rng) cpds
