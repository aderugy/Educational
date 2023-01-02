(** Basic arithmetics for ordered euclidian ring, case of bitarrays. *)

open Scalable;;

let sign = function
  |[] -> 0
  |e::l -> e;;

let abs = function
  |[] -> []
  |e::l -> 0::l;;

let rec gcd a b =
  if b = [] || b = [0] then a
  else gcd b (mod_b a b);;

(** Greater common (positive) divisor of two non-zero integers.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let gcd_b bA bB = match (bA, bB) with
  |([], _) | (_, []) -> []
  |(lA, lB) -> gcd (abs lA) (abs lB);;

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let bezout_b bA bB =
  let gcd = gcd_b bA bB in
  let rec aux m n =
    if n = [] || n = [0] then ([0;1], [])
    else let (x, y) = aux n (mod_b m n) in
         (y, diff_b x (mult_b (quot_b m n) y))
  in let (u, v) = aux bA bB in (u, v, gcd);;
