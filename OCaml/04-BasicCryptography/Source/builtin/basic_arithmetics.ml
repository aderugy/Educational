(** Basic arithmetics with builtin integers *)

open Builtin

(** Greater common (positive) divisor of two non-zero integers.
    @param a non-zero integer
    @param b non-zero integer
*)
let rec gcd a b =
        if (sign a <> 1 && sign b <> 1) then gcd (-a) (-b)
        else if (b = 0) then a
        else gcd b (modulo a b);;
(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param a non-zero integer
    @param b non-zero integer.
*)
let bezout a b =
  let gcd = gcd a b in
  let rec aux m n =
    if (n = 0) then (1, 0)
    else let (x, y) = aux n (modulo m n) in
         (y, x - (quot m n) * y)
  in let (u, v) = aux a b in (u, v, gcd);;
