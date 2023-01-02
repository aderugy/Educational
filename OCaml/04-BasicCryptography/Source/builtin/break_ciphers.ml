(** Factoring Builtin int primes *)

open Builtin
open Basic_arithmetics

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)
let break key =
  let (pub, e) = key in
  let rec aux n =
    if pub mod n = 0 && n < pub then n
    else aux (n + 2)
  in let q = if pub mod 2 = 0 then 2 else aux 3
     in (q, pub/q);;
