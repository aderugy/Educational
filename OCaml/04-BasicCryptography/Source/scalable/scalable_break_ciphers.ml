(** Factoring bitarrays into primes *)

open Scalable
open Scalable_basic_arithmetics

(** Factors product of two prime bitarrays.
    @param key is public key of an RSA cryptosystem.
 *)
let break key =
  let (pub, e) = key in
  let rec aux n =
    if (mod_b pub n = []) then n
    else aux (add_b n (from_int 2))
  in let q = if mod_b pub (from_int 2) = [] then (from_int 2)
             else aux (from_int 3)
     in (q, (quot_b pub q));;
