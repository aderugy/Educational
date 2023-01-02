(** Generating primes *)

open Builtin
open Basic_arithmetics

let is_prime n = match n with
  |1 -> false
  |n when n <= 3 -> true
  |n when n mod 2 = 0 -> false
  |n ->
    let max = (int_of_float (sqrt (float_of_int n) +. 1.)) in
    let rec aux cpt flag =
      if cpt <= max
      then flag && aux (cpt + 2) (n mod cpt <> 0)
      else flag
    in aux 3 true;;

(** List composed of 2 and then odd integers starting at 3.
    @param n limit of list of odd integers, minimum value is 2.
 *)
let init_eratosthenes n =
  let rec aux = function
    |2 -> 2::(aux 3)
    |o when o <= n -> o::aux (o + 2)
    |_ -> []
  in aux 2;;


(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
*)
let eratosthenes n =
  let era = init_eratosthenes n in
  let rec aux = function
    | [] -> []
    | o::era -> if is_prime o then o::aux era
                else aux era
  in aux era;;

(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)
let write_list li file =
  let oc = open_out file in

  let rec aux = function
    |[] -> ()
    |e::li ->
      begin
        Printf.fprintf oc "%d\n" e ;
        aux li
      end

  in aux li ;
     close_out oc;;

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime numbers up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file =
  write_list (eratosthenes n) file;;

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list out of reading a line per line channel.
    @param in_c input channel.
 *)
let create_list in_c =
  let rec _create_list in_c =
    match input_line_opt in_c with
    | Some line -> (int_of_string line)::(_create_list in_c)
    | None -> []
  in
  _create_list in_c

(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file =
  let chan = open_in file in
  create_list (chan);;

(** Get biggest prime.
    @param l list of prime numbers.
 *)
let rec last_element l = match l with
  | [] -> failwith "You're list is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "List has to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let double_primes limit isprime =
  let list = eratosthenes (limit) in
  let rec aux = function
    |[] -> []
    |p::list when isprime (2*p + 1) -> (p, (2 * p + 1))::aux list
    |_::list -> aux list
  in aux list;;

(** Finding twin primes.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
  let list = eratosthenes (limit) in
  let rec aux = function
    |[] -> []
    |p::list when isprime (p + 2) -> (p, (p + 2))::aux list
    |_::list -> aux list
  in aux list;;
