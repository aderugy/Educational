(** Generating prime bitarrays *)

open Scalable
open Scalable_basic_arithmetics

let sqrt = function
  |[] | [0] -> []
  |n when n <<! [] -> invalid_arg "negative sqrt"
  |n ->
    let rec aux = function
      |x when mult_b x x <<= n -> x
      |x -> aux (diff_b x (from_int 1))
    in aux (quot_b n (from_int 2));;

let is_prime = function
  |n when n <<= (from_int 1) -> false
  |n when n <<= (from_int 3) -> true
  |n when mod_b n (from_int 2) = [] -> false
  |n -> let max = sqrt n in
        let rec aux cpt flag =
          if cpt <<= max
          then flag &&
                 (aux (add_b cpt (from_int 2)) ((mod_b n cpt) <> []))
          else flag
        in aux (from_int 3) true;;

(** List composed of 2 and then odd bitarrays starting at 3.
    @param n upper bound to elements in the list of bitarrays.
 *)
let init_eratosthenes n =
  if n << (from_int 2) then invalid_arg "n < 2"
  else
    let rec aux = function
      |o when (compare_b o (from_int 2)) = 0 -> (from_int 2)::aux (from_int 3)
      |o when o <<= n -> o::aux (add_b o (from_int 2))
      |_ -> []
    in aux (from_int 2);;

(** Eratosthene sieve.
    @param n upper bound to elements in the list of primes, starting
           at 2.
*)
let eratosthenes n =
  let era = init_eratosthenes n in
  let rec aux = function
    |[] -> []
    |o::era -> if is_prime o then o::aux era
               else aux era
  in aux era;;

(** Write a list into a file. Element seperator is newline. Inner
   seperator within elements of an element is ','.
   @param file path to write to.
*)
let write_list li file =
  let oc = open_out file in

  let rec write_b = function
    |[] -> ()
    |bit::[] -> Printf.fprintf oc "%d\n" bit
    |bit::bA -> (Printf.fprintf oc "%d," bit ;
                 write_b bA)
  in let rec aux = function
    |[] -> ()
    |bB::era -> (write_b bB ; aux era)
  in aux li; close_out oc;;

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime bitarrays up to which to build up a list of primes.
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

let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None;;

(** Create a list of bitarrays out of reading a line per line channel.
    @param in_c input channel.  *)
let create_list in_c =
  let rec convert_line s len = function
    |n when n >= len -> []
    |n when n >= 0 -> (if s.[n] = '0' then 0 else 1)::convert_line s len (n + 2)
    |_ -> failwith "Create list: index out of bounds"

  in let rec aux in_c = match input_line_opt in_c with
       |Some line -> (convert_line line (String.length line) 0):: (aux in_c)
       |None -> []
     in aux in_c;;

(** Load list of prime bitarrays into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file =
  let chan = open_in file in
  create_list chan;;

(** Get last element of a list.
    @param l list of prime bitarrays.
 *)
let rec last_element l = match l with
  | [] -> failwith "Scalable.generate_primes.last_element: Youre list \
                    is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two last elements.
    @param l list of prime bitarrays.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "Scalable.generate_primes.last_two: List has \
                          to have at least two elements."
  | e::g::[] -> (e, g)
  | h::t -> last_two t

(** Finding couples of prime bitarrays where second entry is twice the
    first plus 1.
    @param upper bound for searched for prime bitarrays, a built-in integer.
    @param isprime function testing for (pseudo)primality.  *)
let double_primes limit isprime =
  let list = eratosthenes limit in
  let rec aux = function
    |[] -> []
    |bA::list -> let v = add_b (mult_b bA (from_int 2)) (from_int 1) in
                 if isprime v then (bA, v)::aux list
                 else aux list
  in aux list;;

(** Finding twin primes.
    @param upper bound for searched for prime bitarrays, a built-in integer..
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
  let list = eratosthenes limit in
  let rec aux = function
    |[] -> []
    |bA::list when isprime (add_b bA (from_int 2)) ->
      (bA, (add_b bA (from_int 2)))::aux list
    |_::list -> aux list
  in aux list;;
