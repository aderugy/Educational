(** A naive implementation of big integers

This module aims at creating a set of big integers naively. Such data
types will be subsequently called bitarrays. A bitarray is a list of
zeros and ones ; first integer representing the sign bit. In this
contexte zero is reprensented by the empty list []. The list is to
be read from left to right ; this is the opposite convention to the
one you usually write binary decompositions with. After the sign bit
the first encountered bit is the coefficient in front of two to
the power zero. This convention has been chosen to ease writing
down code.

 *)

(** Creates a bitarray from a built-in integer.
    @param x built-in integer.
*)
let from_int x =
  if x = 0 then []
  else let sign = x < 0 in
       let rec aux = function
         |0 -> []
         |n -> (n mod 2)::aux (n / 2)
       in (if sign then 1::aux (-x)
           else 0::aux x);;

(** Transforms bitarray of built-in size to built-in integer.
    UNSAFE: possible integer overflow.
    @param bA bitarray object.
 *)
let to_int = function
  |[] -> 0
  |sb::bA ->
    let sign = if sb = 0 then 1
               else (-1) in
    let rec aux = function
      |[] -> 0
      |bit::bA -> bit + 2 * aux bA
    in sign * aux bA;;

(** Prints bitarray as binary number on standard output.
    @param bA a bitarray.
  *)
let rec print_b = function
  |[] -> print_int 0
  |bit::bA ->
      let rec aux = function
        |[] -> ()
        |bit::bA ->
          begin
            aux bA ;
            print_int bit
          end
    in print_int bit ; aux bA;;

(** Toplevel directive to use print_b as bitarray printer.
    CAREFUL: print_b is then list int printer.
    UNCOMMENT FOR TOPLEVEL USE.
*)
(* #install_printer print_b *)

(** Internal comparisons on bitarrays and naturals. Naturals in this
    context are understood as bitarrays missing a bit sign and thus
    assumed to be non-negative.
*)

(** Comparing naturals. Output is 1 if first argument is bigger than
    second -1 otherwise.
    @param nA A natural, a bitarray having no sign bit.
           Assumed non-negative.
    @param nB A natural.
 *)
let compare_n nA nB =
  let a = List.length nA
  and b = List.length nB in
  if a > b then 1
  else if a < b then (-1)
  else
    let lA = List.rev nA
    and lB = List.rev nB in
    let rec aux = function
      |([], _) |(_, []) -> 0
      |(eA::lA, eB::lB) ->
        if eA = eB then aux (lA, lB)
        else if eA = 1 then 1
        else (-1)
    in aux (lA, lB);;

  (** Bigger inorder comparison operator on naturals. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
   *)
let (>>!) nA nB = compare_n nA nB = 1;;


  (** Smaller inorder comparison operator on naturals. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
   *)
let (<<!) nA nB = compare_n nA nB = (-1);;

  (** Bigger or equal inorder comparison operator on naturals. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
   *)
let (>=!) nA nB = compare_n nA nB <> (-1);;

  (** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
   *)
let (<=!) nA nB = compare_n nA nB <> 1;;

                      (** Comparing two bitarrays. Output is 1 if first argument is bigger
    than second -1 otherwise.
    @param bA A bitarray.
    @param bB A bitarray.
                       *)

let compare_b bA bB = match (bA, bB) with
  |([], []) -> 0
  |([], e::l) -> if e = 1 then 1 else 0
  |(e::l, []) -> if e = 0 then 1 else 0
  |(a::bA, b::bB) ->
    match (a, b) with
    |(0, 0) -> compare_n bA bB
    |(1, 0) -> (-1)
    |(0, 1) -> 1
    |(1, 1) -> (-1) * compare_n bA bB
    |(_, _) -> invalid_arg "compare_b: strange values";;

(** Bigger inorder comparison operator on bitarrays. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>) bA bB = (compare_b bA bB) = 1;;

  (** Smaller inorder comparison operator on bitarrays. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
   *)
let (<<) bA bB = (compare_b bA bB) = (-1);;

(** Bigger or equal inorder comparison operator on bitarrays. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>=) bA bB = (compare_b bA bB) <> (-1);;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<=) bA bB = (compare_b bA bB) <> 1;;


(** Sign of a bitarray.
    @param bA Bitarray.
*)
let sign_b = function
  |[] -> 1
  |a::_ -> if a = 1 then (-1)
           else 1;;

(** Absolute value of bitarray.
    @param bA Bitarray.
*)
let abs_b = function
      |[] -> []
      |_::l -> 0::l;;

(** Quotient of integers smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _quot_t a = if a < 2 then 0 else 1;;

(** Modulo of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _mod_t a = if a = 1 || a = 3 then 1 else 0;;

(** Division of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _div_t a = (_quot_t a, _mod_t a);;

let to_n = function
  |[] -> []
  |_::l -> l;;

(** Addition of two naturals.
    @param nA Natural.
    @param nB Natural.
*)
let add_n nA nB =
  let add a b r =
    let result = a + b + r
    in (result mod 2, result / 2) in

  let rec aux n = function
    |([],[]) -> if n = 0 then [] else [1]
    |([], e::l) | (e::l, []) ->
      let (x, r) = add e 0 n in
      x::aux r (l, [])
    |(a::lA, b::lB) ->
      let (x, r) = add a b n in
      x::aux r (lA, lB)
  in aux 0 (nA, nB);;

(** Difference of two naturals.
    UNSAFE: First entry is assumed to be bigger than second.
    @param nA Natural.
    @param nB Natural.
 *)
let diff_n nA nB =
  (* Logical part of the substraction *)
  let sub a b =
    let result = a - b in
    if result >= 0 then (result, 0)
    else (1, 1) in

  (* Cleans unnecessary 0s in the result*)
  let trim l =
    let rec auxtrim flag = function
      |[] -> []
      |e::l -> if flag then e::l
               else if e = 0 then auxtrim false l
               else e::auxtrim true l
    in List.rev (auxtrim false  (List.rev l)) in

  (* Recursion *)
  let rec aux = function
    |([], []) -> []
    |([], _::_) -> invalid_arg "B > A"
    |(l, []) -> l
    |(a::lA, b::lB) ->
      let (x, r) = sub a b in
      let lB = if r = 0 then lB
               else add_n lB [1] in
      x::aux (lA, lB)
  in trim (aux (nA, nB));;

(** Addition of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
 *)
let add_b bA bB =
  match (bA, bB) with
  |([], b) | (b, []) -> b
  |(sA::bA, sB::bB) ->
    (* Rewriting the operation to use diff_n safely *)
    if sA = sB then sB::add_n bA bB
    else let sign = compare_n bA bB in
         if sA = 1 then if sign = 1 then 1::diff_n bA bB
                        else 0::diff_n bB bA
         else if sign >= 0 then 0::diff_n bA bB
         else 1::diff_n bB bA;;

(** Difference of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let diff_b bA = function
  |[] -> bA
  |e::bB -> add_b bA ((if e = 1 then 0 else 1)::bB);;

(** Shifts bitarray to the left by a given natural number.
    @param bA Bitarray.
    @param d Non-negative integer.
*)
let rec shift bA = function
  |0 -> bA
  |d -> 0::shift bA (d - 1);;


let mult_n bA bB =
  match (bA, bB) with
  |([], _) | (_, []) -> []
  |(bA, bB) ->

  let rec aux offset = function
    |[] -> []
    |e::bB -> add_n (if e = 0 then [] else shift bA offset) (aux (offset + 1) bB)
  in aux 0 bB;;

(** Multiplication of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let mult_b bA bB =
  match (bA, bB) with
  |([], _) | (_, []) -> []
  |(sA::bA, sB::bB) ->

  let rec aux offset = function
    |[] -> []
    |e::bB -> add_n (if e = 0 then [] else shift bA offset) (aux (offset + 1) bB)
  in ((sA + sB) mod 2)::aux 0 bB;;

(* UNSAFE when x < q *)
let shift_quot_n x q =
  let rec aux n s =
    let shifted = shift s 1 in
    if shifted <=! x then aux (add_n n (from_int 1)) shifted
    else n
  in shift q (to_int (aux [] q));;

let quot_n nA nB =
  if nB <=! [] then invalid_arg "division by zero"
  else
    let shift_right = function
      |[] -> invalid_arg "negative shifting"
      |_::l -> l in
    let rec aux a b =
      if b <<! nB then []
      else if a <<! b then 0::aux a (shift_right b)
      else 1::aux (diff_n a b) (shift_right b)
    in let ret = List.rev (aux nA (shift_quot_n nA nB))
       in if ret = [0] then [] else ret;;

(** Quotient of two bitarrays.
    @param bA Bitarray you want to divide by second argument.
    @param bB Bitarray you divide by. Non-zero!
*)
let quot_b bA bB = match (bA, bB) with
  |([], _) | (_, []) -> []
  |(sA::bA, sB::bB) ->
    let sign = (sA + sB) mod 2 in
    let q = quot_n bA bB in
    sign::(if (compare_n (mult_n q bB) bA) = 0 then q
           else if sign = 0 then q
           else (add_n q [1]));;

let mod_n a = function
  |[] |[0] -> []
  |b -> diff_n a (mult_n (quot_n a b) b);;

let rec print = function
  |[] -> print_newline ()
  |e::l -> print_int e ; print l;;

(** Modulo of a bitarray against a positive one.
    @param bA Bitarray the modulo of which you're computing.
    @param bB Bitarray which is modular base.
 *)
let mod_b bA bB = match (bA, bB) with
  |([], _) | (_, []) -> []
  |(sA::a, sB::b) ->
    let l = (match (sA, sB) with
    |(0, 0) -> mod_n a b
    |(1, 0) -> diff_n b (mod_n a b)
    |(_, _) -> invalid_arg "mod_b: strange values") in
    if l = [] then []
    else 0::l;;

(** Integer division of two bitarrays.
    @param bA Bitarray you want to divide.
    @param bB Bitarray you wnat to divide by.
*)
let div_b bA bB = ((quot_b bA bB),(mod_b bA bB));;
