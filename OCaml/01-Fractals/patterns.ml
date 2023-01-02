let rec build_line n str =
  if n = 0 then "" else str ^ build_line (n - 1) str ;;

build_line 5 "*" ;;
build_line 4 ".*" ;;



let square n str =
  let str2 =
    "\n" ^ build_line n str in print_string ((build_line n str2) ^ "\n") ;; 

square 5 "*" ;;
square 6 "@" ;;


let square2 n (str1, str2) =
  let rec aux n x (str1, str2) =
    if x = 0 then "\n"
    else ("\n" ^ build_line n (
                     if (n - x) mod 2 = 0
                     then str1 ^ str2
                     else str2 ^ str1)
         ) ^ aux n (x - 1) (str1, str2)


  in print_string (aux n n (str1, str2)) ;;

square2 5 ("*", ".") ;;
square2 6 ("&", " ") ;;


let rec triangle n str =
  if n = 0 then ()
  else triangle (n - 1) str ; print_string ((build_line n str) ^ "\n") ;;

triangle 5 "*" ;;
triangle 6 "+" ;;


let pyramid n (str1, str2) =

  let rec aux n x (str1, str2) =
    if x = 1 then print_string "\n"

    else aux n (x - 1) (str1, str2) ; print_string (
             let x = build_line (n - x) str1
             and y = build_line (2*x) str2 in  x ^ y ^ x ^ "\n"
           ) in

  aux n n (str1, str2) ;;

pyramid 5 ( "." , "*" ) ;;
pyramid 6 ( "-" , "|" ) ;;



let cross n (str1, str2) =
  let abs n = if n < 0 then -n else n in

  let rec aux n x (str1, str2) =
    match x with
    |y when y = -n -> print_newline ()

      |y when y = 0 -> print_string (
                           let l = build_line (n - 1) str1
                           in "\n" ^ l ^ str2 ^ l) ;
                       aux n (x - 1) (str1, str2)

      |_ -> print_string (
                let l1 = build_line (n - (abs x) - 1) str1
                and l2 = build_line (2 * abs x - 1) str1 in
                "\n"^l1^str2^l2^str2^l1
              ) ;
                aux n (x - 1) (str1, str2) in

      aux n (n - 1) (str1, str2);;

cross 5 (".", "&") ;;
