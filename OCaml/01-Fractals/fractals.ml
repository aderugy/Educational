#use "topfind" ;;
#require "graphics" ;;

open Graphics ;;
open_graph " 1200x800" ;;


let test (x, y) (z, t) =
  clear_graph () ;
  set_color red ;
  moveto x y ;
  lineto z t ;
  set_color blue ;
  rmoveto x y ;
  rlineto z t ;;

let int e = Random.int (e - 1);;

let abs n = if n < 0 then -n else n;;

let draw_line (x,y) (z,t) =
  moveto x y ;
  lineto z t ;;

let mountain n (x, y) (z, t) =
  clear_graph ();
  set_color red;
  moveto x y;

  let rec aux n (x, y) (z, t) =
    if n = 0
    then draw_line (x, y) (z, t)

    else let u = (x + z) / 2
         and v = (y + t) / 2 + int (abs (z - x) / 5 + 20) in

         aux (n-1) (x, y) (u, v) ;
         aux (n-1) (u, v) (z, t)

  in
  aux n (x, y) (z, t) ;;

let dragon n (x, y) (z, t) =
  clear_graph ();
  set_color black;

  let rec aux n (x, y) (z, t) =
    if n = 0
    then draw_line (x, y) (z, t)
    else
      let u = (x + z) / 2 + (t - y) / 2
      and v = (y + t) / 2 - (z - x) / 2 in

      aux (n - 1) (x, y) (u, v) ;
      aux (n - 1) (z, t) (u, v) in

  aux n (x, y) (z, t);;

let carpet n (x, y) =
  clear_graph ();
  set_color black;
  moveto x y;

  let rec aux n (x, y) =
    if n < 3
    then
      fill_rect x y n n
    else
      (
         aux (n/3) (x, y) ;
         aux (n/3) ((x + n/3), y) ;
         aux (n/3) ((x + 2*n/3), y) ;
         aux (n/3) (x, (y + n/3)) ;
         aux (n/3) (x, (y + 2*n/3)) ;
         aux (n/3) ((x + n/3), (y + 2*n/3)) ;
         aux (n/3) ((x + 2*n/3), (y + n/3)) ;
         aux (n/3) ((x + 2*n/3), (y + 2*n/3)) ;
      )
  in aux n (x, y);;



let sierpinski n (x, y) size =
  clear_graph () ;
  set_color black ;

  let h size = sqrt (size*.size -. (size/.2.)*.(size/.2.)) in

  let dt x y size =
    let x0 = int_of_float x
    and y0 = int_of_float y
    and x1 = int_of_float (x +. size)
    and x2 = int_of_float (x +. size/.2.)
    and y1 = int_of_float (y +. h size)

    in (
        draw_line (x0, y0) (x0, y0) ;
        draw_line (x0, y0) (x1, y0) ;
        draw_line (x0, y0) (x2, y1) ;
        draw_line (x1, y0) (x2, y1)
      )
  in

  let rec aux n size (x, y) =
    if n = 0
    then dt x y size
    else
      (
        dt x y size ;
        aux (n - 1) (size/. 2.) (x, y);
        aux (n - 1) (size/. 2.) ((x +. size/. 2.), y);
        aux (n - 1) (size/. 2.) (
            (x +. size/. 4.), (y +. h (size/. 2.))
          )
      )
  in
  aux n (float_of_int size) (float_of_int x, float_of_int y) ;;


let four_circles r (x, y) limit =
  clear_graph () ;
  set_color black ;

  let rec aux r (x, y) limit =
    if r < limit
    then ()

    else
      begin
        draw_circle x y r ;
        aux (r/2) ((x - r/2), y) limit ;
        aux (r/2) ((x + r/2), y) limit ;
        aux (r/2) (x, (y + r/2)) limit ;
        aux (r/2) (x, (y - r/2)) limit ;
      end

  in aux r (x, y) limit ;;


let arrow r (x, y) o limit =
  clear_graph () ;
  set_color black ;

  let rec aux r (x, y) o limit =
    if r <= limit
    then ()
    else
      match o with
      |'N' ->
        begin
          fill_circle x y r ;
          aux (r/2) ((x - r - r/2), y) 'W' limit ;
          aux (r/2) ((x + r + r/2), y) 'E' limit ;
          aux (r/2) (x, (y + r + r/2)) 'N' limit ;
        end

      |'W' ->
        begin
          fill_circle x y r ;
          aux (r/2) ((x - r - r/2), y) 'W' limit ;
          aux (r/2) (x, (y + r + r/2)) 'N' limit ;
          aux (r/2) (x, (y - r - r/2)) 'S' limit ;
        end

      |'E' ->
        begin
          fill_circle x y r ;
          aux (r/2) ((x + r + r/2), y) 'E' limit ;
          aux (r/2) (x, (y + r + r/2)) 'N' limit ;
          aux (r/2) (x, (y - r - r/2)) 'S' limit ;
        end

      |_ ->
        begin
          fill_circle x y r ;
          aux (r/2) ((x - r - r/2), y) 'W' limit ;
          aux (r/2) ((x + r + r/2), y) 'E' limit ;
          aux (r/2) (x, (y - r - r/2)) 'S' limit ;
        end

   in aux r (x, y) o limit;;




let pytagora_tree n (x, y) size =
  clear_graph () ;
  set_color black ;

  let ds (a, b) (c, d) (e, f) (g, h) =
    (
    let a1 = int_of_float a
    and b1 = int_of_float b
    and c1 = int_of_float c
    and d1 = int_of_float d
    and e1 = int_of_float e
    and f1 = int_of_float f
    and g1 = int_of_float g
    and h1 = int_of_float h in
    moveto a1 b1 ;
    lineto c1 d1 ;
    lineto e1 f1 ;
    lineto g1 h1 ;
    lineto a1 b1
    )

  in draw_rect x y size size ;

  let rec aux n (x, y) (z, t) =
    if n = 0
    then
      ()
    else
      (
        let a = (x +. z) /. 2. -. (t -. (t +. y) /. 2.)
        and b = (y +. t) /. 2. +. (z -. (z +. x) /. 2.)
        in let c = x -. (b -. y)
        and d = y +. (a -. x)
        and e = a -. (b -. y)
        and f = b +. (a -. x)
        and g = a -. (t -. b)
        and h = b +. (z -. a)
        and i = z -. (t -. b)
        and j = t +. (z -. a)
        in ds (x, y) (c, d) (e, f) (a, b) ;
           ds (z, t) (a, b) (g, h) (i, j) ;
           aux (n - 1) (c, d) (e, f) ;
           aux (n - 1) (g, h) (i, j) ;
      )

  in let x1 = float_of_int x
     and y1 = float_of_int y
     and size1 = float_of_int size in
     aux n (x1, (y1 +. size1)) ((x1 +. size1), (y1 +. size1)) ;;


pytagora_tree 15 (250, 5) 100 ;;


let vicsek_star n (x, y) size =
  clear_graph () ;
  set_color black ;

  let rec aux n (x, y) size =
    if n = 0
    then fill_rect x y size size
    else
      begin
        let s = size/3 in
        aux (n - 1) (x, y) s ;
        aux (n - 1) ((x + s), (y + s)) s ;
        aux (n - 1) ((x + 2 * s), y) s ;
        aux (n - 1) (x, (y + 2 * s)) s ;
        aux (n - 1) ((x + 2 * s), (y + 2 * s)) s ;
      end

  in aux n (x, y) size ;;


let koch_curve n (x, y) (z, t) =
  set_color black ;

  let getX (x, y) (z, t) =
    x +. cos (3.14 /. 3.) *. (z -. x) -.
      sin (3.14 /. 3.) *. (t -. y) in

  let getY (x, y) (z, t) =
    y +. sin (3.14 /. 3.)  *. (z-.x) +.
      cos (3.14 /. 3.) *. (t-.y) in

  let dt (x, y) (z, t) =
    moveto (int_of_float x) (int_of_float y) ;

    let ax = getX (x, y) (z, t)

    and ay = getY (x, y) (z, t) in

    lineto (int_of_float ax) (int_of_float ay) ;
    lineto (int_of_float z) (int_of_float t) ;

  in let rec aux n (x, y) (z, t) =
       match n with
       |0 -> dt (x, y) (z, t)
       |1 ->
         begin
           let vx = (z -. x) /. 3.
           and vy = (t -. y) /. 3.
           in
           moveto (int_of_float x) (int_of_float y) ;
           let ax = x +. vx and ay = y +. vy in
           let bx = ax +. vx and by = ay +. vy in
           lineto (int_of_float ax) (int_of_float ay) ;
           dt (ax, ay) (bx, by) ;
           lineto (int_of_float z) (int_of_float t) ;
         end
       |_ ->
         begin
           let vx = (z -. x) /. 3.
           and vy = (t -. y) /. 3. in

           moveto (int_of_float x) (int_of_float y) ;
           let ax = x +. vx and ay = y +. vy in
           let bx = ax +. vx and by = ay +. vy in

           let cx = getX (ax, ay) (bx, by)
           and cy = getY (ax, ay) (bx, by) in

           begin
             aux (n - 1) (x, y) (ax, ay) ;
             aux (n - 1) (ax, ay) (cx, cy) ;
             aux (n - 1) (cx, cy) (bx, by) ;
             aux (n - 1) (bx, by) (z, t) ;
           end
         end


     in aux n (float_of_int x, float_of_int y)
          (float_of_int z, float_of_int t) ;;

let koch_snowflake n (x, y) d =
  clear_graph () ;
  set_color black ;

  let x1 = x + d in

  let getX (x, y) (z, t) =
    x +. cos (3.14 /. 3.) *. (z -. x) -.
      sin (3.14 /. 3.) *. (t -. y) in

  let getY (x, y) (z, t) =
    y +. sin (3.14 /. 3.)  *. (z-.x) +.
      cos (3.14 /. 3.) *. (t-.y) in

  let ax = getX ((float_of_int x), (float_of_int y))
             ((float_of_int x1), (float_of_int y))
    and ay = getY ((float_of_int x), (float_of_int y))
             ((float_of_int x1), (float_of_int y)) in

  begin
    koch_curve n (x1, y)
      (x, y) ;

    koch_curve n (x, y)
      (int_of_float ax,  int_of_float ay) ;

    koch_curve n (int_of_float ax, int_of_float ay)
      (x1, y) ;
  end ;;
