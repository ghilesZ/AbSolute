open Graphics

(* window settings *)
let padding = 15.
let x_min = ref 0.
and x_max = ref 0.
and y_min = ref 0.
and y_max = ref 0.
and sx = ref 0.
and sy = ref 0.

(********************************)
(* drawing related computations *)
(********************************)

(* vector determinant for convex hull *)
let det (dx1,dy1) (dx2,dy2) = dx1 *. dy2 -. dy1 *. dx2

(* convex hull computation *)
let hull = function
  | [] -> failwith "can't build convex envelop with no points"
  | h::t as l ->
     let p = List.fold_left min h t in
     let ccw (px,py) (ax,ay) (bx,by) = det (ax-.px,ay-.py) (bx-.px,by-.py) in
     let cmp p1 p2 =
       if p1 = p then 1
       else if p2 = p then -1
       else
         let ccw = ccw p p1 p2 in
         if ccw < 0. then 1
         else if ccw = 0. then 0
         else -1
     in
     let rec graham_aux cl conv =
       match cl,conv with
       | ([],_) -> conv
       | (h::t, a::b::tl) ->
          let p = ccw b a h in
          if p <= 0. then graham_aux cl (b::tl)
          else graham_aux t (h::conv)
       | (h::t,_) -> graham_aux t (h::conv)
     in graham_aux (List.sort cmp l) [p]

(* project from a value from [a;b] to [c;d] *)
let projection (a,b) (c,d) n =
  let perc (x,y) r = x +. (r *. (y-.x))
  and to_perc (x,y) r =
    if x < 0. then (r-.x) /. (y-.x)
    else (r-.x) /. (y-.x)
  in
  if b = a then c else perc (c,d) (to_perc (a,b) n)

(* bring all coordinate to window size *)
let normalize p =
  let to_coord (min_x,max_x) (min_y,max_y) (a,b) =
    let a = projection (min_x,max_x) (padding, (!sx-.padding)) a
    and b = projection (min_y,max_y) (padding, (!sy-. (2. *. padding))) b
    in (int_of_float a, int_of_float b)
  in
  to_coord (!x_min,!x_max) (!y_min,!y_max) p

(***********************************************************************)
(*                        Drawing utilities                            *)
(***********************************************************************)

let fill_circle a b r =
  let a,b = normalize (a,b) in
  fill_circle a b r

(* build the array corresponding to the list of points normalized and
   apply f to it *)
let do_poly f l col =
  set_color col;
  let l = hull l in
  let n = List.length l in
  let arr = Array.make n (0,0) in
  List.iteri (fun i (x,y) -> arr.(i) <- normalize (x,y)) l;
  f arr

let fill_poly = do_poly fill_poly

let draw_poly = do_poly draw_poly

let draw_seg p1 p2 col =
  set_color col;
  let x1,y1 = normalize p1
  and x2,y2 = normalize p2
  in draw_segments [|(x1, y1, x2, y2)|]

let draw_dashed_seg ((x1,y1) as p1) (x2,y2) col =
  let man_dist = abs_float (x2-.x1) +. abs_float (y2-.y1) in
  let step = 1. in
  let move (a,b) =
    (a+.((x2-.x1)*.step/.man_dist)),
    (b+.((y2-.y1)*.step/.man_dist))
  in
  let rec aux (cur_x,cur_y) =
    if abs_float (x2-.cur_x)+.abs_float (y2-.cur_y) < 2. *. step then
      draw_seg (cur_x,cur_y) (x2,y2) col
    else begin
      let next = move (cur_x,cur_y) in
      draw_seg (cur_x,cur_y) next col;
      aux (move next)
    end
  in aux p1

let draw_line_x x col = draw_seg (x, 0.) (x, size_y() |> float_of_int) col

let draw_line_y y col = draw_seg (0., y) ((size_x() |> float_of_int
), y) col

let draw_string x y str col =
  set_color col;
  moveto (int_of_float x) (int_of_float y);
  draw_string str


(***********************************************************************)
(*                        Window Initialization                        *)
(***********************************************************************)

let init (xl,xu) (yl,yu) =
  x_min := xl;
  x_max := xu;
  y_min := yl;
  y_max := yu

let draw_end () =
  let lg = rgb 200 200 200
  and sx = size_x() |> float_of_int
  and sy = size_y() |> float_of_int in
  draw_line_x padding lg;
  draw_string padding (padding/.2. -. 5.) (string_of_float !x_min) black;
  draw_line_x (sx-.padding) lg;
  draw_string (sx-.padding) (padding/.2. -. 5.) (string_of_float !x_max) black;
  draw_line_y padding lg;
  draw_string (padding/.2.-.5.) (padding+.5.) (string_of_float !y_min) black;
  draw_line_y (sy -. padding) lg;
  draw_string (padding/.2.-.5.) (sy-.padding+.5.) (string_of_float !y_max) black

let loop state =
  loop_at_exit [] (fun _ -> ())

let create_window width height =
  Format.sprintf " %ix%i" width height |> open_graph;
  sx := size_x() |> float;
  sy := size_y() - 10 |> float;
  set_window_title "AbSolute";
  loop ()
