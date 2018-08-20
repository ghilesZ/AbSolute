open Cartesian.BoxMix

type t = Cartesian.BoxMix.t

let print = Cartesian.BoxMix.print

let bound abs v = find v abs |> I.to_float_range

let draw draw_f fillpol fillcircle abs (v1,v2) col =
  let (xl,xu) = bound abs v1 and (yl,yu) = bound abs v2 in
  if xl=xu && yl=yu then fillcircle (xl,yl) 5 col
  else fillpol [(xl,yl); (xl,yu); (xu,yu); (xu,yl)] col;
  let itv1 = find v1 abs
  and itv2 = find v2 abs in
  let (xl,xu) = itv1 |> I.to_float_range
  and (yl,yu) = itv2 |> I.to_float_range in
  let draw_seg vert value (b,c) =
    if vert then draw_f (value,b) (value,c) Graphics.black
    else draw_f (b,value) (c,value) Graphics.black
  in
  draw_seg true xl (yl,yu);
  draw_seg true xu (yl,yu);
  draw_seg false yl (xl,xu);
  draw_seg false yu (xl,xu)

let fill fillbox abs (v1,v2) col =
  let (xl,xu) = bound abs v1 and (yl,yu) = bound abs v2 in
  fillbox (xl,yl) (xu,yu) col

let draw2d = View.(draw draw_seg fill_poly fill_circle)

let print_latex fmt = Latex.(fill (filldrawbox fmt))

let draw3d fmt abs_list (v1,v2,v3) =
  let make_cube (a,b) (c,d) (e,f) = ((a,c,e), b-.a, d-.c, f-.e) in
  let cube e = make_cube (bound e v1) (bound e v2) (bound e v3) in
  let cubes = List.rev_map (fun e -> cube e) abs_list in
  let o = Objgen.build_cube_list cubes in
  Format.fprintf fmt "%a" Objgen.print o
