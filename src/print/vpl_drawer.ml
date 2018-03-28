type t = Vpl_domain.VplCP.t

let bound : t -> Csp.var -> float * float
    = fun _ _ -> (0.,0.)

let draw2d : t -> (Csp.var * Csp.var) -> Graphics.color -> unit
    = fun _ _ _ -> ()

let print : Format.formatter -> t -> unit
    = fun _ _ -> ()

let print_latex : Format.formatter -> t -> (Csp.var * Csp.var) -> Graphics.color -> unit
    = fun _ _ _ _ -> ()

let draw3d : Format.formatter -> t list -> (Csp.var * Csp.var * Csp.var) -> unit
    = fun _ _ _ -> ()
