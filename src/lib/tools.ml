(* only one instanciation forall variable maps modules *)
module VMap = Mapext.Make(struct type t = string let compare = compare end)
