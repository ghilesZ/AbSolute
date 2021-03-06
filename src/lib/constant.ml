let precision       = ref 0.001
let max_iter        = ref 100000000
let max_sol         = ref 10000000
let problem         = ref None
let domain          = ref "box"
let visualization   = ref false
let obj             = ref false
let tex             = ref false
let trace           = ref false
let debug           = ref false
let pruning         = ref false
let sure            = ref false
let rewrite         = ref true
let check           = ref false

let set_prec f =
  if f > 0. then precision := f
  else Tools.fail_fmt "precision must be stricly positive: %f" f

let set_max_iter i =
  if i > 0 then max_iter := i
  else Tools.fail_fmt "number of iterations must be stricly positive: %i" i

let set_max_sol s =
  if s > 0 then max_sol := s
  else Tools.fail_fmt "number of solutions must be stricly positive %i" s

let set_prob s =
  if Sys.file_exists s then problem := Some s
  else Tools.fail_fmt "%s : file not found" s

let set_domain d =
  match d with
  | "box" | "oct" | "poly" | "vpl" | "boxmix" -> domain := d
  | _ -> "domain "^d^" undefined. should be one among : box, boxmix boxCP, poly, oct, vpl" |> failwith

let toggle_rewrite () = rewrite := not (!rewrite)
