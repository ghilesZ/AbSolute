(******************************************************************)
(*                   entry point of the solver                    *)
(******************************************************************)

(******************************************************************)
(* An instance of the solver is parmetrized by an abstract domain *)
(* which will be used in the solving process and a  rendering     *)
(* module witch fits the domain we use                            *)
(******************************************************************)

open Drawer_sig

module GoS (Abs:Domain_signature.AbstractCP)(Dr:Drawer with type t = Abs.t) = struct
  module Sol = Solver.Solve(Abs)
  module Print = Out.Make(Dr)

  let go prob =
    let res = Sol.solving prob in
    Format.printf "end of the solving:\n%a\n" Sol.print res;
    Print.out prob res
end

module GoS2 (Abs:Domain_signature2.AbstractCP)(Dr:Drawer with type t = Abs.t) = struct
  module Sol = Solver2.Make(Abs)
  module Print = Out.Make(Dr)

  let go prob =
    let res = Sol.solve prob in
    Format.printf "end of the solving:\n%a\n" Sol.print res;
    Print.out prob res
end


(************************)
(* THE SOLVER INSTANCES *)
(************************)

(* interval domain instance. Only large constraints *)
module SBox      = GoS (Cartesian.BoxF)(Box_drawer)

(* module SBox      = GoS2 (Wrapper.WCBoxF)(Wrapper_drawer) *)

(* apron domain based instances *)
module SOctCP    = GoS (Relational.OctBoxCP)(Apron_drawer.OctDrawer)
module SPolyCP   = GoS (Relational.PolyCP)(Apron_drawer.PolyDrawer)

(* VPL domain based instance *)
module SVplCP = GoS (Vpl_domain.VplCP)(Vpl_drawer)

(********************)
(* OPTIONS HANDLING *)
(********************)

let speclist =
  let open Constant in
  [
  ("-visualization", Arg.Set visualization   , "Enables visualization mode");
  ("-precision"    , Arg.Float set_prec      , "Changes the precision. default is 1e-3");
  ("-max_sol"      , Arg.Int set_max_sol     , "Changes the maximum number of solutions. default is 1e6");
  ("-max_iter"     , Arg.Int set_max_iter    , "Changes the maximum number of iterations. default is 1e7");
  ("-domain"       , Arg.String set_domain   , "Changes the domain used for the solving. default is box");
  ("-obj"          , Arg.Set obj             , "Generates an .obj file (for 3D visualization)");
  ("-tex"          , Arg.Set tex             , "Prints the solutions in latex format on stadard output");
  ("-pruning"      , Arg.Set pruning         , "Enables the \"pruning\" during the solving process");
  ("-trace"        , Arg.Set trace           , "Prints the solutions on standard output");
  ("-sure"         , Arg.Set sure            , "Keeps only the sure solutions");
  ("-no-rewrite"   , Arg.Set rewrite         , "Disables the constraint rewriting");
  ("-debug"        , Arg.Set debug           , "Prints the execution for debug purpose");
  (*********************************************** ALIASES ************************************************)
  ("-t"            , Arg.Set trace           , "Alias for -trace");
  ("-s"            , Arg.Set sure            , "Alias for -sure");
  ("-v"            , Arg.Set visualization   , "Alias for -visualization");
  ("-p"            , Arg.Float set_prec      , "Alias for -precision");
  ("-d"            , Arg.String set_domain   , "Alias for -domain");
  ]

let anonymous_arg = Constant.set_prob

let parse_args () = Arg.parse speclist anonymous_arg ""

(***************)
(* entry point *)
(***************)

let go () =
  let open Constant in
  parse_args ();
  let prob = Builder.parse !problem in
  if !trace then Format.printf "%a" Csp.print prob;
  match !domain with
  | "box"   -> SBox.go prob
  | "oct"   -> SOctCP.go prob
  | "poly"  -> SPolyCP.go prob
  | "vpl" -> SVplCP.go prob
  | _ -> "domain undefined "^(!domain)^". should be one among : box, poly, oct, vpl" |> failwith

let _ = go()
