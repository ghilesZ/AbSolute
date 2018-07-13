(*****************************************************************)
(* this modules checks that the solver implementation works fine *)
(*****************************************************************)

module CheckBox = Checker.Make(Cartesian.BoxMix)

let print_sep () =
  Format.printf "-----------------------------------------------------------------\n"

let print_results not_bads goods nb_files =
  Format.printf "%ssuccess : %i/%i with %i confirmed\n"
                (if not_bads = nb_files then "\027[32m" else "\027[31m")
                not_bads
                nb_files
                goods

let print_frontier fmt f =
  if classify_float f <> FP_normal then
    Format.fprintf fmt "(no frontier)"
  else
    Format.fprintf fmt "(frontier %.2f%%)" (f *. 100.)

(* returns the couple (g,b) where g are the known solutions of the problem
   and b are the known nogoods of a problem *)
let split sols =
  List.fold_left (fun (g,ng) (i,b) -> if b then (i::g,ng) else g,(i::ng))
    ([],[]) sols

let _ =
  Random.self_init();
  let dir = "tests/" in
  Format.printf "regression test of the solver\n";
  Format.printf "using the %s domain\n" !Constant.domain;
  print_sep();
  Constant.set_max_iter 15;
  let goods          = ref 0 in
  let not_bads       = ref 0 in
  let problem        = ref 0 in
  let frontier_ratio = ref 0. in
  let files = Sys.readdir dir in
  Array.iter (fun fn ->
      Format.printf "%s : " fn;
      let prob = Builder.parse (Some (dir^fn)) in
      let igoods,ibads = split prob.Csp.solutions in
      let res = CheckBox.result prob in
      let known_sol = CheckBox.check_known_solutions fn res igoods in
      let known_bad = CheckBox.check_known_bad fn res ibads in
      let frontier = CheckBox.check_unsure fn prob res in
      let inner = CheckBox.check_sure fn prob res in
      frontier_ratio := !frontier_ratio +. frontier;
      (match inner, known_sol, known_bad with
       | _,false,_ ->
          incr problem;
          Format.printf "covering element not found for a solution (false negative)"
       | _,_,false ->
          incr problem;
          Format.printf "covering element found for a nogoods (false positive)"
       | 0,true,true  ->
          incr not_bads;
          Format.printf "didn't find any solution"
       | nb_sol,true,true ->
          Format.printf "%i generated solutions" nb_sol;
          incr goods;
          incr not_bads);
      Format.printf "\t%a\n" print_frontier frontier
    ) files;
  print_sep();
  print_results (!not_bads) (!goods) (Array.length files)
