(*****************************************************************)
(* this modules checks that the solver implementation works fine *)
(*****************************************************************)

module CheckBox = Checker.Make(Cartesian.BoxF)

let print_sep () =
  Format.printf "-----------------------------------------------------------------\n"

let print_results not_bads goods nb_files =
  Format.printf "%s success : %i/%i with %i confirmed\n"
                (if not_bads = nb_files then "\027[32m" else "\027[31m")
                not_bads
                nb_files
                goods

let print_frontier fmt f =
  if classify_float f <> FP_normal then
    Format.fprintf fmt "(no frontier)"
  else
    Format.fprintf fmt "(frontier %.2f%%)" (f *. 100.)

let _ =
  Random.self_init();
  Format.printf "regression test of the solver\n";
  print_sep();
  Constant.set_max_iter 10;
  let goods = ref 0 in
  let not_bads = ref 0 in
  let frontier_ratio = ref 0. in
  let files = Sys.readdir "tests" in
  Array.iter (fun fn ->
      Format.printf "%s\t: " fn;
      let prob = Builder.parse (Some ("tests/"^fn)) in
      let res = CheckBox.result prob in
      let known_sol = CheckBox.check_known_solutions prob res in
      let frontier = CheckBox.check_unsure prob res in
      frontier_ratio := !frontier_ratio +. frontier;
      (match CheckBox.check_sure prob res, known_sol with
       | _,false ->
          Format.printf "covering element not found"
       | 0,true  ->
          incr not_bads;
          Format.printf "didn't find any solution"
       | nb_sol,true ->
          Format.printf "%i generated solutions" nb_sol;
          incr goods;
          incr not_bads);
      Format.printf "\t%a\n" print_frontier frontier
    ) files;
  print_sep();
  print_results (!not_bads) (!goods) (Array.length files)
