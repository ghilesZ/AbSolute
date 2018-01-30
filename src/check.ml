(*****************************************************************)
(* this modules checks that the solver implementation works fine *)
(*****************************************************************)

module CheckBox = Checker.Make(Cartesian.BoxF)

let _ =
  Random.self_init();
  Format.printf "regression test of the solver\n";
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
      | _,false -> Format.printf "didn't find any covering element"
      | 0,_     ->
         incr not_bads;
         Format.printf "didn't find any solution"
      | nb_sol,true ->
         Format.printf "%i generated solutions" nb_sol;
         incr goods;
         incr not_bads);
      Format.printf "\t(frontier %.2f%%)\n" (frontier *. 100.)
      ) files;
    Format.printf "------------------------------------------\n";
    Format.printf "success : %i/%i with %i confirmed, (frontier %.2f%%)\n"
                  (!not_bads)
                  (Array.length files)
                  (!goods)
                  (100. *. !frontier_ratio /. (Array.length files |> float))
