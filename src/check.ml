(*****************************************************************)
(* this modules checks that the solver implementation works fine *)
(*****************************************************************)

module CheckBox = Checker.Make(Cartesian.BoxF)

let _ =
  Format.printf "regression test of the solver\n";
  Constant.set_max_iter 10;
  let goods = ref 0 in
  let files = Sys.readdir "tests" in
    Array.iter (fun fn ->
      Format.printf "%s\t: " fn;
      let prob = Builder.parse (Some ("tests/"^fn)) in
      let res = CheckBox.result prob in
      let known_sol = CheckBox.check_known_solutions prob res in
      match CheckBox.check_sure prob res, known_sol with
      | _,false -> Format.printf "could'nt find any covering abstract element\n"
      | 0,_     -> Format.printf "could'nt find any solution\n"
      | nb_sol,true ->
         Format.printf "%i solutions verified\n" nb_sol;
         incr goods
    ) files;
  Format.printf "success : %i/%i\n" (!goods) (Array.length files)
