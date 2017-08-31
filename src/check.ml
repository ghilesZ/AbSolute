(*****************************************************************)
(* this modules checks that the solver implementation works fine *)
(*****************************************************************)

module CheckBox = Checker.Make(Cartesian.BoxF)

let go() =
  Format.printf "regression test of the solver\n";
  Constant.set_max_iter 10;
  let goods = ref 0 in
  let files = Sys.readdir "tests" in
  Array.iter (fun fn ->
      Format.printf "testing %s\n" fn;
      let prob = Builder.parse (Some ("tests/"^fn)) in
      match CheckBox.check_sure prob with
      | 0 -> Format.printf "could'nt find any solution for problem %s\n" fn
      | nb_sol -> Format.printf "%i solutions verified\n" nb_sol;
                  incr goods
      | exception _ -> Format.printf "problem %a solved incorrectly\n" Csp.print prob
    ) files;
  Format.printf "success : %i/%i\n" (!goods) (Array.length files)

let _ = go ()
