(*****************************************************************)
(* this modules checks that the solver implementation works fine *)
(*****************************************************************)

module CheckBox = Checker.Make(Cartesian.BoxMix)

let print_sep () =
  Format.printf "-----------------------------------------------------------------\n"


let print_frontier fmt f =
  if classify_float f <> FP_normal then
    Format.fprintf fmt "(no frontier)"
  else
    Format.fprintf fmt "(frontier %.2f%%)" (f *. 100.)

let print_good fmt () =
  Format.fprintf fmt "%s\xE2\x9C\x94%s" "\027[32m" "\027[0m"

let print_not_bad fmt () =
  Format.fprintf fmt "%s\xE2\x9C\x94%s" "\027[33m" "\027[0m"

let print_bad fmt () =
  Format.fprintf fmt "%s\xE2\x9D\x8C%s" "\027[31m" "\027[0m"

let print_results not_bads goods nb_files =
  Format.printf "%ssuccess : %i/%i with %i confirmed%s\n"
                (if not_bads = nb_files then "\027[32m" else "\027[31m")
                not_bads
                nb_files
                goods
                "\027[0m"

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
  Constant.set_max_iter 10;
  let goods          = ref 0 in
  let not_bads       = ref 0 in
  let problem        = ref 0 in
  let frontier_ratio = ref 0. in
  let files = Sys.readdir dir in
  let mat =
    Array.map (fun fn ->
        let arr = Array.make 4 "" in
        arr.(0) <- Format.asprintf "%s :" fn;
        try
          let prob = Builder.parse (Some (dir^fn)) in
          let igoods,ibads = split prob.Csp.solutions in
          let res = CheckBox.result prob in
          let known_sol = CheckBox.check_known_solutions fn res igoods in
          let known_bad = CheckBox.check_known_bad fn res ibads in
          let frontier = CheckBox.check_unsure fn prob res in
          let inner = CheckBox.check_sure fn prob res in
          frontier_ratio := !frontier_ratio +. frontier;
          arr.(1) <-
            (match inner, known_sol, known_bad with
             | _,false,_ ->
                arr.(3) <- Format.asprintf "%a" print_bad ();
                incr problem;
                Format.asprintf "covering element missing (false negative)"
             | _,_,false ->
                incr problem;
                Format.asprintf "covering element exceeding (false positive)"
             | 0,true,true  ->
                arr.(3) <- Format.asprintf "%a" print_not_bad ();
                incr not_bads;
                Format.asprintf "didn't find any solution"
             | nb_sol,true,true ->
                arr.(3) <- Format.asprintf "%a" print_good ();
                incr goods;
                incr not_bads;
                Format.asprintf "%i generated solutions" nb_sol);
          arr.(2) <- Format.asprintf "%a" print_frontier frontier;
          arr
        with e ->
          let msg = Printexc.to_string e in
          arr.(1) <- Format.asprintf "the solver crashed :( ";
          arr.(2) <- msg;
          arr.(3) <- Format.asprintf "%a" print_bad ();
          arr
      ) files
  in
  Format.printf "%a" Tools.matrix_print_indent mat;
  print_sep();
  print_results (!not_bads) (!goods) (Array.length files)
