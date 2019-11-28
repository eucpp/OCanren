
(* pretty dumb file reading *)
let read_file fn =
  let ichan = open_in fn in
  let rec helper lines =
    try
      let line = input_line ichan in
      helper (line :: lines)
    with End_of_file -> lines
  in
  let lines = List.rev @@ helper [] in
  String.concat "\n" lines

let testnames =
  [ "test000" ]

let insert_testname tpl testname =
  let re = Str.regexp "%{test}" in
  Str.global_replace re testname tpl

let generate_testrules (tpl_fn, gen_fn) =
  let tpl = read_file tpl_fn in
  let rules = List.map (insert_testname tpl) testnames in
  let content = String.concat "\n" rules in
  let ochan = open_out gen_fn in
  output_string ochan content

let generate () =
  let log_fns = ("dune.tests.log.tpl", "dune.tests.log.gen") in
  let diff_fns = ("dune.tests.diff.tpl", "dune.tests.diff.gen") in
  let fns = [log_fns; diff_fns] in
  List.iter generate_testrules fns

let () =
  generate ()