let project_root = Array.get Sys.argv 1

let () =
  Printf.printf "Project root is: %s\n" project_root

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

let string_match re s = Str.string_match re s 0

let match_fn_ext fn ext =
  String.equal ext @@ Filename.extension fn

let get_testnames () =
  let re = Str.regexp "test*" in
  let check_fn fn =
       (string_match re fn)
    && (match_fn_ext fn ".ml")
    && (not @@ match_fn_ext (Filename.remove_extension fn) ".pp")
    && (not @@ String.equal fn "tester.ml")
  in
  let tests_dir =
    String.concat Filename.dir_sep [project_root; "regression"]
  in
     Sys.readdir tests_dir
  |> Array.to_list
  |> List.filter check_fn
  |> List.map Filename.remove_extension

let testnames = get_testnames ()

let insert_testname tpl testname =
  let re = Str.regexp "%{test}" in
  Str.global_replace re testname tpl

let generate_diffrules () =
  let tpl_fn = "dune.tests.diff.tpl" in
  let gen_fn = "dune.tests.diff.gen" in
  let tpl = read_file tpl_fn in
  let rules = List.map (insert_testname tpl) testnames in
  let content = String.concat "\n" rules in
  let ochan = open_out gen_fn in
  output_string ochan content

let generate () =

  generate_diffrules ()

let () =
  generate ()