
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

let gen_tests () =
  let tests = ["test000"] in
  let tpl = read_file "dune.tests.log.tpl" in
  let re = Str.regexp "%test" in
  let genrule s = Str.global_replace re s tpl in
  let content = String.concat "\n" @@ List.map genrule tests in
  let ochan = open_out "dune.tests.gen" in
  output_string ochan content

let () =
  gen_tests ()