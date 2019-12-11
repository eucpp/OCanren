
module Cfg = Configurator.V1

let extract = Cfg.Flags.extract_comma_space_separated_words

let discover_camlp5_flags cfg =
  let camlp5_dir = String.trim @@
    Cfg.Process.run_capture_exn cfg
      "ocamlfind" ["query"; "camlp5"]
  in
  let camlp5_archives =
    List.map
      (fun arch -> String.concat Filename.dir_sep [camlp5_dir; arch])
      ["pa_o.cmo"; "pa_op.cmo"; "pr_o.cmo"]
  in
  Cfg.Flags.write_lines "camlp5-flags.cfg" camlp5_archives

let discover_gt_flags cfg =
  let gt_archives =
    Cfg.Process.run_capture_exn cfg
      "ocamlfind" ["query"; "-pp"; "camlp5"; "-a-format"; "-predicates"; "byte"; "GT,GT.syntax.all"]
  in
  Cfg.Flags.write_lines "gt-flags.cfg" @@ extract gt_archives

let discover_logger_flags cfg =
  let logger_archives =
    Cfg.Process.run_capture_exn cfg
      "ocamlfind" ["query"; "-pp"; "camlp5"; "-a-format"; "-predicates"; "byte"; "logger,logger.syntax"]
  in
  Cfg.Flags.write_lines "logger-flags.cfg" @@ extract logger_archives

(* command line arguments *)

let camlp5_flags  = ref false
let gt_flags      = ref false
let logger_flags  = ref false
let all_flags     = ref false
let all           = ref false

let args = Arg.align @@
  [ ("-camlp5-flags", Arg.Set camlp5_flags  , "discover camlp5 flags" )
  ; ("-gt-flags"    , Arg.Set gt_flags      , "discover GT flags"     )
  ; ("-gt-flags"    , Arg.Set logger_flags  , "discover logger flags" )
  ; ("-all-flags"   , Arg.Set all_flags     , "discover all flags"    )
  ; ("-all"         , Arg.Set all           , "discover all"          )
  ]

let () =

  Cfg.main ~name:"ocanren" ~args (fun cfg ->
    if !camlp5_flags || !all_flags || !all then
      discover_camlp5_flags cfg ;
    if !gt_flags || !all_flags || !all then
      discover_gt_flags cfg ;
    if !logger_flags || !all_flags || !all then
      discover_logger_flags cfg ;
    ()
  )