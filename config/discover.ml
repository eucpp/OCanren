
module Cfg = Configurator.V1

let () =
  Cfg.main ~name:"ocanren" (fun cfg ->
    let include_dirs =
      Cfg.Process.run_capture_exn cfg
        "ocamlfind" ["query"; "-pp"; "camlp5"; "-i-format"; "GT,GT.syntax.all"]
    in
    let archives =
      Cfg.Process.run_capture_exn cfg
        "ocamlfind" ["query"; "-pp"; "camlp5"; "-a-format"; "GT,GT.syntax.all"]
    in
    let extract = Cfg.Flags.extract_comma_space_separated_words in
    Cfg.Flags.write_sexp "flags.sexp"
      ((extract include_dirs) @ (extract archives))
  )
