open MiniKanren
open MiniKanrenStd
open Tester
open Printf
open GT

let show_int       = show(int)
let show_int_opt   = show(option) (show(int))
let show_intl      = show(logic)  (show(int))
let show_intl_optl = show(logic)  (show(option) (show(logic) (show(int))))

let int_opt_reifier = Option.reify ManualReifiers.int

let _ = Option.(
    run_exn show_int 1 q qh (REPR(fun q -> q === !!5));
    runR int_opt_reifier show_int_opt show_intl_optl 1 q qh (REPR(fun q -> q === some !!5));
    runR int_opt_reifier show_int_opt show_intl_optl 1 q qh (REPR(fun q -> q === none ()));
    runR ManualReifiers.int show_int     show_intl   1 q qh (REPR(fun q -> some q === some !!5 ));
    runR int_opt_reifier show_int_opt show_intl_optl 1 q qh (REPR(fun q -> call_fresh (fun w -> q === some w)))
  )

module Result =
  struct

    module X =
      struct
        @type ('a,'b) t = Ok of 'a | Error of 'b with show, gmap
        let fmap f g x = gmap(t) f g x
      end

  include X
  include Fmap2 (X)

  let ok x    = inj @@ distrib (Ok x)
  let error x = inj @@ distrib (Error x)
end

let show1 = show(Result.t) (show(int)) (show(option) (show(int)))
let show1logic =
  show(logic) (show(Result.t) (show(logic) (show int)) (show(logic) (show option @@ show(logic) (show int))))

let runResult n = runR (Result.reify ManualReifiers.int int_opt_reifier) show1 show1logic n

let _ =
  run_exn show1 1  q qh (REPR(fun q -> q === Result.ok !!5 ));
  runResult   (-1) q qh (REPR(fun q -> call_fresh (fun r -> (q === Result.ok r) &&& conde [r === !!5; success])));
  runResult   (-1) q qh (REPR(fun q -> Fresh.two (fun r s -> conde
                                                                [ (q === Result.ok    s) &&& (s =/= !!4)
                                                                ; (q === Result.error r)
                                                                ])
                        ))
