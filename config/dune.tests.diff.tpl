(rule
  (target %test.diff)
  (deps ./test.sh %test.log ./orig/%test.log)
  (mode promote-until-clean)
  (action
    (with-accepted-exit-codes (or 0 1)
      (run ./test.sh %test ./orig/%test.log ./%test.log %{target})
    )
  )
)