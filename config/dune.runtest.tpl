(rule
  (target %{test}.log)
  (deps %{test}.exe)
  (mode promote-until-clean)
  (action
    (with-stdout-to %{test}.log (run ./%{test}.exe))
  )
)

(rule
  (alias run-%{test})
  (target %{test}.diff)
  (deps ./test.sh %{test}.log ./orig/%{test}.orig)
  (mode promote-until-clean)
  (action
    (with-accepted-exit-codes (or 0 1)
      (run ./test.sh %{test} ./orig/%{test}.orig ./%{test}.log %{target})
    )
  )
)

(rule
  (alias promote-%{test})
  (target %{test}.orig)
  (deps %{test}.log)
  (mode (promote (into orig)))
  (action (copy %{test}.log %{test}.orig))
)

