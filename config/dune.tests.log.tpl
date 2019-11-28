(rule
  (target %{test}.log)
  (deps %{test}.exe)
  (mode promote-until-clean)
  (action
    (with-stdout-to %{test}.log (run ./%{test}.exe))
  )
)