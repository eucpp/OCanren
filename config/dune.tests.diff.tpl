(rule
  (target %test.diff)
  (deps %test.exe %test.log orig/%test.log)
  (mode promote-until-clean)
  (action
    (with-stdout-to %test.diff (diff ./%test.exe %test.log))
  )
)