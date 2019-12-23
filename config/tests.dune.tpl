(executables
  (names
    %{tests}
  )
  (libraries GT OCanren OCanren.syntax)
  (preprocess
    (action
      (run camlp5
        %{read-lines:../config/camlp5-flags.cfg}
        %{read-lines:../config/gt-flags.cfg}
        %{read-lines:../config/logger-flags.cfg}
        %{workspace_root}/camlp5/pa_ocanren.cma
        %{input-file})
    )
  )
  (preprocessor_deps (file %{workspace_root}/camlp5/pa_ocanren.cma))
)


