(executables
  (names
    %{tests}
  )
  (libraries GT OCanren OCanren.syntax)
  (flags (:standard -warn-error -A -rectypes))
  (preprocess
    (action
      (run camlp5
        %{read-lines:../config/camlp5-flags}
        %{read-lines:../config/gt-flags}
        %{read-lines:../config/logger-flags}
        %{workspace_root}/camlp5/pa_ocanren.cma
        %{input-file})
    )
  )
  (preprocessor_deps (file %{workspace_root}/camlp5/pa_ocanren.cma))
)


