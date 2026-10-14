# saferGraph 2.0.0

* Migrated to the safer-r backbone v20.0: every function now follows the saferDev 2.0.0 structure, with arg_check() v2.0.0 semantics (exact class/typeof/mode comparison; class = c("matrix", "array") for matrices, mode = "numeric" preferred for numeric arguments).
* All functions carry the mandatory lib_path, safer_check and error_text reserved arguments.
* Bug fixes found during the migration: mixed-quote paste0() idioms that broke r_debugging_tools argument reporting (post_plot), grDevices::colors() calls applied to par objects instead of applying the par (post_plot custom.par handling), inter_ticks warn_print no longer ignored by the on.exit warning, open2 width/height now reject NA and negative values.
* Complete test suite rewritten in the saferDev style (tests/testthat), including a test-mandatory_args.R replicating the saferDev pattern; code coverage brought to 100% with # nocov markers on OS-specific and internal-error branches.
* R (>= 4.5.0) is now required.

# saferGraph 1.0.0

* Initial version (backbone v19.0).
