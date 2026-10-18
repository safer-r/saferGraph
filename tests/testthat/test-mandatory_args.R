# This file tests the "mandatory arg of safer-r functions" checking block present in every saferGraph function
# This block fires only if the function is written without one of the mandatory formals (lib_path, safer_check, error_text)
# It cannot be triggered by user values: the test uses a temporary function built from the same body but with the mandatory formal removed from formals()
# No # nocov markers are used, and no R source files are modified: the block is actually executed by the tests

testthat::test_that("mandatory arg block of saferGraph functions (public functions)", {

    ## data argument values
    # full calls with all formals except the mandatory one to drop (lib_path for all these functions)
    full_calls <- base::list(
        close2 = base::quote(close2(kind = "pdf", return_text = TRUE, safer_check = FALSE, error_text = "")),
        inter_ticks = base::quote(inter_ticks(lim = c(0, 1), log = "no", breaks = NULL, n = NULL, warn_print = TRUE, safer_check = FALSE, error_text = "")),
        open2 = base::quote(open2(pdf = TRUE, pdf_path = tempdir(), pdf_name = "mandatory_args_test", width = 7, height = 7, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = FALSE, safer_check = FALSE, error_text = "")),
        post_plot = base::quote(post_plot(x_side = 0, x_log_scale = FALSE, x_categ = NULL, x_categ_pos = NULL, x_lab = "", x_axis_size = 1.5, x_label_size = 1.5, x_dist_legend = 0.5, x_nb_inter_tick = 1L, y_side = 0, y_log_scale = FALSE, y_categ = NULL, y_categ_pos = NULL, y_lab = "", y_axis_size = 1.5, y_label_size = 1.5, y_dist_legend = 0.5, y_nb_inter_tick = 1L, text_angle = 90, tick_length = 0.5, sec_tick_length = 0.3, bg_color = NULL, grid_lwd = NULL, grid_col = "white", corner_text = "", corner_text_size = 1, just_label_add = FALSE, par_reset = FALSE, custom_par = NULL, safer_check = FALSE, error_text = "")),
        scale2 = base::quote(scale2(n = 5, lim = c(2, 3.101), kind = "approx", safer_check = FALSE, error_text = ""))
    )
    ## end data argument values

    ## initialization of tests
    pdf_ini <- grDevices::pdf(file = base::file.path(base::tempdir(), "mandatory_args_ini.pdf"), width = 7, height = 7, paper = "special", title = "mandatory_args_ini") # post_plot() requires an open graphic device
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    ######## mandatory arg of safer-r functions
    for(fn in base::names(x = full_calls)){
        f <- base::get(x = fn, envir = base::asNamespace("saferGraph"))
        fml <- base::formals(fun = f)
        fml_dropped <- fml[base::setdiff(x = base::names(x = fml), y = "lib_path")] # drop lib_path: the block must fire
        tempo_env <- base::new.env(parent = base::environment(fun = f))
        tempo_env[[fn]] <- base::as.function(base::c(fml_dropped, base::list(base::body(f))), envir = tempo_env)
        cl <- full_calls[[fn]]
        res <- base::tryCatch(
            expr = base::eval(expr = cl, envir = tempo_env),
            error = function(e){base::conditionMessage(e)}
        )
        testthat::expect_true(object = base::grepl(x = res, pattern = "FOLLOWING ARGUMENT IS MANDATORY IN SAFER-R FUNCTIONS:", fixed = TRUE), label = base::paste0(fn, ": mandatory-args block message"))
        testthat::expect_true(object = base::grepl(x = res, pattern = "lib_path", fixed = TRUE), label = base::paste0(fn, ": lib_path reported as missing mandatory arg"))
        testthat::expect_false(object = base::grepl(x = res, pattern = "object '", fixed = TRUE), label = base::paste0(fn, ": no error before the mandatory-args block"))
    }
    ######## end mandatory arg of safer-r functions

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    # restore check: with all formals present, the block must NOT fire
    testthat::expect_no_error(object = saferGraph::close2(kind = "pdf", return_text = TRUE, safer_check = FALSE, lib_path = NULL, error_text = "")) # close2() also closes the pdf device opened above
    if(base::length(x = grDevices::dev.list()) > 0){
        grDevices::dev.off(which = grDevices::dev.cur())
    }
    ## end other tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
