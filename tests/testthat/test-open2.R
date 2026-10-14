testthat::test_that("open2()", {

    ## data argument values
    str1 <- "working.dir"
    str2 <- "graph"
    num1 <- 7
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor("a")
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(open2(caca = 1)) # to test for the absence of ...
    temp_dir <- base::tempdir()
    testthat::expect_no_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
    result <- grDevices::dev.list()
    if( ! base::is.null(x = result)){
        if(base::length(x = grDevices::dev.list()) > 0){base::invisible(grDevices::dev.off(which = grDevices::dev.cur()))}
    }
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    # the "::()" dev branch is triggered by any call via saferGraph:::open2(), but such a call opens a "graph.pdf" device in the current working directory: run the two no-args calls from tempdir() and clean after
    ini_wd <- base::getwd()
    base::setwd(dir = temp_dir)
    testthat::expect_error(saferGraph:::open2()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()") # runs with all defaults
    result0c <- open2(pdf_overwrite = TRUE) # all-default call: covers the arg_user_setting_eval <- NULL branch
    base::setwd(dir = ini_wd)
    testthat::expect_true(object = base::is.list(x = result0c))
    if(base::length(x = grDevices::dev.list()) > 0){
        base::invisible(grDevices::dev.off())
    }
    if(base::file.exists(base::file.path(temp_dir, "graph.pdf"))){
        base::file.remove(base::file.path(temp_dir, "graph.pdf"))
    }
    #### end function name

    ########  arg with no default values
    ######## end arg with no default values

    ######## management of NULL arguments
    testthat::expect_error(open2(pdf = NULL, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = NULL, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = NULL, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = NULL, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = NULL, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = NULL, pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = NULL, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = NULL, remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = NULL, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = NULL))
    if(base::length(x = grDevices::dev.list()) > 0){base::invisible(grDevices::dev.off(which = grDevices::dev.cur()))}
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    testthat::expect_error(open2(pdf = integer(), pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = double(), pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = logical(), pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = complex(), pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = data.frame(), pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = list(), pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(open2(pdf = TRUE, pdf_path = character(), pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = integer(), pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = double(), pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = logical(), pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = complex(), pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = data.frame(), pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = list(), pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = character(), width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = integer(), width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = double(), width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = logical(), width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = complex(), width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = data.frame(), width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = list(), width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = integer(), height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = double(), height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = logical(), height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = complex(), height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = data.frame(), height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = list(), height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = integer(), paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = double(), paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = logical(), paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = complex(), paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = data.frame(), paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = list(), paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = integer(), pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = double(), pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = logical(), pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = complex(), pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = data.frame(), pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = list(), pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = integer(), rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = double(), rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = logical(), rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = complex(), rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = data.frame(), rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = list(), rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = integer(), remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = double(), remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = logical(), remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = complex(), remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = data.frame(), remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = list(), remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = integer(), return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = double(), return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = logical(), return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = complex(), return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = data.frame(), return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = list(), return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = integer(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = double(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = logical(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = complex(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = data.frame(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = list(), safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = logical(), lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = list(), lib_path = NULL, error_text = ""))

    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    if(base::length(x = grDevices::dev.list()) > 0){base::invisible(grDevices::dev.off(which = grDevices::dev.cur()))}
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    testthat::expect_error(open2(pdf = NA, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = NA, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = NA, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = NA, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = NA, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = NA, pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = NA, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = NA, remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = NA, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = NA))
    if(base::length(x = grDevices::dev.list()) > 0){base::invisible(grDevices::dev.off(which = grDevices::dev.cur()))}
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    if(base::length(x = grDevices::dev.list()) > 0){base::invisible(grDevices::dev.off(which = grDevices::dev.cur()))}
    testthat::expect_no_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = FALSE, lib_path = NULL, error_text = ""))
    if(base::length(x = grDevices::dev.list()) > 0){base::invisible(grDevices::dev.off(which = grDevices::dev.cur()))}
    ######## end safer_check argument checking

    ######## check of lib_path
    testthat::expect_no_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = mat1, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = factor1, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = expr1, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = fun1, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), error_text = ""))
    if(base::length(x = grDevices::dev.list()) > 0){base::invisible(grDevices::dev.off(which = grDevices::dev.cur()))}
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    if(base::length(x = grDevices::dev.list()) > 0){base::invisible(grDevices::dev.off(which = grDevices::dev.cur()))}
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    if(base::length(x = grDevices::dev.list()) > 0){base::invisible(grDevices::dev.off(which = grDevices::dev.cur()))}
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # pdf
    testthat::expect_error(open2(pdf = NULL, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = NA, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = 1, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = "TRUE", pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = c(TRUE, FALSE), pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = mat1, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = factor1, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = expr1, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = fun1, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end pdf
    # pdf_path
    ini_wd2 <- base::getwd() # pdf_path = "working.dir" writes graph.pdf in the working dir: run it from tempdir() and clean after
    base::setwd(dir = temp_dir)
    testthat::expect_no_error(open2(pdf = TRUE, pdf_path = str1, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # "working.dir" special value
    base::setwd(dir = ini_wd2)
    if(base::length(x = grDevices::dev.list()) > 0){base::invisible(grDevices::dev.off(which = grDevices::dev.cur()))}
    if(base::file.exists(base::file.path(temp_dir, "graph.pdf"))){
        base::suppressWarnings(expr = base::file.remove(base::file.path(temp_dir, "graph.pdf")), classes = "warning") # the pdf device can still hold the file on Windows: deletion will occur at the end of the R session if it fails
    }
    testthat::expect_error(open2(pdf = TRUE, pdf_path = NULL, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = NA, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = 1, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = c(temp_dir, temp_dir), pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # length must be 1
    testthat::expect_error(open2(pdf = TRUE, pdf_path = mat1, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = factor1, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = expr1, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = fun1, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end pdf_path
    # pdf_name
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = NULL, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = NA, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = 1, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = c(str2, str2), width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # length must be 1
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = mat1, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = factor1, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = expr1, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = fun1, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end pdf_name
    # width
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = NULL, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = NA, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = "a", height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = TRUE, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = c(7, 7), height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # length must be 1
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = mat1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = factor1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = expr1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = fun1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = -1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # negative not allowed
    # end width
    # height: same as width (same arg_check() settings)
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = NULL, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = NA, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = "a", paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = c(7, 7), paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = mat1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = factor1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = expr1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = fun1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = -1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end height
    # paper
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = NULL, pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = NA, pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = 1, pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = c("a4", "letter"), pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # length must be 1
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "NOTGOOD", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # not in the allowed options
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = mat1, pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = factor1, pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = expr1, pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = fun1, pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end paper
    # pdf_overwrite
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = NULL, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = NA, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = 1, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = "TRUE", rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = c(TRUE, FALSE), rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = mat1, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = factor1, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = expr1, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = fun1, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end pdf_overwrite
    # rescale
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = NULL, remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = NA, remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = 1, remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = c("a4", "letter"), remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # length must be 1
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "NOTGOOD", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # not in the allowed options
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = mat1, remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = factor1, remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = expr1, remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = fun1, remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end rescale
    # remove_read_only
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = NULL, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = NA, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = 1, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = "TRUE", return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = c(TRUE, FALSE), return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = mat1, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = factor1, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = expr1, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = fun1, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end remove_read_only
    # return_output
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = "TRUE", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = c(TRUE, FALSE), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = mat1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = factor1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = expr1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = fun1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end return_output
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(open2(pdf = TRUE, pdf_path = "", pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = "", width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # error_text can be ""
    if(base::length(x = grDevices::dev.list()) > 0){base::invisible(grDevices::dev.off(which = grDevices::dev.cur()))}
    ######## end management of "" in arguments of mode character

    #### end argument secondary checking

    #### second round of checking and data preparation

    ######## code that protects set.seed() in the global environment
    ######## end code that protects set.seed() in the global environment

    ######## warning initiation
    ######## end warning initiation

    ######## graphic device checking
    ######## end graphic device checking

    ######## other checkings
    testthat::expect_error(open2(pdf = TRUE, pdf_path = "PATH_NOT_GOOD", pdf_name = str2, width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # non existing directory
    ######## end other checkings
    #### end second round of checking and data preparation

    #### main code
    # pdf device
    pdf_file <- base::file.path(temp_dir, "open2_test.pdf")
    if(base::file.exists(pdf_file)){
        base::file.remove(pdf_file)
    }
    # pdf_path with trailing "/" and "\\" (cleaned in main code)
    result0 <- open2(pdf = TRUE, pdf_path = base::paste0(temp_dir, "/"), pdf_name = "open2_test", width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result0$pdf.loc, expected = pdf_file)
    base::invisible(grDevices::dev.off())
    result0b <- open2(pdf = TRUE, pdf_path = base::paste0(temp_dir, "\\"), pdf_name = "open2_test", width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result0b$pdf.loc, expected = pdf_file)
    base::invisible(grDevices::dev.off())
    result <- open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = "open2_test", width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.list(x = result))
    testthat::expect_equal(object = base::names(x = result), expected = base::c("pdf.loc", "ini.par", "zone.ini", "dim"))
    testthat::expect_equal(object = result$pdf.loc, expected = pdf_file)
    testthat::expect_true(object = base::is.list(x = result$ini.par)) # par() with no.readonly = TRUE returns a list
    testthat::expect_equal(object = result$dim, expected = base::unname(grDevices::dev.size(unit = "in"))[1:2])
    testthat::expect_true(object = base::file.exists(pdf_file)) # the pdf file is created at return? No: the file is created when the device is closed. Thus here, it does not exist yet. But pdf.loc is the path where it will be created
    # actually, the pdf file is only written on dev.off()
    base::invisible(grDevices::dev.off())
    testthat::expect_true(object = base::file.exists(pdf_file))
    # pdf_name with .pdf extension
    pdf_file2 <- base::file.path(temp_dir, "open2_test2.pdf")
    if(base::file.exists(pdf_file2)){
        base::file.remove(pdf_file2)
    }
    result2 <- open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = "open2_test2.pdf", width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result2$pdf.loc, expected = pdf_file2)
    base::invisible(grDevices::dev.off())
    # return_output = FALSE does not return anything
    pdf_file3 <- base::file.path(temp_dir, "open2_test3.pdf")
    if(base::file.exists(pdf_file3)){
        base::file.remove(pdf_file3)
    }
    result3 <- open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = "open2_test3", width = num1, height = num1, paper = "special", pdf_overwrite = TRUE, rescale = "fixed", remove_read_only = TRUE, return_output = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_null(object = result3)
    base::invisible(grDevices::dev.off())
    # existing pdf file and pdf_overwrite = FALSE -> error
    testthat::expect_error(open2(pdf = TRUE, pdf_path = temp_dir, pdf_name = "open2_test", width = num1, height = num1, paper = "special", pdf_overwrite = FALSE, rescale = "fixed", remove_read_only = TRUE, return_output = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    base::invisible(grDevices::dev.off(which = grDevices::dev.cur())) # close the device opened before the error
    # no device left open
    testthat::expect_null(object = grDevices::dev.list())
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
