testthat::test_that("inter_ticks()", {

    ## data argument values
    num1 <- 1.5
    int1 <- 3L
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor("a")
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(inter_ticks(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferGraph:::inter_ticks()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()") # errors on the missing lim default, but the function_name block is executed first
    #### end function name

    ########  arg with no default values
    testthat::expect_error(inter_ticks())
    lim <- c(1, 10)
    testthat::expect_error(inter_ticks())
    base::rm(lim)
    ########  end arg with no default values

    ######## management of NULL arguments
    testthat::expect_error(inter_ticks(lim = NULL, log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = NULL, breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = NULL, n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # breaks cannot be NULL when log == "no" (see below)
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = NULL, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    testthat::expect_error(inter_ticks(lim = character(), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = integer(), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = double(), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = logical(), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = complex(), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = data.frame(), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = list(), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = character(), breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = integer(), breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = double(), breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = logical(), breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = complex(), breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = data.frame(), breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = list(), breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = character(), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = integer(), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = double(), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = logical(), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = complex(), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = data.frame(), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = list(), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = integer(), warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = double(), warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = logical(), warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = complex(), warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = data.frame(), warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = list(), warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = character(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = integer(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = double(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = logical(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = complex(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = data.frame(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = list(), safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = logical(), lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = list(), lib_path = NULL, error_text = ""))

    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    testthat::expect_error(inter_ticks(lim = c(NA, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = NA, breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = NA, n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = NA, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    testthat::expect_no_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = mat1, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = factor1, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = expr1, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = fun1, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    # lib_path is checked even when safer_check is FALSE (backbone v21.2: lib_path used outside if(safer_check == TRUE){}, so the lib_path check is always executed)
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = FALSE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # lim
    testthat::expect_error(inter_ticks(lim = NULL, log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = NA, log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = "a", log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(TRUE, FALSE), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = mat1, log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = factor1, log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = expr1, log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = fun1, log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = num1, log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # length must be 2
    testthat::expect_error(inter_ticks(lim = c(1, 10, 20), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # length must be 2
    testthat::expect_error(inter_ticks(lim = c(1, Inf), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # Inf not allowed (inf_values = FALSE)
    # end lim
    # log
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = NULL, breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = NA, breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = 1, breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = c("no", "log10"), breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # length must be 1
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "NOTGOOD", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # not in the allowed options
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = mat1, breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = factor1, breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = expr1, breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = fun1, breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end log
    # breaks
    testthat::expect_no_error(inter_ticks(lim = c(-4, 4), log = "log2", breaks = NULL, n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # breaks can be NULL with log != "no"
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "log2", breaks = NA, n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "log2", breaks = "a", n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "log2", breaks = c(TRUE, FALSE), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(inter_ticks(lim = c(-4, 4), log = "log2", breaks = mat1, n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # matrix has mode "numeric", allowed by arg_check()
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "log2", breaks = factor1, n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "log2", breaks = expr1, n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "log2", breaks = fun1, n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "log2", breaks = 1, n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # Inf not allowed (inf_values = FALSE)
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "log2", breaks = c(1, Inf), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # Inf not allowed (inf_values = FALSE)
    # end breaks
    # n
    testthat::expect_no_error(inter_ticks(lim = c(-4, 4), log = "log2", breaks = NULL, n = NULL, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # n can be NULL with log != "no"
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "log2", breaks = NULL, n = NA, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "log2", breaks = NULL, n = 1.5, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # not integer
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "log2", breaks = NULL, n = "a", warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "log2", breaks = NULL, n = c(TRUE, FALSE), warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "log2", breaks = NULL, n = mat1, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "log2", breaks = NULL, n = factor1, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "log2", breaks = NULL, n = expr1, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "log2", breaks = NULL, n = fun1, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "log2", breaks = NULL, n = -1, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # negative not allowed (neg_values = FALSE)
    # end n
    # warn_print
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = "TRUE", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = c(TRUE, FALSE), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = mat1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = factor1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = expr1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = fun1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end warn_print
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
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
    testthat::expect_error(inter_ticks(lim = c(1, 1), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # null range
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = NULL, n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # breaks cannot be NULL when log == "no"
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 0, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # n <= 0 (arg_check neg_values = FALSE should catch it before, but keep the check)
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = 1, n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # breaks of length < 2
    testthat::expect_error(inter_ticks(lim = c(-4, 4), log = "no", breaks = c(0, 1, 5), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # breaks not equidistant
    ######## end other checkings
    #### end second round of checking and data preparation

    #### main code
    # no log scale
    result <- inter_ticks(lim = c(-4, 4), log = "no", breaks = c(-2, 0, 2), n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result$log, expected = "no")
    testthat::expect_equal(object = result$coordinates, expected = base::c(-4, -3.5, -3, -2.5, -2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4))
    testthat::expect_equal(object = result$values, expected = base::c(-4, -3.5, -3, -2.5, -2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4))
    testthat::expect_null(object = result$warn)
    # no log scale, reverse lim, breaks already in the same order
    result2 <- inter_ticks(lim = c(10, 0), log = "no", breaks = c(10, 8, 6, 4, 2, 0), n = 4, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_null(object = result2$warn)
    # no log scale, reverse lim, breaks not in the same order -> warning + reorder
    result3 <- inter_ticks(lim = c(10, 0), log = "no", breaks = c(0, 2, 4, 6, 8, 10), n = 4, warn_print = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::grepl(x = result3$warn, pattern = "VALUES IN breaks ARGUMENT NOT IN THE SAME ORDER AS IN lim ARGUMENT", fixed = TRUE))
    testthat::expect_equal(object = result3$coordinates, expected = result2$coordinates) # same results after reordering
    # log2
    result4 <- inter_ticks(lim = c(-4, 4), log = "log2", breaks = NULL, n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result4$log, expected = "log2")
    testthat::expect_true(object = base::all(result4$values >= base::min(2^(c(-4, 4))) & result4$values <= base::max(2^(c(-4, 4)))))
    testthat::expect_true(object = base::all(base::abs(result4$coordinates - base::log2(result4$values)) < 1e-06))
    testthat::expect_null(object = result4$warn)
    # log10
    result5 <- inter_ticks(lim = c(-4, 4), log = "log10", breaks = NULL, n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result5$log, expected = "log10")
    testthat::expect_true(object = base::all(result5$values >= base::min(10^(c(-4, 4))) & result5$values <= base::max(10^(c(-4, 4)))))
    testthat::expect_true(object = base::all(base::abs(result5$coordinates - base::log10(result5$values)) < 1e-06))
    testthat::expect_null(object = result5$warn)
    # no inter ticks computed -> warning (and the on.exit() warning is emitted when warn_print = TRUE)
    result6 <- inter_ticks(lim = c(0.505, 0.51), log = "log10", breaks = NULL, n = 3, warn_print = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::grepl(x = result6$warn, pattern = "NO INTER TICKS COMPUTED BETWEEN THE LIMITS INDICATED", fixed = TRUE))
    testthat::expect_equal(object = base::length(x = result6$coordinates), expected = 0L)
    testthat::expect_warning(inter_ticks(lim = c(0.505, 0.51), log = "log10", breaks = NULL, n = 3, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "NO INTER TICKS COMPUTED BETWEEN THE LIMITS INDICATED", fixed = TRUE)
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
