testthat::test_that("close2()", {

    ## data argument values
    str1 <- "pdf"
    str2 <- "windows"
    str3 <- "NOTGOOD"
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor(str1)
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(close2(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(close2(kind = str1, return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
    testthat::expect_error(close2(kind = "", return_text = TRUE, safer_check = FALSE, lib_path = NULL, error_text = "")) # already rejected above by the options check of arg_check(): the "cannot contain empty string" block of the "" management section is unreachable for kind
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_no_error(saferGraph:::close2()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()") # runs with all defaults (closes pdf devices if any)
    testthat::expect_no_error(close2()) # the function has all-default args, so this covers the arg_user_setting_eval <- NULL branch and returns NULL (no device to close)
    #### end function name

    ########  arg with no default values
    ######## end arg with no default values

    ######## management of NULL arguments
    testthat::expect_error(close2(kind = NULL, return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = TRUE, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(close2(kind = str1, return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    testthat::expect_error(close2(kind = character(), return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = integer(), return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = double(), return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = logical(), return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = complex(), return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = data.frame(), return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = list(), return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(close2(kind = str1, return_text = character(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = integer(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = double(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = logical(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = complex(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = data.frame(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = list(), safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(close2(kind = str1, return_text = TRUE, safer_check = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = TRUE, safer_check = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = TRUE, safer_check = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = TRUE, safer_check = logical(), lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = TRUE, safer_check = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = TRUE, safer_check = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = TRUE, safer_check = list(), lib_path = NULL, error_text = ""))

    testthat::expect_error(close2(kind = str1, return_text = TRUE, safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(close2(kind = str1, return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    testthat::expect_error(close2(kind = NA, return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = TRUE, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = TRUE, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(close2(kind = str1, return_text = TRUE, safer_check = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = TRUE, safer_check = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = TRUE, safer_check = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = TRUE, safer_check = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = TRUE, safer_check = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = TRUE, safer_check = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(close2(kind = str1, return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(close2(kind = str1, return_text = TRUE, safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    testthat::expect_no_error(close2(kind = str1, return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = TRUE, safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = TRUE, safer_check = TRUE, lib_path = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = TRUE, safer_check = TRUE, lib_path = mat1, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = TRUE, safer_check = TRUE, lib_path = factor1, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = TRUE, safer_check = TRUE, lib_path = expr1, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = TRUE, safer_check = TRUE, lib_path = fun1, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = TRUE, safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    # lib_path is checked even when safer_check is FALSE (backbone v21.2: lib_path used outside if(safer_check == TRUE){}, so the lib_path check is always executed)
    testthat::expect_error(close2(kind = str1, return_text = TRUE, safer_check = FALSE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(close2(kind = str1, return_text = TRUE, safer_check = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(close2(kind = str1, return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(close2(kind = str1, return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # kind
    testthat::expect_no_error(close2(kind = "windows", return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = NULL, return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = NA, return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = 1, return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = c(TRUE, FALSE), return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = mat1, return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = factor1, return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = expr1, return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = fun1, return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = c(str1, str1), return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # length must be 1
    testthat::expect_error(close2(kind = str3, return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # not in the allowed options
    # end kind
    # return_text
    testthat::expect_no_error(close2(kind = str1, return_text = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = "TRUE", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = c(TRUE, FALSE), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = mat1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = factor1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = expr1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(close2(kind = str1, return_text = fun1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end return_text
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(close2(kind = "", return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(close2(kind = str1, return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
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
    ######## end other checkings
    #### end second round of checking and data preparation

    #### main code
    # open devices then close them with close2()
    grDevices::pdf(file = base::file.path(base::tempdir(), "close2_test.pdf"), width = 7, height = 7)
    grDevices::pdf(file = base::file.path(base::tempdir(), "close2_test2.pdf"), width = 7, height = 7)
    result <- close2(kind = "pdf", return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::grepl(x = result, pattern = "THE REQUIRED KIND OF GRAPHIC DEVICES TO CLOSE ARE pdf", fixed = TRUE))
    testthat::expect_true(object = base::grepl(x = result, pattern = " DEVICE NUMBER ", fixed = TRUE))
    testthat::expect_true(object = base::grepl(x = result, pattern = " HAS BEEN CLOSED", fixed = TRUE))
    testthat::expect_null(object = grDevices::dev.list())
    # no device open: no closed device in the text
    result2 <- close2(kind = "pdf", return_text = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_false(object = base::grepl(x = result2, pattern = " HAS BEEN CLOSED", fixed = TRUE))
    # return_text = FALSE does not return the text
    grDevices::pdf(file = base::file.path(base::tempdir(), "close2_test3.pdf"), width = 7, height = 7)
    result3 <- close2(kind = "pdf", return_text = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_null(object = result3)
    testthat::expect_null(object = grDevices::dev.list())
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
