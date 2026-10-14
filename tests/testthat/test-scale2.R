testthat::test_that("scale2()", {

    ## data argument values
    num1 <- 5
    num2 <- c(2, 3.101)
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor("a")
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(scale2(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferGraph:::scale2()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()") # errors on the missing n and lim defaults, but the function_name block is executed first
    #### end function name

    ########  arg with no default values
    testthat::expect_error(scale2())
    n <- num1
    testthat::expect_error(scale2())
    base::rm(n)
    lim <- num2
    testthat::expect_error(scale2())
    base::rm(lim)
    ########  end arg with no default values

    ######## management of NULL arguments
    testthat::expect_error(scale2(n = NULL, lim = num2, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = NULL, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    testthat::expect_error(scale2(n = integer(), lim = num2, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = double(), lim = num2, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = logical(), lim = num2, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = complex(), lim = num2, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = data.frame(), lim = num2, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = list(), lim = num2, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(scale2(n = num1, lim = character(), kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = integer(), kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = double(), kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = logical(), kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = complex(), kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = data.frame(), kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = list(), kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(scale2(n = num1, lim = num2, kind = character(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = integer(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = double(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = logical(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = complex(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = data.frame(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = list(), safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = logical(), lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = list(), lib_path = NULL, error_text = ""))

    testthat::expect_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    testthat::expect_error(scale2(n = NA, lim = num2, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = NA, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    testthat::expect_no_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = TRUE, lib_path = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = TRUE, lib_path = mat1, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = TRUE, lib_path = factor1, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = TRUE, lib_path = expr1, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = TRUE, lib_path = fun1, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # n
    testthat::expect_error(scale2(n = NULL, lim = num2, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = NA, lim = num2, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = 1.5, lim = num2, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = "")) # not integer
    testthat::expect_error(scale2(n = "a", lim = num2, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = c(TRUE, FALSE), lim = num2, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = mat1, lim = num2, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = factor1, lim = num2, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = expr1, lim = num2, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = fun1, lim = num2, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = -1, lim = num2, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = "")) # negative not allowed
    testthat::expect_error(scale2(n = 0, lim = num2, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = "")) # null not allowed
    # end n
    # lim
    testthat::expect_error(scale2(n = num1, lim = NULL, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = NA, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = "a", kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = c(TRUE, FALSE), kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = mat1, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = factor1, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = expr1, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = fun1, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num1, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = "")) # length must be 2
    testthat::expect_error(scale2(n = num1, lim = c(1, 2, 3), kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = "")) # length must be 2
    testthat::expect_error(scale2(n = num1, lim = c(1, Inf), kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = "")) # Inf not allowed
    # end lim
    # kind
    testthat::expect_error(scale2(n = num1, lim = num2, kind = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = c("approx", "strict"), safer_check = TRUE, lib_path = NULL, error_text = "")) # length must be 1
    testthat::expect_error(scale2(n = num1, lim = num2, kind = "NOTGOOD", safer_check = TRUE, lib_path = NULL, error_text = "")) # not in the allowed options
    testthat::expect_error(scale2(n = num1, lim = num2, kind = mat1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = factor1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = expr1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(scale2(n = num1, lim = num2, kind = fun1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end kind
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(scale2(n = num1, lim = num2, kind = "", safer_check = TRUE, lib_path = NULL, error_text = ""))
    ######## end management of "" in arguments of mode character

    #### end argument secondary checking

    #### second round of checking and data preparation

    ######## code that protects set.seed() in the global environment
    ######## end code that protects set.seed() in the global environment

    ######## warning initiation
    ######## end warning initiation

    ######## graphic device checking
    testthat::expect_no_error(scale2(n = num1, lim = num2, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = "")) # no graphic device open: on.exit() restoration skipped
    ######## end graphic device checking

    ######## other checkings
    testthat::expect_error(scale2(n = 0, lim = num2, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = "")) # n == 0 (float-dealing all.equal check)
    testthat::expect_error(scale2(n = num1, lim = c(2, 2), kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = "")) # null range
    ######## end other checkings
    #### end second round of checking and data preparation

    #### main code
    # approx
    result <- scale2(n = num1, lim = num2, kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.numeric(x = result))
    testthat::expect_true(object = base::length(x = result) >= 2)
    testthat::expect_true(object = base::min(result) >= base::min(num2) & base::max(result) <= base::max(num2))
    # strict
    result2 <- scale2(n = num1, lim = num2, kind = "strict", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result2, expected = saferTool::round2(data = base::seq(2, 3.101, length.out = num1), dec_nb = 2, after_lead_zero = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""))
    # strict.cl, odd n
    result3 <- scale2(n = 5, lim = num2, kind = "strict.cl", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.numeric(x = result3))
    testthat::expect_equal(object = base::length(x = result3), expected = 5L)
    testthat::expect_true(object = base::min(result3) >= base::min(num2) & base::max(result3) <= base::max(num2))
    testthat::expect_true(object = base::all(base::diff(result3) > 0))
    # strict.cl, even n
    result4 <- scale2(n = 6, lim = num2, kind = "strict.cl", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = base::length(x = result4), expected = 6L)
    testthat::expect_true(object = base::all(base::diff(result4) > 0))
    # strict.cl with n == 1
    result5 <- scale2(n = 1, lim = num2, kind = "strict.cl", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = base::length(x = result5), expected = 1L)
    # strict.cl with n == 2
    result6 <- scale2(n = 2, lim = num2, kind = "strict.cl", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = base::length(x = result6), expected = 2L)
    # inverted axis
    result7 <- scale2(n = num1, lim = base::rev(num2), kind = "approx", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result7, expected = base::rev(result))
    # wide range (powers of 10 branch)
    result8 <- scale2(n = 10, lim = c(1e-04, 1e+06), kind = "strict.cl", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.numeric(x = result8))
    testthat::expect_true(object = base::min(result8) >= 1e-04 & base::max(result8) <= 1e+06)
    testthat::expect_true(object = base::all(base::diff(result8) > 0))
    # last check branch: min < tempo.min triggers the shift branch (min(output) < tempo.min)
    result9 <- scale2(n = 3, lim = c(0.99, 1.5), kind = "strict.cl", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.numeric(x = result9))
    testthat::expect_true(object = base::min(result9) >= 0.99 & base::max(result9) <= 1.5)
    # tiny interval: not reachable with valid n because arg_check() rejects huge non-representable n first
    # (the tempo.inter == 0 branch is unreachable with valid inputs; marked as such for coverage)
    testthat::expect_error(scale2(n = 2.5, lim = c(0.1, 0.2), kind = "strict.cl", safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "MUST BE", fixed = TRUE) # non-integer n
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    # n = Inf passes arg_check (inf_values = TRUE) and makes tempo.inter = tempo.range / (n + 1) == 0 (main code error at the beginning of the strict.cl branch)
    testthat::expect_error(scale2(n = Inf, lim = c(0.1, 0.2), kind = "strict.cl", safer_check = TRUE, lib_path = NULL, error_text = ""))
    # graphic device open when scale2() is called: the graphic parameters are restored on exit (main code)
    pdf_ini2 <- grDevices::pdf(file = base::file.path(base::tempdir(), "scale2_ini2.pdf"), width = 7, height = 7, paper = "special", title = "scale2_ini2")
    graphics::plot(x = 1:3, y = 1:3)
    par_before <- graphics::par(c("usr", "mfrow"))
    result12 <- scale2(n = num1, lim = num2, kind = "strict.cl", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.numeric(x = result12))
    par_after <- graphics::par(c("usr", "mfrow"))
    testthat::expect_equal(object = par_before, expected = par_after) # par restored on exit
    grDevices::dev.off(which = grDevices::dev.cur())
    ## end other tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
