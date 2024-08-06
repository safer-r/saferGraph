test_that("inter_ticks function argument tests", {
    
    # Test for 'lim' argument
    expect_error(inter_ticks(lim = NULL))
    expect_error(inter_ticks(lim = c(10, 10)))
    expect_error(inter_ticks(lim = c(Inf, 10)))
    
    # Test for 'log' argument
    expect_no_error(inter_ticks(lim = c(2, 3), log = "log2"))
    expect_error(inter_ticks(lim = c(2, 3), log = "invalid_log"))
    
    # Test for 'breaks' argument
    expect_no_error(inter_ticks(lim = c(0, 26.5), breaks = c(0, 10, 20)))
    # expect_error(inter_ticks(lim = c(0, 26.5), breaks = NULL))
    
    # Test for 'n' argument
    expect_no_error(inter_ticks(lim = c(0, 26.5), n = 3))
    expect_error(inter_ticks(lim = c(0, 26.5), n = 0))
    
    # Test for 'warn.print' argument
    expect_no_error(inter_ticks(lim = c(0, 26.5), warn.print = TRUE))
    expect_no_error(inter_ticks(lim = c(0, 26.5), warn.print = FALSE))
    expect_error(inter_ticks(lim = c(0, 26.5), warn.print = 123))
    
    
    # Test case for all arguments combined
    
    # Test with default arguments
    expect_no_error(inter_ticks(lim = c(2, 3)))
    
    # Test with specified arguments
    expect_no_error(inter_ticks(lim = c(0, 26.5), log = "no", breaks = c(0, 10, 20), n = 3, warn.print = TRUE, safer_check = TRUE))
    expect_error(inter_ticks(lim = c(0, 26.5), log = "no", breaks = c(0, 10, 20), n = 3, warn.print = NULL, safer_check = TRUE))
    expect_error(inter_ticks(lim = c(0, 26.5), log = "no", breaks = c(0, 10, 20), n = 3, warn.print = TRUE, safer_check = NULL))
    
    # Test with invalid arguments
    expect_error(inter_ticks(lim = NULL, log = "no"))
    expect_error(inter_ticks(lim = c(2, 3), log = "invalid_log"))
    expect_error(inter_ticks(lim = c(2, 3), n = 0))
    expect_no_error(inter_ticks(lim = c(2, 3), breaks = NULL))
})
