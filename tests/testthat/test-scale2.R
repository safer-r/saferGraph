test_that("scale2 - n argument", {
    # Test with positive integer
    expect_no_error(scale2(n = 10, lim = c(0, 100)))
    
    # Test with non-integer
    # expect_error(scale2(n = 10.5, lim = c(0, 100)))
    
    # Test with zero
    expect_error(scale2(n = 0, lim = c(0, 100)))
    
    
    # scale2 - lim argument"
    # Test with numeric vector of length 2
    expect_no_error(scale2(n = 10, lim = c(0, 100)))
    
    # Test with non-numeric lim
    expect_error(scale2(n = 10, lim = c("a", "b")))
    
    # Test with identical lim values
    expect_error(scale2(n = 10, lim = c(10, 10)))
    
    # Test with Inf values in lim
    expect_error(scale2(n = 10, lim = c(0, Inf)))
    
    # scale2 - kind argument
    # Test with valid kind values
    expect_no_error(scale2(n = 10, lim = c(0, 100), kind = "approx"))
    expect_no_error(scale2(n = 10, lim = c(0, 100), kind = "strict"))
    expect_no_error(scale2(n = 10, lim = c(0, 100), kind = "strict.cl"))
    
    # Test with invalid kind value
    expect_error(scale2(n = 10, lim = c(0, 100), kind = "invalid"))
    
    
    # Tests for combinations of arguments
    
    # scale2 - all arguments
    # Test with valid arguments
    expect_no_error(scale2(n = 10, lim = c(0, 100), kind = "approx", safer_check = TRUE))
    
    # Test with invalid lim
    expect_error(scale2(n = 10, lim = c("a", "b"), kind = "approx", safer_check = TRUE))
})