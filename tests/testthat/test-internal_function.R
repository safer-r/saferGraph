test_that(".pack_and_function_check function argument tests", {
    
    # Test for 'fun' argument
    expect_error(.pack_and_function_check(fun = NULL, lib.path = NULL, external.function.name = "fun1"))
    expect_error(.pack_and_function_check(fun = "some_function", lib.path = NULL, external.function.name = "fun1"))
    
    # Test for 'lib.path' argument (skipped because already checked in the main function)
    
    # Test for 'external.function.name' argument (no specific test)
    
    # Test case for all arguments combined
    
    # Test with default arguments
    expect_error(.pack_and_function_check(fun = NULL, lib.path = NULL, external.function.name = "fun1"))
    
    # Test with specified arguments
    expect_error(.pack_and_function_check(fun = "ggplot2::geom_point", lib.path = "C:/Program Files/R/R-4.3.1/library", external.function.name = "fun1"))
    
})
