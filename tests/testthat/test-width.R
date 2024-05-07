test_that("the function width works correctly", {
    # Test with positive integer
    expect_no_error(width(categ.nb = 10, inch.left.space = 1, inch.right.space = 1))
    
    # Test with zero
    expect_no_error(width(categ.nb = 0, inch.left.space = 1, inch.right.space = 1))
    
    # Test with non-integer
    expect_error(width(categ.nb = 10.5, inch.left.space = 1, inch.right.space = 1))
    
    
    # width - inches.per.categ.nb argument
    # Test with positive numeric value
    expect_no_error(width(categ.nb = 10, inch.left.space = 1, inch.right.space = 1, inches.per.categ.nb = 0.2))
    
    # Test with zero
    expect_no_error(width(categ.nb = 10, inch.left.space = 1, inch.right.space = 1, inches.per.categ.nb = 0))
    
    # Test with negative value
    expect_error(width(categ.nb = 10, inch.left.space = 1, inch.right.space = 1, inches.per.categ.nb = -0.2))
    
    
    # width - ini.window.width argument
    # Test with positive numeric value
    expect_no_error(width(categ.nb = 10, inch.left.space = 1, inch.right.space = 1, ini.window.width = 7))
    
    # Test with zero
    expect_no_error(width(categ.nb = 10, inch.left.space = 1, inch.right.space = 1, ini.window.width = 0))
    
    # Test with negative value
    expect_error(width(categ.nb = 10, inch.left.space = 1, inch.right.space = 1, ini.window.width = -7))
    
    
    # width - inch.left.space argument
    # Test with positive numeric value
    expect_no_error(width(categ.nb = 10, inch.left.space = 1, inch.right.space = 1))
    
    # Test with zero
    expect_no_error(width(categ.nb = 10, inch.left.space = 0, inch.right.space = 1))
    
    # Test with negative value
    expect_error(width(categ.nb = 10, inch.left.space = -1, inch.right.space = 1))
    
    
    # width - inch.right.space argument
    # Test with positive numeric value
    expect_no_error(width(categ.nb = 10, inch.left.space = 1, inch.right.space = 1))
    
    # Test with zero
    expect_no_error(width(categ.nb = 10, inch.left.space = 1, inch.right.space = 0))
    
    # Test with negative value
    expect_error(width(categ.nb = 10, inch.left.space = 1, inch.right.space = -1))
    
    
    # width - boundarie.space argument
    # Test with positive numeric value
    expect_no_error(width(categ.nb = 10, inch.left.space = 1, inch.right.space = 1, boundarie.space = 0.5))
    
    # Test with zero
    expect_no_error(width(categ.nb = 10, inch.left.space = 1, inch.right.space = 1, boundarie.space = 0))
    
    # Test with negative value
    expect_error(width(categ.nb = 10, inch.left.space = 1, inch.right.space = 1, boundarie.space = -0.5))
    
    #width - safer_check argument
    # Test with positive numeric value
    expect_no_error(width(categ.nb = 10, inch.left.space = 1, inch.right.space = 1, safer_check = TRUE))

    # Test with zero
    expect_no_error(width(categ.nb = 0, inch.left.space = 1, inch.right.space = 1, safer_check = TRUE))
    
    # Test with non-integer
    expect_error(width(categ.nb = 10.5, inch.left.space = 1, inch.right.space = 1, safer_check = TRUE))

    # width - all arguments
    # Test with valid arguments
    expect_no_error(width(categ.nb = 10, inch.left.space = 1, inch.right.space = 1, inches.per.categ.nb = 0.2, ini.window.width = 7, boundarie.space = 0.5, safer_check = TRUE))
    
    # Test with invalid arguments
    expect_no_error(width(categ.nb = 0, inch.left.space = 1, inch.right.space = 1, inches.per.categ.nb = 0.2, ini.window.width = 7, boundarie.space = 0.5, safer_check = TRUE))
})
