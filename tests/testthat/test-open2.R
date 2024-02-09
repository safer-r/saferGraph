test_that("open2 function argument tests", {
    
    # Test for 'pdf' argument
    expect_error(open2(pdf = "not_logical"))
    
    # Test for 'pdf.path' argument
    expect_error(open2(pdf.path = "not_existing_path"))
    
    # Test for 'pdf.name' argument
    expect_error(open2(pdf.name = 123))
    
    # Test for 'width' argument
    expect_error(open2(width = "not_numeric"))
    
    # Test for 'height' argument
    expect_error(open2(height = "not_numeric"))
    
    # Test for 'paper' argument
    expect_error(open2(paper = "not_in_options"))
    
    # Test for 'pdf.overwrite' argument
    expect_error(open2(pdf.overwrite = "not_logical"))
    
    # Test for 'rescale' argument
    expect_error(open2(rescale = "not_in_options"))
    
    # Test for 'remove.read.only' argument
    expect_error(open2(remove.read.only = "not_logical"))
    
    # Test for 'return.output' argument (no specific test)
    
    # Test case for all arguments combined
    
    # Test with default arguments
    # expect_error(open2())
    
    # Test with specified arguments
    expect_error(open2(pdf = "not_logical", pdf.path = "not_existing_path", pdf.name = 123, width = "not_numeric", height = "not_numeric", paper = "not_in_options", pdf.overwrite = "not_logical", rescale = "not_in_options", remove.read.only = "not_logical", return.output = "not_logical"))
})