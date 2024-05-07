test_that("The function prior_plot() works correctly", {
    expect_error(prior_plot(param.reinitial = TRUE))
    expect_no_error(prior_plot(param.reinitial = FALSE))
    
    
    # Test for xlog.scale argument
    expect_no_error(prior_plot(xlog.scale = TRUE))
    expect_no_error(prior_plot(xlog.scale = FALSE))
    
    
    # Test for ylog.scale argument
    expect_no_error(prior_plot(ylog.scale = TRUE))
    expect_no_error(prior_plot(ylog.scale = FALSE))
    
    
    # Test for remove.label argument
    expect_no_error(prior_plot(remove.label = TRUE))
    expect_no_error(prior_plot(remove.label = FALSE))
    
    
    # Test for remove.x.axis argument
    expect_no_error(prior_plot(remove.x.axis = TRUE))
    expect_no_error(prior_plot(remove.x.axis = FALSE))
    
    
    # Test for remove.y.axis argument
    expect_no_error(prior_plot(remove.y.axis = TRUE))
    expect_no_error(prior_plot(remove.y.axis = FALSE))
    
    
    # Test for std.x.range argument
    expect_no_error(prior_plot(std.x.range = TRUE))
    expect_no_error(prior_plot(std.x.range = FALSE))
    
    # Test for std.y.range argument
    expect_no_error(prior_plot(std.y.range = TRUE))
    expect_no_error(prior_plot(std.y.range = FALSE))
    
    
    # Test for down.space argument"
    expect_no_error(prior_plot(down.space = 1))
    expect_no_error(prior_plot(down.space = 2))
    
    
    # Test for left.space argument"
    expect_no_error(prior_plot(left.space = 1))
    expect_no_error(prior_plot(left.space = 2))
    
    
    # Test for up.space argument
    expect_no_error(prior_plot(up.space = 1))
    expect_no_error(prior_plot(up.space = 2))
    
    
    # Test for right.space argument
    expect_no_error(prior_plot(right.space = 1))
    expect_no_error(prior_plot(right.space = 2))
    
    
    # Test for orient argument"
    expect_no_error(prior_plot(orient = 1))
    expect_no_error(prior_plot(orient = 2))
    
    # Test for dist.legend argument"
    expect_no_error(prior_plot(dist.legend = 3.5))
    expect_no_error(prior_plot(dist.legend = 4.5))
    
    
    # Test for tick.length argument
    expect_no_error(prior_plot(tick.length = 0.5))
    expect_no_error(prior_plot(tick.length = 0.6))
    
    
    # Test for box.type argument
    expect_no_error(prior_plot(box.type = "n"))
    expect_no_error(prior_plot(box.type = "o"))
    
    
    # Test for amplif.label argument
    expect_no_error(prior_plot(amplif.label = 1))
    expect_no_error(prior_plot(amplif.label = 2))
    
    # Test for amplif.axis argument
    expect_no_error(prior_plot(amplif.axis = 1))
    expect_no_error(prior_plot(amplif.axis = 2))
    
    
    # Test for display.extend argument
    expect_no_error(prior_plot(display.extend = TRUE))
    expect_no_error(prior_plot(display.extend = FALSE))
    
    # Test for return.par argument
    expect_no_error(prior_plot(return.par = TRUE))
    expect_no_error(prior_plot(return.par = FALSE))

    # Test for safer_check argument
    expect_no_error(prior_plot(safer_check = TRUE))
    expect_no_error(prior_plot(safer_check = FALSE))
    
    
    # Test for all arguments combined
    expect_error(prior_plot(param.reinitial = TRUE,
                               xlog.scale = TRUE,
                               ylog.scale = TRUE,
                               remove.label = FALSE,
                               remove.x.axis = FALSE,
                               remove.y.axis = FALSE,
                               std.x.range = FALSE,
                               std.y.range = FALSE,
                               down.space = 2,
                               left.space = 2,
                               up.space = 2,
                               right.space = 2,
                               orient = 2,
                               dist.legend = 4.5,
                               tick.length = 0.6,
                               box.type = "o",
                               amplif.label = 2,
                               amplif.axis = 2,
                               display.extend = TRUE,
                               return.par = TRUE))
})