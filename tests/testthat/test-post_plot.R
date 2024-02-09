test_that("Test for x.side argument", {
    plot(x = 1:3)
    # ARGUMENT x.side CAN ONLY BE 1 OR 3
    expect_error(post_plot(x.side = 5))
    expect_no_error(post_plot(x.side = 1))
    expect_no_error(post_plot(x.side = 3))
    
    # Test for x.log.scale argument
    expect_no_error(post_plot(x.log.scale = TRUE))
    expect_no_error(post_plot(x.log.scale = FALSE))
    
    
    # Test for x.categ argument
    expect_no_error(post_plot(x.categ = c("A", "B", "C")))
    
    
    # Test for y.side argument
    expect_error(post_plot(y.side = 5))
    expect_no_error(post_plot(y.side = 2))
    expect_no_error(post_plot(y.side = 4))
    
    
    # Test for y.log.scale argument
    expect_no_error(post_plot(y.log.scale = TRUE))
    expect_no_error(post_plot(y.log.scale = FALSE))
    
    
    # Test for y.categ argument
    expect_no_error(post_plot(y.categ = c("A", "B", "C")))
    
    
    # Test for x.color argument
    expect_error(post_plot(x.color = "blue"))
    
    
    # Test for y.color argument
    expect_error(post_plot(y.color = "red"))
    
    
    # Test for main argument
    expect_error(post_plot(main = "Title"))
    
    
    # Tests for combinations of arguments
    
    expect_error(post_plot(x.side = 5, x.log.scale = TRUE))
    
    
    # Test for x.categ and x.log.scale arguments combination
    expect_no_error(post_plot(x.categ = c("A", "B", "C"), x.log.scale = TRUE))
    
    # Test for y.side and y.log.scale arguments combination"
    expect_error(post_plot(y.side = 1, y.log.scale = TRUE))
    
    
    # Test for y.categ and y.log.scale arguments combination
    expect_no_error(post_plot(y.categ = c("A", "B", "C"), y.log.scale = TRUE))
    
    
    # Test for x.color and y.color arguments combination
    expect_error(post_plot(x.color = "blue", y.color = "red"))
    
    
    # Test for main and x.color arguments combination"
    expect_error(post_plot(main = "Title", x.color = "blue"))
})