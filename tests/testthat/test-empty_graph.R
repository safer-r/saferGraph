test_that("empty_graph function argument tests", {
  
  # Test for 'text' argument
  expect_no_error(empty_graph(text = "No Graph"))
  # expect_error(empty_graph(text = NULL))
  
  # Test for 'text.size' argument
  expect_no_error(empty_graph(text.size = 1))
  expect_error(empty_graph(text.size = NULL))
  # expect_error(empty_graph(text.size = "invalid_size"))
  
  # Test for 'title' argument
  expect_no_error(empty_graph(title = "Graph Title"))
  # expect_error(empty_graph(title = NULL))
  
  # Test for 'title.size' argument
  expect_no_error(empty_graph(title.size = 1.5))
  expect_error(empty_graph(title.size = NULL))
   # expect_error(empty_graph(title.size = "invalid_size"))
  
  
  # Test with default arguments
  expect_no_error(empty_graph())
  
  # Test with specified arguments
  expect_no_error(empty_graph(text = "No Graph", text.size = 1, title = "Graph Title", title.size = 1.5, safer_check = TRUE))
  
  # Test with invalid arguments
  # expect_error(empty_graph(text = NULL, text.size = 1, title = "Graph Title", title.size = 1.5))
  expect_error(empty_graph(text = "No Graph", text.size = NULL, title = "Graph Title", title.size = 1.5, safer_check = TRUE))
  # expect_error(empty_graph(text = "No Graph", text.size = 1, title = NULL, title.size = 1.5))
  expect_error(empty_graph(text = "No Graph", text.size = 1, title = "Graph Title", title.size = NULL, safer_check = TRUE))
  expect_error(empty_graph(text = "No Graph", text.size = "invalid_size", title = "Graph Title", title.size = 1.5, safer_check = TRUE))
  expect_error(empty_graph(text = "No Graph", text.size = 1, title = "Graph Title", title.size = "invalid_size", safer_check = TRUE))
  
})