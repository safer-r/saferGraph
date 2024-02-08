test_that("close2 function argument tests", {
  
  # Test for 'kind' argument
  expect_no_error(close2(kind = "pdf"))
  # expect_error(close2(kind = "invalid_kind"))
  # expect_error(close2(kind = "invalid_kind"), class = "stop")
  
  # Test for 'return.text' argument
  expect_no_error(close2(return.text = TRUE))
  # expect_error(close2(return.text = "invalid"), class = "stop")
  

# Test case for all arguments combined
  
  # Test with default arguments
  expect_no_error(close2())
  
  # Test with specified kind and return.text
  expect_no_error(close2(kind = "pdf", return.text = TRUE))
  
  # Test with non-default kind
  expect_no_error(close2(kind = "invalid"))
  
  # Test with invalid kind
  expect_no_error(close2(kind = "invalid_kind"))
  
  
  # Test with invalid return.text
  expect_no_error(close2(return.text = "invalid"))
  
})
