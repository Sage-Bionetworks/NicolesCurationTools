context("utils.R")

test_that("get_file_data returns NULL for NULL path and unnaccepted ext", {
  res1 <- get_file_data(NULL)
  res2 <- get_file_data(NA)
  res3 <- get_file_data("file_with_no_extension")
  res4 <- get_file_data("file_with_no_extension.")
  res5 <- get_file_data("file.foo")
  expect_null(res1)
  expect_null(res2)
  expect_null(res3)
  expect_null(res4)
  expect_null(res5)
})

# Need to test get_file_data more
