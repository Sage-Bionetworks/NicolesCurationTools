context("utils_tables.R")

# data frame
data <- data.frame(
  col1 = c("a", NA, NA, "b", NA),
  col2 = c(1, NA, 2, NA, NA),
  col3 = c(NA, NA, TRUE, NA, NA)
)

test_that("remove_na_rows removes NA rows across all columns", {
  # data frame
  res1 <- remove_na_rows(data)
  # tibble
  res2 <- remove_na_rows(tibble::as_tibble(data))

  expected <- data[-c(2, 5), ]

  expect_equal(res1, expected)
  expect_equal(res2, tibble::as_tibble(expected))
})

test_that("remove_na_rows removes NA rows across given columns", {
  res1 <- remove_na_rows(data, columns = "col1")
  res2 <- remove_na_rows(data, columns = c("col1", "col2"))
  res3 <- remove_na_rows(data, columns = c("col1", "col3"))
  res4 <- remove_na_rows(data, columns = c("col2", "col3"))

  expect_equal(res1, data[-c(2, 3, 5), ])
  expect_equal(res2, data[-c(2, 5), ])
  expect_equal(res3, data[-c(2, 5), ])
  expect_equal(res4, data[-c(2, 4, 5), ])
})
