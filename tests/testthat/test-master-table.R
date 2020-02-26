context("master-table.R")

test_that("append_column_study appends column with study name", {
  data <- data.frame(col1 = c("a", "b", "c"), col2 = c(1, 2, 3))
  # data frame
  res1 <- append_column_study(data, "mystudy")
  # tibble
  res2 <- append_column_study(tibble::as_tibble(data), "mystudy")
  expect_true("study" %in% colnames(res1))
  expect_true("study" %in% names(res2))
  expect_true(all(res1$study == "mystudy"))
  expect_true(all(res2$study == "mystudy"))
})

test_that("append_column_study returns error if study column exists", {
  data <- data.frame(col1 = c("a", "b", "c"), study = c(1, 2, 3))
  expect_error(
    append_column_study(data, "mystudy"),
    regexp = "study column already exists"
  )
})
