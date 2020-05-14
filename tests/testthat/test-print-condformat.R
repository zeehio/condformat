test_that("print.condformat_tbl returns its input", {
  data(iris)
  x <- condformat(head(iris, n = 1))
  # capture.output is used so we don't pollute the test output
  utils::capture.output(out <- print(x, viewer = NULL))
  expect_identical(out, x)
})

