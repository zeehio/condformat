# Tests:
context("rendering")

test_that("print.condformat_tbl returns its input", {
  data(iris)
  x <- condformat(head(iris, n = 1))
  out <- print(x)
  expect_identical(out, x)
})

test_that("knitr returns an HTML table", {
  data(iris)
  knitr::opts_knit$set(out.format = "html")
  out <- knitr::knit_print(condformat(head(iris)))
  expect_match(out, "^<table.*</table>$")
})

test_that("condformat2excel works", {
  data(iris)
  filename <- tempfile()
  out <- condformat2excel(condformat(head(iris)), file = filename)
  expect_true(file.exists(filename))
  unlink(filename)
})


test_that("merge_css_conditions returns the expected", {
  css_fields <- list("background-color" = matrix(c("red", "red",
                                             "blue", "green",
                                             "yellow", "orange"),
                                           nrow = 3, ncol = 2, byrow = TRUE),
                     "text-align" = matrix(c("left", "right",
                                             "left", "center",
                                             "right", "left"),
                                           nrow = 3, ncol = 2, byrow = TRUE))

  output <- merge_css_conditions(matrix("", nrow = 3, ncol = 2), css_fields)
  expected_output <- matrix(c("; background-color: red; text-align: left", "; background-color: red; text-align: right",
                              "; background-color: blue; text-align: left", "; background-color: green; text-align: center",
                              "; background-color: yellow; text-align: right", "; background-color: orange; text-align: left"),
                            nrow = 3, ncol = 2, byrow = TRUE)
  expect_equal(dim(output), c(3,2))
  expect_equal(output, expected_output)
})


