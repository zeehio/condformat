# Tests:
library(condformat)
library(dplyr)
library(testthat)
context("rendering")

test_that("print.condformat_tbl returns its input", {
  data(iris)
  x <- condformat(head(iris))
  out <- print(x)
  expect_that(out,is_identical_to(x))
})

test_that("knitr returns an HTML table", {
  data(iris)
  library(knitr)
  knitr::opts_knit$set(out.format = "html")
  out <- knit_print(condformat(head(iris)))
  expect_that(out, matches("^<table.*</table>$"))
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
  expect_that(nrow(output), equals(3))
  expect_that(ncol(output), equals(2))
  expect_that(output, equals(expected_output))
})


