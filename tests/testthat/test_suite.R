# Tests:
library(condformat)
library(dplyr)

context("printing condformat")

test_that("print works", {
  data(iris)
  print(condformat(iris))
})
