# Tests:
library(condformat)
library(dplyr)
library(testthat)
context("show")

test_that("rule_fill_discrete works", {
  data(iris)
  x <- condformat(iris[c(1:10, 51:60, 101:110),])
  y <- x + rule_fill_discrete(Species,
                              expression  = Sepal.Length > max(Sepal.Length),
                              colours = c("TRUE" = "red", "FALSE" = "blue"))
  out <- condformat2html(y)
  expect_that(out[1], not(matches("red")))

  y <- x + rule_fill_discrete(Species,
                              expression  = Sepal.Length >= min(Sepal.Length),
                              colours = c("TRUE" = "red", "FALSE" = "blue"))
  out <- condformat2html(y)
  expect_that(out[1], not(matches("blue")))

  y <- x + rule_fill_discrete(Species)
  out <- condformat2html(y)
  expect_that(out[1], matches("^<table.*</table>$"))
})


