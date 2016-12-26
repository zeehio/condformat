# Tests:
context("rule_fill_gradient")

test_that("rule_fill_gradient works", {
  data(iris)
  x <- condformat(iris[c(1:10, 51:60, 101:110),])
  y <- x + rule_fill_gradient(Sepal.Length)
  out <- condformat2html(y)
  expect_match(out, "^<table.*</table>$")

  y <- x + rule_fill_gradient_("Sepal.Length")
  out <- condformat2html(y)
  expect_match(out, "^<table.*</table>$")
})

test_that("rule_fill_gradient2 works", {
  data(iris)
  x <- condformat(iris[c(1:10, 51:60, 101:110),])
  y <- x + rule_fill_gradient2(Sepal.Length)
  out <- condformat2html(y)
  expect_match(out, "^<table.*</table>$")

  y <- x + rule_fill_gradient2_("Sepal.Length")
  out <- condformat2html(y)
  expect_match(out, "^<table.*</table>$")
})

test_that("rule_fill_gradient is created", {
  expect_warning(rule_fill_gradient(ColA, ColB))
})
