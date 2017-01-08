# Tests:
context("rule_fill_discrete")

test_that("rule_fill_discrete works", {
  data(iris)
  x <- condformat(iris[c(1:10, 51:60, 101:110),])
  y <- x + rule_fill_discrete(Species,
                              expression  = Sepal.Length > max(Sepal.Length),
                              colours = c("TRUE" = "red", "FALSE" = "blue"))
  out <- condformat2html(y)
  expect_failure(expect_match(out, "red"))

  y <- x + rule_fill_discrete(Species,
                              expression  = Sepal.Length >= min(Sepal.Length),
                              colours = c("TRUE" = "red", "FALSE" = "blue"))
  out <- condformat2html(y)
  expect_failure(expect_match(out, "blue"))

  y <- x + rule_fill_discrete(Species)
  out <- condformat2html(y)
  expect_match(out, "^<table.*</table>$")
})


test_that("rule_fill_discrete lock cells", {
  data(iris)
  x <- condformat(head(iris))
  y <- x +
    rule_fill_discrete(Species,
                       expression  = 1,
                       colours = c("1" = "red")) +
    rule_fill_discrete(Species,
                       expression  = 1,
                       colours = c("1" = "blue"))
  out <- condformat2html(y)
  expect_failure(expect_match(out, "red"))

  y <- x +
    rule_fill_discrete(Species,
                       expression  = 1,
                       colours = c("1" = "red"),
                       lockcells = TRUE) +
    rule_fill_discrete(Species,
                       expression  = 1,
                       colours = c("1" = "blue"))
  out <- condformat2html(y)
  expect_failure(expect_match(out, "blue"))
})


test_that("rule_fill_discrete(_) syntax with multiple variables and no expression gives warning", {
  expect_warning(rule_fill_discrete(Species, Sepal.Length),
                 "multiple variables")
  expect_warning(rule_fill_discrete_(columns = c("Species", "Sepal.Length")),
                 "multiple variables")
})

test_that("rule_fill_discrete_ works", {
  data(iris)
  x <- condformat(iris[c(1, 2, 51, 101, 102), ])
  y <- x + rule_fill_discrete_("Species", colours = c("virginica" = "#FF0000",
                                                      "versicolor" = "#00FF00",
                                                      "setosa" = "#0000FF"))
  out <- condformat2html(y)
  expect_equal(
    length(sapply(strsplit(as.character(out), "\n", fixed = TRUE),
                  function(line) grep("(#0000FF.*setosa)", line))),
    2)
  expect_equal(
    length(sapply(strsplit(as.character(out), "\n", fixed = TRUE),
                  function(line) grep("(#00FF00.*versicolor)", line))),
    1)
  expect_equal(
    length(sapply(strsplit(as.character(out), "\n", fixed = TRUE),
         function(line) grep("(#FF0000.*virginica)", line))),
    2)

})

test_that("rule_fill_discrete_ works with formula", {
  data(iris)
  x <- condformat(iris[c(1, 2, 51, 101, 102), ])
  y <- x + rule_fill_discrete_("Species", expression = ~Sepal.Length > 5.5,
                               colours = c("TRUE" = "#FF0000",
                                           "FALSE" = "#00FF00"))
  out <- condformat2html(y)
  expect_match(out, "^<table.*</table>$")
  expect_equal(length(gregexpr("#FF0000", out, fixed = TRUE)[[1]]),
               3)
  expect_equal(length(gregexpr("#00FF00", out, fixed = TRUE)[[1]]),
               2)
})

test_that("rule_fill_discrete_ works programmatically", {
  data(iris)
  color_data_column_by_column <- function(data, color_column, by_column) {
    condformat(data) + rule_fill_gradient_(color_column, ~ uq(as.name(by_column)))
  }

  expect_equal(color_data_column_by_column(iris[c(1,51,101),], "Species", "Petal.Length"),
               condformat(iris[c(1,51,101),]) + rule_fill_gradient_("Species", ~ Petal.Length))
})

test_that("custom rule_ passes doing nothing", {
  custom_ruleobj <- structure(list(),
                              class = c("condformat_rule"))
  data(iris)
  x <- condformat(head(iris))
  y <- x + custom_ruleobj
  out_x <- condformat2html(x)
  out_y <- condformat2html(y)
  expect_identical(out_x, out_y)
})
