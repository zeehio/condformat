# Tests:
context("rule_fill_discrete")

test_that("rule_fill_discrete works (0.6 syntax)", {
  # Deprecated
  data(iris)
  x <- condformat(iris[c(1:10, 51:60, 101:110),])
  expect_warning(
    r <- rule_fill_discrete(Species,
                            expression  = Sepal.Length > max(Sepal.Length),
                            colours = c("TRUE" = "red", "FALSE" = "blue")),
    regexp = ".*deprecated.*")
  y <- x + r
  out <- condformat2html(y)
  expect_failure(expect_match(out, "red"))

  expect_warning(
    r <- rule_fill_discrete(Species,
                            expression  = Sepal.Length >= min(Sepal.Length),
                            colours = c("TRUE" = "red", "FALSE" = "blue")))
  y <- x + r
  out <- condformat2html(y)
  expect_failure(expect_match(out, "blue"))

  expect_warning(r <- rule_fill_discrete(Species))
  y <- x + r
  out <- condformat2html(y)
  expect_match(out, "^<table.*</table>$")
})

test_that("rule_fill_discrete works", {
  data(iris)
  x <- condformat(iris[c(1:10, 51:60, 101:110),])
  y <- x %>% rule_fill_discrete("Species",
                                expression = Sepal.Length > max(Sepal.Length),
                                colours = c("TRUE" = "red", "FALSE" = "blue"))
  out <- condformat2html(y)
  expect_failure(expect_match(out, "red"))

  y <- x %>% rule_fill_discrete("Species",
                                expression  = Sepal.Length >= min(Sepal.Length),
                                colours = c("TRUE" = "red", "FALSE" = "blue"))

  out <- condformat2html(y)
  expect_failure(expect_match(out, "blue"))

  y <- x %>% rule_fill_discrete("Species")
  out <- condformat2html(y)
  expect_match(out, "^<table.*</table>$")

  y <- x %>% rule_fill_discrete(starts_with("Species"))
  out <- condformat2html(y)
  expect_match(out, "^<table.*</table>$")

  y <- x %>% rule_fill_discrete(c(starts_with("Species"), starts_with("Sepal")),
                                expression = Species)
  out <- condformat2html(y)
  expect_match(out, "^<table.*</table>$")

})


test_that("rule_fill_discrete lock cells (0.6 syntax)", {
  # Deprecated 0.6 API
  data(iris)
  x <- condformat(head(iris))
  expect_warning(
    r1 <- rule_fill_discrete(Species,
                             expression  = 1,
                             colours = c("1" = "red")))
  expect_warning(
    r2 <- rule_fill_discrete(Species,
                             expression  = 1,
                             colours = c("1" = "blue"))
  )
  y <- x + r1 + r2
  out <- condformat2html(y)
  expect_failure(expect_match(out, "red"))

  expect_warning(
    r1 <- rule_fill_discrete(Species,
                             expression  = 1,
                             colours = c("1" = "red"),
                             lockcells = TRUE)
  )
  expect_warning(
    r2 <- rule_fill_discrete(Species,
                             expression  = 1,
                             colours = c("1" = "blue"))
  )
  y <- x + r1 + r2
  out <- condformat2html(y)
  expect_failure(expect_match(out, "blue"))
})

test_that("rule_fill_discrete lock cells", {
  data(iris)
  x <- condformat(head(iris))
  y <- x %>% rule_fill_discrete("Species",
                                expression  = 1,
                                colours = c("1" = "red")) %>%
    rule_fill_discrete("Species",
                       expression  = 1,
                       colours = c("1" = "blue"))
  out <- condformat2html(y)
  expect_failure(expect_match(out, "red"))

  y <- x %>% rule_fill_discrete("Species",
                                expression  = 1,
                                colours = c("1" = "red"),
                                lockcells = TRUE) %>%
    rule_fill_discrete("Species",
                       expression  = 1,
                       colours = c("1" = "blue"))
  out <- condformat2html(y)
  expect_failure(expect_match(out, "blue"))
})


test_that("rule_fill_discrete(_) syntax with multiple variables and no expression gives warning (0.6 syntax)", {
  # Deprecated (test 0.6 API)
  expect_warning(rule_fill_discrete(Species, Sepal.Length),
                 "multiple variables")
  expect_warning(rule_fill_discrete_(columns = c("Species", "Sepal.Length")),
                 "multiple columns")
})

test_that("rule_fill_discrete syntax with multiple variables and no expression gives warning", {
  expect_warning(
    condformat2html(
      rule_fill_discrete(condformat(head(iris)),
                         c("Species", "Sepal.Length"))),
    "multiple columns")
})


test_that("rule_fill_discrete_ works (0.6 syntax)", {
  # Deprecated
  data(iris)
  x <- condformat(iris[c(1, 2, 51, 101, 102), ])
  expect_warning(r1 <- rule_fill_discrete_("Species", colours = c("virginica" = "#FF0000",
                                                                  "versicolor" = "#00FF00",
                                                                  "setosa" = "#0000FF")))
  y <- x + r1
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

test_that("rule_fill_discrete_ works with formula (0.6 syntax)", {
  # Deprecated
  data(iris)
  x <- condformat(iris[c(1, 2, 51, 101, 102), ])
  expect_warning(r1 <- rule_fill_discrete_("Species", expression = ~Sepal.Length > 5.5,
                                           colours = c("TRUE" = "#FF0000",
                                                       "FALSE" = "#00FF00")))
  y <- x + r1
  out <- condformat2html(y)
  expect_match(out, "^<table.*</table>$")
  expect_equal(length(gregexpr("#FF0000", out, fixed = TRUE)[[1]]),
               3)
  expect_equal(length(gregexpr("#00FF00", out, fixed = TRUE)[[1]]),
               2)
})

test_that("rule_fill_discrete_ works programmatically (0.6 syntax)", {
  # Deprecated
  data(iris)
  color_data_column_by_column <- function(data, color_column, by_column) {
    expect_warning(r1 <- rule_fill_gradient_(color_column, ~ uq(as.name(by_column))))
    condformat(data) + r1
  }

  expect_warning(r2 <- rule_fill_gradient_("Species", ~ Petal.Length))
  expect_equal(color_data_column_by_column(iris[c(1,51,101),], "Species", "Petal.Length"),
               condformat(iris[c(1,51,101),]) + r2)
})

test_that("rule_fill_discrete has expected LaTeX output", {
  latex <- condformat(data.frame(a = c("Dog", "Cat", "Mouse"))) %>%
    rule_fill_discrete("a", colours = c("Dog" = "#A52A2A")) %>%
    condformat2latex()
  expect_true("\\cellcolor[HTML]{A52A2A}Dog\\\\" %in% strsplit(latex, "\n", fixed = TRUE)[[1]])
})

test_that("rule_fill_discrete accepts a function as colours=", {
  num_to_colour <- function(x) {
    ifelse(x == "potato", "#FF0000", "#00FF00")
  }

  x <- data.frame(a = c("potato", "apple")) %>%
    condformat() %>%
    rule_fill_discrete("a", colours = num_to_colour) %>%
    condformat2html() %>%
    strsplit("\n", fixed = TRUE)
  expect_true(
    any(
      grepl(pattern = "background-color: #FF0000", x[[1]]) &
        grepl(pattern = "potato", x[[1]])))
  expect_true(
    any(
      grepl(pattern = "background-color: #00FF00", x[[1]]) &
        grepl(pattern = "apple", x[[1]])))
})
