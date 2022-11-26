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

test_that("rule_fill_discrete syntax with multiple variables and no expression gives warning", {
  expect_warning(
    condformat2html(
      rule_fill_discrete(condformat(head(iris)),
                         c("Species", "Sepal.Length"))),
    "multiple columns")
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


test_that("rule_fill_discrete gtable works", {
  skip_if_not_installed("vdiffr")
  cfg <- condformat(iris[c(1,70, 120), "Species", drop = FALSE]) %>%
    rule_fill_discrete(Species) %>%
    condformat2grob(draw = FALSE)
  expect_equal(cfg$grobs[[14]]$gp$fill, "#F8766D")
  expect_equal(cfg$grobs[[15]]$gp$fill, "#00BA38")
  expect_equal(cfg$grobs[[16]]$gp$fill, "#619CFF")
  vdiffr::expect_doppelganger(title = "rule_fill_discrete gtable", cfg)
})
