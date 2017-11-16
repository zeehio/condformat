# Tests:
context("rule_text_bold")

test_that("rule_text_bold works", {
  data(iris)
  x <- condformat(iris[c(1:10, 51:60, 101:110),])
  y <- x %>% rule_text_bold(Species,
                            expression = Species == "setosa")
  out <- condformat2html(y)
  expect_match(out, "font-weight: bold.*setosa")
  out <- condformat2latex(y)
  expect_match(out, "\\\\textbf\\{setosa\\}")
  expect_failure(
    expect_match(out, "\\\\textbf\\{versicolor\\}"))
})

test_that("rule_text_bold works for LaTeX output", {
  x <- condformat(data.frame(a = "potato")) %>%
    rule_text_bold("a", expression = a == "potato") %>%
    condformat2latex() %>%
    strsplit(split = "\n", fixed = TRUE)
  expect_true(any(grepl(pattern = "\\textbf{potato}", x[[1]], fixed = TRUE)))
})
