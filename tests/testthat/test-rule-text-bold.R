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

test_that("rule_text_bold recycles a scalar expression to every row", {
  out <- condformat(iris[1:5, ]) %>%
    rule_text_bold(Species, expression = TRUE) %>%
    condformat2html()
  expect_equal(lengths(regmatches(out, gregexpr("font-weight: bold", out))), 5)
})

test_that(".col lets one rule_text_bold call bold several columns by their own values (#19)", {
  x <- data.frame(a = c(1, 5), b = c(5, 1))
  out_col <- x %>%
    condformat() %>%
    rule_text_bold(c(a, b), .col > 3) %>%
    condformat2html()
  out_chained <- x %>%
    condformat() %>%
    rule_text_bold(a, a > 3) %>%
    rule_text_bold(b, b > 3) %>%
    condformat2html()
  expect_equal(out_col, out_chained)
})

test_that("rule_text_bold lockcells prevents further LaTeX rules from applying", {
  out <- condformat(data.frame(a = "potato")) %>%
    rule_text_bold("a", expression = TRUE, lockcells = TRUE) %>%
    rule_text_bold("a", expression = TRUE) %>%
    condformat2latex()
  matches <- gregexpr("\\textbf{", out, fixed = TRUE)[[1]]
  n_matches <- if (identical(matches, -1L)) 0L else length(matches)
  expect_equal(n_matches, 1L)
})
