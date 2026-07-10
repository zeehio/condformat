test_that("rule_text_bold works", {
  data(iris)
  x <- condformat(iris[c(1:10, 51:60, 101:110),])
  y <- x |> rule_text_bold(Species,
                            expression = Species == "setosa")
  out <- condformat2html(y)
  expect_match(out, "font-weight: bold.*setosa")
  out <- condformat2latex(y)
  expect_match(out, "\\\\textbf\\{setosa\\}")
  expect_failure(
    expect_match(out, "\\\\textbf\\{versicolor\\}"))
})

test_that("rule_text_bold works for LaTeX output", {
  x <- condformat(data.frame(a = "potato")) |>
    rule_text_bold("a", expression = a == "potato") |>
    condformat2latex() |>
    strsplit(split = "\n", fixed = TRUE)
  expect_true(any(grepl(pattern = "\\textbf{potato}", x[[1]], fixed = TRUE)))
})

test_that("rule_text_bold recycles a scalar expression to every row", {
  out <- condformat(iris[1:5, ]) |>
    rule_text_bold(Species, expression = TRUE) |>
    condformat2html()
  expect_equal(lengths(regmatches(out, gregexpr("font-weight: bold", out))), 5)
})

test_that(".col lets one rule_text_bold call bold several columns by their own values (#19)", {
  x <- data.frame(a = c(1, 5), b = c(5, 1))
  out_col <- x |>
    condformat() |>
    rule_text_bold(c(a, b), .col > 3) |>
    condformat2html()
  out_chained <- x |>
    condformat() |>
    rule_text_bold(a, a > 3) |>
    rule_text_bold(b, b > 3) |>
    condformat2html()
  expect_equal(out_col, out_chained)
})

test_that("rule_text_bold lockcells prevents further LaTeX rules from applying", {
  out <- condformat(data.frame(a = "potato")) |>
    rule_text_bold("a", expression = TRUE, lockcells = TRUE) |>
    rule_text_bold("a", expression = TRUE) |>
    condformat2latex()
  matches <- gregexpr("\\textbf{", out, fixed = TRUE)[[1]]
  n_matches <- if (identical(matches, -1L)) 0L else length(matches)
  expect_equal(n_matches, 1L)
})

test_that("rule_text_bold renders bold cells in a gtable", {
  skip_if_not_installed("gridExtra")
  cfg <- condformat(data.frame(a = "potato", b = "carrot")) |>
    rule_text_bold("a", expression = TRUE) |>
    condformat2grob(draw = FALSE)
  ind_bold <- find_cell(cfg, 2, 2, name = "core-fg")
  ind_normal <- find_cell(cfg, 2, 3, name = "core-fg")
  # grid::gpar(fontface = "bold") actually sets $font (not $fontface) to 2.
  expect_equal(unname(cfg$grobs[ind_bold][[1]][["gp"]][["font"]]), 2)
  expect_equal(unname(cfg$grobs[ind_normal][[1]][["gp"]][["font"]]), 1)
})

test_that("rule_text_bold lockcells prevents further gtable rules from applying", {
  skip_if_not_installed("gridExtra")
  cfg <- data.frame(a = "potato") |>
    condformat() |>
    rule_text_bold("a", expression = TRUE, lockcells = TRUE) |>
    rule_text_bold("a", expression = FALSE) |>
    condformat2grob(draw = FALSE)
  ind <- find_cell(cfg, 2, 2, name = "core-fg")
  expect_equal(unname(cfg$grobs[ind][[1]][["gp"]][["font"]]), 2)
})

test_that("rule_text_bold defaults expression to .col when omitted", {
  out <- condformat(data.frame(a = TRUE)) |>
    rule_text_bold(a) |>
    condformat2html()
  expect_match(out, "font-weight: bold")
})

test_that("rule_text_bold with an empty column selection is a no-op", {
  out <- condformat(data.frame(a = "potato")) |>
    rule_text_bold(starts_with("nonexistent")) |>
    condformat2html()
  expect_false(grepl("font-weight: bold", out, fixed = TRUE))
})
