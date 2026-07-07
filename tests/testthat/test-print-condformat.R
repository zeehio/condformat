test_that("print.condformat_tbl returns its input", {
  data(iris)
  x <- condformat(head(iris, n = 1))
  # capture.output is used so we don't pollute the test output
  utils::capture.output(out <- print(x, viewer = NULL))
  expect_identical(out, x)
})

test_that("print.condformat_tbl works with paginate = FALSE", {
  data(iris)
  x <- condformat(head(iris, n = 1))
  utils::capture.output(out <- print(x, viewer = NULL, paginate = FALSE))
  expect_identical(out, x)
})

test_that("print.condformat_tbl delegates to knit_print when knitr.in.progress is set", {
  old <- getOption("knitr.in.progress")
  options(knitr.in.progress = TRUE)
  on.exit(options(knitr.in.progress = old), add = TRUE)
  x <- condformat(head(iris, n = 1))
  # outside an actual knitting session neither is_latex_output() nor
  # is_html_output() are TRUE, so this only fires via knit_print's fallback -
  # confirming print.condformat_tbl delegated to it
  expect_warning(print(x), "Output format not supported by condformat")
})

test_that("knit_print.condformat_tbl warns and falls back outside html/latex output", {
  x <- condformat(head(iris, n = 1))
  expect_warning(
    knitr::knit_print(x),
    "Output format not supported by condformat")
})

test_that("condformat2html_or_widget returns HTML() directly when paginate = FALSE", {
  x <- condformat(head(iris, n = 1))
  out <- condformat2html_or_widget(x, paginate = FALSE)
  expect_s3_class(out, "html")
  expect_match(as.character(out), "^<table")
})

test_that("condformat2html_or_widget returns a widget's tags when paginate = TRUE", {
  x <- condformat(head(iris, n = 1))
  out <- condformat2html_or_widget(x, paginate = TRUE)
  expect_false(inherits(out, "html"))
})
