test_that("theme_htmlTable works", {
  data(iris)
  x <- condformat(head(iris)) |> theme_htmlTable(caption = "Table 1: MySimpleTestCaption")
  out <- condformat2html(x)
  expect_match(out, "MySimpleTestCaption")
})

test_that("theme_htmlTable args accumulate across chained calls", {
  data(iris)
  out <- condformat(head(iris)) |>
    theme_htmlTable(caption = "MyChainedCaption") |>
    theme_htmlTable(rnames = FALSE) |>
    condformat2html()
  expect_match(out, "MyChainedCaption")
})

test_that("theme_caption works", {
  x <- data.frame(a = 1) |> theme_caption("potato") |> condformat2html()
  out <- strsplit(x, "\n", fixed = TRUE)[[1]]
  expect_true(any(grepl("potato", out, fixed = TRUE)))
  x <- data.frame(a = 1) |> theme_htmlTable(caption = "potato") |> condformat2html()
  out <- strsplit(x, "\n", fixed = TRUE)[[1]]
  expect_true(any(grepl("potato", out, fixed = TRUE)))
})

test_that("theme_htmlTable redirects deprecated htmlWidget-style arguments", {
  expect_warning(
    x <- condformat(head(iris)) |> theme_htmlTable(number_of_entries = 10),
    "should be given to theme_htmlWidget"
  )
  themes <- attr(x, "condformat")[["themes"]]
  finaltheme <- render_theme_condformat_tbl(themes, head(iris))
  expect_equal(finaltheme[["htmlWidget"]][["number_of_entries"]], 10)
})

test_that("theme_htmlTable errors on a genuinely unknown argument", {
  expect_error(
    condformat(head(iris)) |> theme_htmlTable(totally_bogus_arg = 1),
    "unknown by htmlTable"
  )
})
