test_that("theme_htmlWidget wraps a plain data.frame into condformat", {
  x <- data.frame(a = 1) |> theme_htmlWidget(number_of_entries = 10)
  expect_s3_class(x, "condformat_tbl")
})

test_that("theme_htmlWidget stores arguments matched by abbreviation", {
  # Arguments are validated against abbreviation-matching (so "num"/"w" are
  # recognized, not warned about as unknown), but stored under their
  # as-given name: the eventual call to htmlTable::htmlTableWidget() still
  # resolves them correctly via R's own partial argument matching.
  x <- condformat(head(iris)) |> theme_htmlWidget(num = 10, w = 200)
  themes <- attr(x, "condformat")[["themes"]]
  finaltheme <- render_theme_condformat_tbl(themes, head(iris))
  expect_equal(finaltheme[["htmlWidget"]][["num"]], 10)
  expect_equal(finaltheme[["htmlWidget"]][["w"]], 200)
})

test_that("theme_htmlWidget warns on an unknown argument", {
  expect_warning(
    condformat(head(iris)) |> theme_htmlWidget(bogus_arg = 1),
    "unknown by htmlTable::htmlTableWidget"
  )
})

test_that("theme_htmlWidget args accumulate across chained calls", {
  x <- condformat(head(iris)) |>
    theme_htmlWidget(number_of_entries = 10) |>
    theme_htmlWidget(width = 200)
  themes <- attr(x, "condformat")[["themes"]]
  finaltheme <- render_theme_condformat_tbl(themes, head(iris))
  expect_equal(finaltheme[["htmlWidget"]][["number_of_entries"]], 10)
  expect_equal(finaltheme[["htmlWidget"]][["width"]], 200)
})
