test_that("theme_kable wraps a plain data.frame into condformat", {
  x <- data.frame(a = 1) |> theme_kable(booktabs = TRUE)
  expect_s3_class(x, "condformat_tbl")
})

test_that("theme_kable args accumulate across chained calls", {
  x <- condformat(head(iris)) |>
    theme_kable(booktabs = TRUE) |>
    theme_kable(align = "c")
  themes <- attr(x, "condformat")[["themes"]]
  finaltheme <- render_theme_condformat_tbl(themes, head(iris))
  expect_true(finaltheme[["kable_args"]][["booktabs"]])
  expect_equal(finaltheme[["kable_args"]][["align"]], "c")
})

test_that("theme_kable can explicitly reset an argument to NULL", {
  x <- condformat(head(iris)) |>
    theme_kable(align = "c") |>
    theme_kable(align = NULL)
  themes <- attr(x, "condformat")[["themes"]]
  finaltheme <- render_theme_condformat_tbl(themes, head(iris))
  expect_true("align" %in% names(finaltheme[["kable_args"]]))
  expect_null(finaltheme[["kable_args"]][["align"]])
})
