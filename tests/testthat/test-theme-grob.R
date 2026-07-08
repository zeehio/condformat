test_that("theme_grob stores NULL values instead of dropping them", {
  x <- condformat(head(iris)) %>% theme_grob(rows = NULL)
  themes <- attr(x, "condformat")[["themes"]]
  finaltheme <- render_theme_condformat_tbl(themes, head(iris))
  expect_true("rows" %in% names(finaltheme[["tableGrobArgs"]]))
  expect_null(finaltheme[["tableGrobArgs"]][["rows"]])
})

test_that("theme_grob args accumulate across chained calls", {
  x <- condformat(head(iris)) %>%
    theme_grob(rows = NULL) %>%
    theme_grob(cols = 1:2)
  themes <- attr(x, "condformat")[["themes"]]
  finaltheme <- render_theme_condformat_tbl(themes, head(iris))
  expect_true("rows" %in% names(finaltheme[["tableGrobArgs"]]))
  expect_null(finaltheme[["tableGrobArgs"]][["rows"]])
  expect_equal(finaltheme[["tableGrobArgs"]][["cols"]], 1:2)
})
