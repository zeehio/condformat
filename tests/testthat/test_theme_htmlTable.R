test_that("theme_htmlTable works", {
  data(iris)
  x <- condformat(head(iris)) + theme_htmlTable(caption = "Table 1: MySimpleTestCaption")
  out <- condformat2html(x)
  expect_match(out, "MySimpleTestCaption")
})
