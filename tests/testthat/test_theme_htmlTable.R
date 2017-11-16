context("theme_htmlTable")
test_that("theme_htmlTable works", {
  data(iris)
  x <- condformat(head(iris)) %>% theme_htmlTable(caption = "Table 1: MySimpleTestCaption")
  out <- condformat2html(x)
  expect_match(out, "MySimpleTestCaption")
})

test_that("theme_htmlTable works (old API)", {
  # Deprecated
  data(iris)
  expect_warning(t1 <- theme_htmlTable(caption = "Table 1: MySimpleTestCaption"))
  x <- condformat(head(iris)) + t1
  out <- condformat2html(x)
  expect_match(out, "MySimpleTestCaption")
})

test_that("theme_caption works", {
  x <- data.frame(a = 1) %>% theme_caption("potato") %>% condformat2html()
  out <- strsplit(x, "\n", fixed = TRUE)[[1]]
  expect_true(any(grepl("potato", out, fixed = TRUE)))
  x <- data.frame(a = 1) %>% theme_htmlTable(caption = "potato") %>% condformat2html()
  out <- strsplit(x, "\n", fixed = TRUE)[[1]]
  expect_true(any(grepl("potato", out, fixed = TRUE)))
})
