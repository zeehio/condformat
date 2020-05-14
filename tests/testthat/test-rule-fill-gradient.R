test_that("rule_fill_gradient works in the limits", {
  data(iris)
  x <- condformat(iris[c(1,51,101),])
  y <- x %>% rule_fill_gradient(Sepal.Length, low = "#FF0000", high = "#00FF00")
  out <- condformat2html(y)
  expect_match(out, "^<table.*</table>$")
  expect_equal(
    length(sapply(strsplit(as.character(out), "\n", fixed = TRUE),
                  function(line) grep("(#FF0000.*5.1)", line))),
    1)
  expect_equal(
    length(sapply(strsplit(as.character(out), "\n", fixed = TRUE),
                  function(line) grep("(#00FF00.*7.0)", line))),
    1)
})
test_that("rule_fill_gradient2 works", {
  data(iris)
  x <- condformat(iris[c(1, 51, 101),])
  y <- x %>% rule_fill_gradient2(Sepal.Length)
  out <- condformat2html(y)
  y2 <- x %>% rule_fill_gradient2("Sepal.Length")
  out2 <- condformat2html(y2)
  expect_match(out, "^<table.*</table>$")
  expect_equal(out, out2)
})
