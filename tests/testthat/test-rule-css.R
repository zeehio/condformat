test_that("rule_css works", {
  data(iris)
  x <- condformat(iris[c(1:10, 51:60, 101:110),])
  y <- x %>% rule_css(Species,
                      expression = ifelse(Species == "setosa",
                                          "red", "darkgreen"),
                      css_field = "color")
  out <- condformat2html(y)
  expect_match(out, "red")
  expect_match(out, "darkgreen")
})
