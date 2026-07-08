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

test_that("rule_css recycles a scalar expression to every row", {
  out <- condformat(iris[1:5, ]) %>%
    rule_css(Species, expression = "solid", css_field = "border-style") %>%
    condformat2html()
  expect_equal(lengths(regmatches(out, gregexpr("border-style: solid", out))), 5)
})
