test_that("rule_css works", {
  data(iris)
  x <- condformat(iris[c(1:10, 51:60, 101:110),])
  y <- x |> rule_css(Species,
                      expression = ifelse(Species == "setosa",
                                          "red", "darkgreen"),
                      css_field = "color")
  out <- condformat2html(y)
  expect_match(out, "red")
  expect_match(out, "darkgreen")
})

test_that("rule_css recycles a scalar expression to every row", {
  out <- condformat(iris[1:5, ]) |>
    rule_css(Species, expression = "solid", css_field = "border-style") |>
    condformat2html()
  expect_equal(lengths(regmatches(out, gregexpr("border-style: solid", out))), 5)
})

test_that(".col lets one rule_css call style several columns by their own values (#19)", {
  x <- data.frame(a = c(1, 5), b = c(5, 1))
  out_col <- x |>
    condformat() |>
    rule_css(c(a, b), ifelse(.col > 3, "solid", "dashed"), css_field = "border-style") |>
    condformat2html()
  out_chained <- x |>
    condformat() |>
    rule_css(a, ifelse(a > 3, "solid", "dashed"), css_field = "border-style") |>
    rule_css(b, ifelse(b > 3, "solid", "dashed"), css_field = "border-style") |>
    condformat2html()
  expect_equal(out_col, out_chained)
})
