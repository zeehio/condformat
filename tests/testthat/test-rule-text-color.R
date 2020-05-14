test_that("rule_text_color works", {
  data(iris)
  x <- condformat(iris[c(1:10, 51:60, 101:110),])
  y <- x %>% rule_text_color(Species,
                             expression = ifelse(Species == "setosa", "blue", ""))
  out <- condformat2html(y)
  expect_match(out, "color: blue.*setosa")
  out <- condformat2latex(y)
  expect_true(grepl(pattern = "\\textcolor[RGB]{0,0,255}{setosa}", out, fixed = TRUE))
  expect_false(grepl(pattern = "\\textcolor[RGB]{0,0,255}{versicolor}", out, fixed = TRUE))
})
