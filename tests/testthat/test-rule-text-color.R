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

test_that("rule_text_color lockcells prevents further LaTeX rules from applying", {
  out <- condformat(data.frame(a = "potato")) %>%
    rule_text_color("a", expression = "red", lockcells = TRUE) %>%
    rule_text_color("a", expression = "blue") %>%
    condformat2latex()
  expect_true(grepl("RGB]{255,0,0}", out, fixed = TRUE))
  expect_false(grepl("RGB]{0,0,255}", out, fixed = TRUE))
})

test_that("rule_text_color lockcells prevents further gtable rules from applying", {
  cfg <- data.frame(a = "potato") %>%
    condformat() %>%
    rule_text_color("a", expression = "red", lockcells = TRUE) %>%
    rule_text_color("a", expression = "blue") %>%
    condformat2grob(draw = FALSE)
  ind <- find_cell(cfg, 2, 2, name = "core-fg")
  expect_equal(cfg$grobs[ind][[1]][["gp"]][["col"]], "red")
})
