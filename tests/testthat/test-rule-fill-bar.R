test_that("rule_fill_bar works", {
  x <- data.frame(a = c(1,3,5)) %>%
    condformat() %>%
    rule_fill_bar(a)
  xv_cf <- get_xview_and_cf_fields(x)
  css_fields <- render_cf_fields_to_css_fields(xv_cf$cf_fields, xv_cf$xview)
  expect_equal(css_fields$`background-size`[2,1], "50% 100%")
})

test_that("rule_fill_bar computes border, background-color and background-image", {
  x <- data.frame(a = c(1, NA, 5)) %>%
    condformat() %>%
    rule_fill_bar(a)
  xv_cf <- get_xview_and_cf_fields(x)
  css_fields <- render_cf_fields_to_css_fields(xv_cf$cf_fields, xv_cf$xview)

  # NA cells get na.value as background-color, and no border/gradient
  expect_equal(css_fields$`background-color`[2, 1], "#BEBEBE")
  expect_true(is.na(css_fields$border[2, 1]))
  expect_true(is.na(css_fields$`background-image`[2, 1]))

  # non-NA cells get the default background colour and a gradient
  expect_equal(css_fields$`background-color`[1, 1], "#FFFFFF")
  expect_equal(css_fields$border[1, 1], "1px solid black")
  expect_equal(
    css_fields$`background-image`[1, 1],
    "linear-gradient(to right, rgba(0, 100, 0, 1) 0%, rgba(255, 255, 255, 1) 100%)")
})

test_that("rule_fill_bar respects custom low/high/background/na.value colours", {
  x <- data.frame(a = c(1, NA, 5)) %>%
    condformat() %>%
    rule_fill_bar(a, low = "red", high = "blue", background = "black", na.value = "yellow")
  xv_cf <- get_xview_and_cf_fields(x)
  css_fields <- render_cf_fields_to_css_fields(xv_cf$cf_fields, xv_cf$xview)

  expect_equal(css_fields$`background-color`[2, 1], "#FFFF00")
  expect_equal(css_fields$`background-color`[1, 1], "#000000")
  expect_equal(
    css_fields$`background-image`[1, 1],
    "linear-gradient(to right, rgba(255, 0, 0, 1) 0%, rgba(0, 0, 255, 1) 100%)")
})

test_that("rule_fill_bar respects explicit limits", {
  x <- data.frame(a = c(1, 3, 5)) %>%
    condformat() %>%
    rule_fill_bar(a, limits = c(0, 10))
  xv_cf <- get_xview_and_cf_fields(x)
  css_fields <- render_cf_fields_to_css_fields(xv_cf$cf_fields, xv_cf$xview)
  expect_equal(css_fields$`background-size`[1, 1], "10% 100%")
  expect_equal(css_fields$`background-size`[2, 1], "30% 100%")
  expect_equal(css_fields$`background-size`[3, 1], "50% 100%")
})

test_that("rule_fill_bar fills in missing limits with the data range", {
  x <- data.frame(a = c(1, 3, 5)) %>%
    condformat() %>%
    rule_fill_bar(a, limits = c(NA, 10))
  xv_cf <- get_xview_and_cf_fields(x)
  css_fields <- render_cf_fields_to_css_fields(xv_cf$cf_fields, xv_cf$xview)
  expect_equal(css_fields$`background-size`[1, 1], "0% 100%")
  expect_equal(css_fields$`background-size`[2, 1], "22% 100%")
  expect_equal(css_fields$`background-size`[3, 1], "44% 100%")
})

test_that("rule_fill_bar lockcells prevents further CSS rules from applying", {
  y <- data.frame(a = c(1, 3, 5)) %>%
    condformat() %>%
    rule_fill_bar(a, background = "red", lockcells = TRUE) %>%
    rule_fill_bar(a, background = "blue")
  xv_cf <- get_xview_and_cf_fields(y)
  css_fields <- render_cf_fields_to_css_fields(xv_cf$cf_fields, xv_cf$xview)
  expect_equal(css_fields$`background-color`[1, 1], "#FF0000")
})

test_that("rule_fill_bar warns when applied to multiple columns without an explicit expression", {
  expect_warning(
    condformat2html(
      rule_fill_bar(condformat(data.frame(a = 1:3, b = 4:6)), c("a", "b"))),
    "multiple columns")
})

test_that("rule_fill_bar is not supported in LaTeX and warns", {
  expect_warning(
    condformat2latex(data.frame(a = c(1, 3, 5)) %>% condformat() %>% rule_fill_bar(a)),
    "not supported by condformat in LaTeX")
})

test_that("rule_fill_bar renders in HTML", {
  out <- data.frame(a = c(1, 3, 5)) %>%
    condformat() %>%
    rule_fill_bar(a) %>%
    condformat2html()
  expect_match(out, "linear-gradient")
})

test_that("rule_fill_bar gtable renders a gradient bar and a plain fill for NA cells", {
  # Values are chosen so the rescaled bar width is never exactly 0% or 100%,
  # since colorRampPalette(..., space = "Lab")(0) errors on this R version.
  cfg_before <- data.frame(a = c(2, NA, 8)) %>%
    condformat() %>%
    condformat2grob(draw = FALSE)
  cfg <- data.frame(a = c(2, NA, 8)) %>%
    condformat() %>%
    rule_fill_bar(a, limits = c(0, 10)) %>%
    condformat2grob(draw = FALSE)

  # one rect grob is added per non-NA cell (rows 1 and 3)
  expect_equal(nrow(cfg$layout), nrow(cfg_before$layout) + 2)

  ind_na <- find_cell(cfg, 3, 2, name = "core-bg")
  expect_equal(cfg$grobs[ind_na][[1]][["gp"]][["fill"]], "#BEBEBE")
})

test_that("rule_fill_bar gtable does not crash when a value rescales to exactly 0%", {
  # a=1 rescales to exactly 0% under the default (data range) limits; this
  # used to error inside colorRampPalette(..., space = "Lab")(0)
  cfg_before <- data.frame(a = c(1, 3, 5)) %>%
    condformat() %>%
    condformat2grob(draw = FALSE)
  cfg <- data.frame(a = c(1, 3, 5)) %>%
    condformat() %>%
    rule_fill_bar(a) %>%
    condformat2grob(draw = FALSE)
  # one rect grob is added per cell, including the one that rescales to 0%
  expect_equal(nrow(cfg$layout), nrow(cfg_before$layout) + 3)
})

test_that("rule_fill_bar na.value does not crash when only some columns are targeted", {
  x <- data.frame(a = c(1, NA, 5), b = c(10, 20, 30)) %>%
    condformat() %>%
    rule_fill_bar(a)
  xv_cf <- get_xview_and_cf_fields(x)
  css_fields <- render_cf_fields_to_css_fields(xv_cf$cf_fields, xv_cf$xview)
  expect_equal(css_fields$`background-color`[2, 1], "#BEBEBE")
  expect_true(is.na(css_fields$`background-color`[1, 2]))
})

test_that("rule_fill_bar lockcells prevents further CSS rules from overwriting na.value", {
  y <- data.frame(a = c(1, NA, 5)) %>%
    condformat() %>%
    rule_fill_bar(a, na.value = "red", lockcells = TRUE) %>%
    rule_fill_bar(a, na.value = "blue")
  xv_cf <- get_xview_and_cf_fields(y)
  css_fields <- render_cf_fields_to_css_fields(xv_cf$cf_fields, xv_cf$xview)
  expect_equal(css_fields$`background-color`[2, 1], "#FF0000")
})

test_that("rule_fill_bar lockcells prevents further gtable rules from applying", {
  # each non-NA cell gets its own rect grob added on top of "core-bg" (which
  # is left untouched), so a locked-out second rule must add zero new grobs
  cfg_rule1_only <- data.frame(a = c(1, 3, 5)) %>%
    condformat() %>%
    rule_fill_bar(a, background = "red", lockcells = TRUE) %>%
    condformat2grob(draw = FALSE)
  cfg_both <- data.frame(a = c(1, 3, 5)) %>%
    condformat() %>%
    rule_fill_bar(a, background = "red", lockcells = TRUE) %>%
    rule_fill_bar(a, background = "blue") %>%
    condformat2grob(draw = FALSE)
  expect_equal(nrow(cfg_both$layout), nrow(cfg_rule1_only$layout))
})

test_that("rule_fill_bar lockcells prevents further gtable rules from overwriting na.value", {
  cfg <- data.frame(a = c(1, NA, 5)) %>%
    condformat() %>%
    rule_fill_bar(a, na.value = "red", lockcells = TRUE) %>%
    rule_fill_bar(a, na.value = "blue") %>%
    condformat2grob(draw = FALSE)
  ind <- find_cell(cfg, 3, 2, name = "core-bg")
  expect_equal(cfg$grobs[ind][[1]][["gp"]][["fill"]], "#FF0000")
})
