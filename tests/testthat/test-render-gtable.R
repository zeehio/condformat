test_that("basic condformat works", {
  skip_if_not_installed("vdiffr")
  cf <- condformat(iris[c(1:5,70:75, 120:125),])
  cfg <- condformat2grob(cf, draw = FALSE)
  expect_equal(nrow(cfg), 18)
  expect_equal(length(cfg), 216)
  vdiffr::expect_doppelganger(title = "Basic condformat image", cfg)
})

test_that("condformat2grob omits the rowname offset when rows = NULL", {
  cfg <- data.frame(a = "Dog") %>%
    condformat() %>%
    theme_grob(rows = NULL) %>%
    rule_fill_discrete("a", colours = c("Dog" = "#FF0000")) %>%
    condformat2grob(draw = FALSE)
  ind <- find_cell(cfg, 2, 1, name = "core-bg")
  expect_equal(cfg$grobs[ind][[1]][["gp"]][["fill"]], "#FF0000")
})

test_that("condformat2grob combines multiple rules on the same cell", {
  cfg <- data.frame(a = "Dog") %>%
    condformat() %>%
    rule_fill_discrete("a", colours = c("Dog" = "#FF0000")) %>%
    rule_text_bold("a", expression = TRUE) %>%
    condformat2grob(draw = FALSE)
  ind_bg <- find_cell(cfg, 2, 2, name = "core-bg")
  ind_fg <- find_cell(cfg, 2, 2, name = "core-fg")
  expect_equal(cfg$grobs[ind_bg][[1]][["gp"]][["fill"]], "#FF0000")
  expect_equal(cfg$grobs[ind_fg][[1]][["gp"]][["fontface"]], "bold")
})

test_that("condformat2grob draws without error when draw = TRUE", {
  grDevices::pdf(file = tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  result <- condformat2grob(condformat(head(iris)))
  expect_s3_class(result, "gtable")
})

test_that("cf_field_to_gtable warns for rules with no gtable method", {
  expect_warning(
    condformat2grob(
      data.frame(a = "Dog") %>%
        condformat() %>%
        rule_css("a", expression = "red", css_field = "color"),
      draw = FALSE),
    "not supported by condformat in this output format")
})
