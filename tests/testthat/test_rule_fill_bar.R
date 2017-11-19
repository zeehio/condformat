# Tests:
context("rule_fill_bar")

test_that("rule_fill_bar works", {
  x <- data.frame(a = c(1,3,5)) %>%
    condformat() %>%
    rule_fill_bar(a)
  xv_cf <- get_xview_and_cf_fields(x)
  css_fields <- render_cf_fields_to_css_fields(xv_cf$cf_fields, xv_cf$xview)
  expect_equal(css_fields$`background-size`[2,1], "50% 100%")
})
