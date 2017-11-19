# Tests:
context("rule_fill_bar")

test_that("rule_fill_bar works", {
  x <- data.frame(a = c(1,3,5)) %>%
    condformat() %>%
    rule_fill_bar(a)
  xv_cf <- get_xview_and_cf_fields(x)
  expect_equal(as.data.frame(xv_cf$xview), x)
  css_fields <- render_cf_fields_to_css_fields(xv_cf$cf_fields, xv_cf$xview)
  expect_cf_fields <- list(structure(
    list(col_low = structure(c(0L, 100L, 0L), .Dim = c(3L, 1L),
                             .Dimnames = list(c("red", "green", "blue"), NULL)),
         col_high = structure(c(255L, 255L, 255L), .Dim = c(3L, 1L),
                              .Dimnames = list(c("red", "green", "blue"), NULL)),
         col_background = structure(c(255L, 255L, 255L), .Dim = c(3L, 1L),
                                    .Dimnames = list(c("red", "green", "blue"), NULL)),
         col_na_value = structure(c(190L, 190L, 190L), .Dim = c(3L, 1L),
                                  .Dimnames = list(c("red", "green", "blue"), NULL)),
         border = TRUE,
         bar_width_percent = structure(c(0, 0.5, 1), .Dim = c(3L, 1L),
                                       .Dimnames = list(NULL, "a")),
         pbar_is_na = structure(c(FALSE, FALSE, FALSE), .Dim = c(3L, 1L),
                                .Dimnames = list(NULL, "a")),
         lock_cells = FALSE),
    .Names = c("col_low", "col_high", "col_background", "col_na_value",
               "border", "bar_width_percent", "pbar_is_na", "lock_cells"),
    class = c("cf_field_rule_bar_gradient", "cf_field")))
  expect_equal(xv_cf$cf_fields, expect_cf_fields)

  expected_css_fields <- structure(list(border = structure(c("1px solid black", "1px solid black",
                                                             "1px solid black"),
                                                           .Dim = c(3L, 1L)),
                                        `background-repeat` = structure(c("no-repeat","no-repeat", "no-repeat"),
                                                                        .Dim = c(3L, 1L)),
                                        `background-color` = structure(c("#FFFFFF", "#FFFFFF", "#FFFFFF"),
                                                                       .Dim = c(3L, 1L)),
                                        `background-image` = structure(c("linear-gradient(to right, rgba(0, 100, 0, 1) 0%, rgba(255, 255, 255, 1) 100%)",
                                                                         "linear-gradient(to right, rgba(0, 100, 0, 1) 0%, rgba(255, 255, 255, 1) 100%)",
                                                                         "linear-gradient(to right, rgba(0, 100, 0, 1) 0%, rgba(255, 255, 255, 1) 100%)"),
                                                                       .Dim = c(3L, 1L)),
                                        `background-size` = structure(c("0% 100%", "50% 100%", "100% 100%"),
                                                                      .Dim = c(3L, 1L))),
                                   .Names = c("border", "background-repeat", "background-color", "background-image", "background-size"))
  expect_equal(css_fields, expected_css_fields)
})

test_that("rule_text_bold works for LaTeX output", {
  x <- condformat(data.frame(a = "potato")) %>%
    rule_text_bold("a", expression = a == "potato") %>%
    condformat2latex() %>%
    strsplit(split = "\n", fixed = TRUE)
  expect_true(any(grepl(pattern = "\\textbf{potato}", x[[1]], fixed = TRUE)))
})
