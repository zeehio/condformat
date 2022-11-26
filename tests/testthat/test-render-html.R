test_that("render_cf_fields_to_css returns the expected", {
  css_fields <- list("background-color" = matrix(c("red", "red",
                                                   "blue", "green",
                                                   "yellow", "orange"),
                                                 nrow = 3, ncol = 2, byrow = TRUE),
                     "text-align" = matrix(c("left", "right",
                                             "left", "center",
                                             "right", "left"),
                                           nrow = 3, ncol = 2, byrow = TRUE))
  cf_fields <- list(structure(list(css_key = "background-color",
                                   css_values = structure(c("red", "blue", "yellow", "red", "green", "orange"),
                                                          .Dim = c(3L, 2L), .Dimnames = list(NULL, c("a", "b"))),
                                   lock_cells = FALSE),
                              class = c("cf_field_rule_fill_discrete",
                                        "cf_field_css", "cf_field")),
                    structure(list(css_key = "text-align",
                                   css_values = structure(c("left", "left", "right", "right", "center", "left"),
                                                          .Dim = c(3L, 2L), .Dimnames = list(NULL, c("a", "b"))),
                                   lock_cells = FALSE),
                              class = c("cf_field_rule_text_align",
                                        "cf_field_css", "cf_field")))
  xview <- data.frame(a = c(1,2,3), b = c(4,5,6))
  output <- merge_css(render_cf_fields_to_css_fields(cf_fields, xview), dim(xview))
  expected_output <- matrix(c("background-color: red; text-align: left", "background-color: red; text-align: right",
                              "background-color: blue; text-align: left", "background-color: green; text-align: center",
                              "background-color: yellow; text-align: right", "background-color: orange; text-align: left"),
                            nrow = 3, ncol = 2, byrow = TRUE)
  expect_equal(dim(output), c(3,2))
  expect_equal(output, expected_output)
})

test_that("knitr returns an HTML table", {
  data(iris)
  on.exit(knitr::opts_knit$set(rmarkdown.pandoc.to = NULL, out.format = NULL))
  knitr::opts_knit$set(rmarkdown.pandoc.to = "html", out.format = "html")
  out <- knitr::knit_print(condformat(head(iris)))
  expect_match(out, "^<table.*</table>$")
})

