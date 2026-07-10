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

test_that("condformat2widget's ... argument is deprecated in favor of theme_htmlWidget", {
  expect_warning(
    w <- condformat2widget(condformat(head(iris)), number_of_entries = 2),
    "deprecated.*theme_htmlWidget"
  )
  expect_s3_class(w, "htmlwidget")
})

test_that("condformat2htmlcommon merges a css.cell theme argument with the rule-computed CSS", {
  # theme_htmlTable()'s public API doesn't accept css.cell (it isn't one of
  # htmlTable::htmlTable's own formals), so the merge path in
  # condformat2htmlcommon() is exercised here by injecting the theme args
  # directly, bypassing that validation.
  x <- condformat(head(iris)) |> rule_fill_discrete(Species)
  theme <- structure(list(htmlargs = list(css.cell = "font-style: italic;")),
                     class = c("theme_htmlTable", "condformat_theme"))
  x <- add_theme_to_condformat(x, theme)
  out <- condformat2html(x)
  out_lines <- strsplit(out, "\n", fixed = TRUE)[[1]]
  expect_true(any(grepl("font-style: italic;", out_lines, fixed = TRUE)))
  expect_true(any(grepl("background-color", out_lines, fixed = TRUE)))
})

test_that("cf_field_to_css.default warns for an unsupported cf_field class", {
  cf_field <- structure(list(), class = c("cf_field_bogus", "cf_field"))
  css_fields <- list()
  unlocked <- matrix(TRUE, nrow = 1, ncol = 1)
  expect_warning(
    result <- cf_field_to_css.default(cf_field, data.frame(a = 1), css_fields, unlocked),
    "cf key cf_field_bogus is not supported"
  )
  expect_equal(result[["css_fields"]], css_fields)
  expect_equal(result[["unlocked"]], unlocked)
})

test_that("knitr returns a paginated htmlwidget when paginate = TRUE", {
  data(iris)
  on.exit(knitr::opts_knit$set(rmarkdown.pandoc.to = NULL, out.format = NULL))
  knitr::opts_knit$set(rmarkdown.pandoc.to = "html", out.format = "html")
  out <- knitr::knit_print(condformat(head(iris)), paginate = TRUE)
  expect_s3_class(out, "htmlwidget")
})

