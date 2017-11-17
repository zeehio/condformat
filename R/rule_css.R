#' Apply a CSS style property as a conditional formatting rule
#' @family rule
#' @inheritParams rule_fill_discrete
#' @param expression This expression should evaluate to an array of the values
#'
#' @param css_field CSS style property name (e.g. `"color"`)
#' @param na.value CSS property value to be used in missing values (e.g. `"grey"`)
#' @examples
#' data(iris)
#' condformat(iris[c(1:5, 51:55, 101:105),]) %>%
#'   rule_css(Species, expression = ifelse(Species == "setosa", "red", "darkgreen"),
#'            css_field = "color")
#' @export
rule_css <- function(x, columns, expression,
                     css_field,
                     na.value = "",
                     lockcells = FALSE) {
  columnsquo <- rlang::enquo(columns)
  helpers <- tidyselect::vars_select_helpers
  columnsquo_bur <- rlang::env_bury(columnsquo, !!! helpers)

  expr <- rlang::enquo(expression)

  rule <- structure(list(columns = columnsquo_bur,
                         expression = expr,
                         css_field = force(css_field),
                         na.value = force(na.value),
                         lockcells = force(lockcells)),
                    class = c("condformat_rule", "rule_css"))

  x <- add_rule_to_condformat(x, rule)
  return(x)
}

applyrule.rule_css <- function(rule, finalformat, xfiltered, xview, ...) {
  columns <- tidyselect::vars_select(colnames(xview), !!! rule[["columns"]])
  if (length(columns) == 0) {
    return(finalformat)
  }
  if (rlang::quo_is_missing(rule[["expression"]])) {
    if (length(columns) > 1) {
      warning("rule_css applied to multiple columns, using column ",
              columns[1], " values as expression. In the future this behaviour will change,",
              "please use a explicit expression instead.",
              call. = FALSE)
    }
    rule[["expression"]] <- as.symbol(as.name(columns[1]))
  }
  css_values <- rlang::eval_tidy(rule[["expression"]], data = xfiltered)
  stopifnot(identical(length(css_values), nrow(xview)))
  # Recycle css values to fit all the columns:
  colours_for_values <- matrix(css_values,
                               nrow = nrow(xview), ncol = ncol(xview), byrow = FALSE)
  finalformat <- fill_css_field_by_cols(finalformat,
                                        rule[["css_field"]], colours_for_values,
                                        columns, xview, rule[["lockcells"]])
  return(finalformat)
}
