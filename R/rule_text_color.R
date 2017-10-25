#' Give a color to the text according to some expression
#' @family rule
#' @inheritParams rule_fill_discrete
#' @param expression Condition that evaluates to color names for the rows where text should be colored
#'
#' @param na.color Color for missing values
#' @examples
#' data(iris)
#' condformat(iris[c(1:5, 51:55, 101:105),]) %>%
#'   rule_text_color(Species, expression = ifelse(Species == "setosa", "blue", ""))
#' @export
rule_text_color <- function(x, columns, expression,
                            na.color = "",
                            lockcells = FALSE) {
  columnsquo <- rlang::enquo(columns)
  helpers <- tidyselect::vars_select_helpers
  columnsquo_bur <- rlang::env_bury(columnsquo, !!! helpers)

  expr <- rlang::enquo(expression)

  rule <- structure(list(columns = columnsquo_bur,
                         expression = expr,
                         na.value = force(na.color),
                         lockcells = force(lockcells)),
                    class = c("condformat_rule", "rule_text_color"))

  x <- add_rule_to_condformat(x, rule)
  return(x)
}

applyrule.rule_text_color <- function(rule, finalformat, xfiltered, xview, ...) {
  columns <- tidyselect::vars_select(colnames(xview), !!! rule$columns)
  if (length(columns) == 0) {
    return(finalformat)
  }
  if (rlang::quo_is_missing(rule$expression)) {
    if (length(columns) > 1) {
      warning("rule_css applied to multiple columns, using column ",
              columns[1], " values as expression. In the future this behaviour will change,",
              "please use a explicit expression instead.",
              call. = FALSE)
    }
    rule$expression <- as.symbol(as.name(columns[1]))
  }
  colors <- rlang::eval_tidy(rule$expression, data = xfiltered)
  stopifnot(identical(length(colors), nrow(xview)))
  # Recycle css values to fit all the columns:
  colors_mat <- matrix(colors, nrow = nrow(xview),
                       ncol = ncol(xview), byrow = FALSE)
  colors_mat[is.na(colors_mat)] <- rule$na.value
  finalformat <- fill_css_field_by_cols(finalformat,
                                        "color", colors_mat,
                                        columns, xview, rule$lockcells)
  return(finalformat)
}
