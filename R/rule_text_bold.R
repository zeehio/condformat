#' Use bold text if a condition is met
#' @family rule
#' @inheritParams rule_fill_discrete
#' @param expression Condition that evaluates to `TRUE` for the rows where bold text should be applied.
#'
#' @param na.bold If `TRUE`, make missing values bold.
#' @examples
#' data(iris)
#' condformat(iris[c(1:5, 51:55, 101:105),]) %>%
#'   rule_text_bold(Species, expression = Species == "setosa")
#' @export
rule_text_bold <- function(x, columns, expression,
                           na.bold = FALSE,
                           lockcells = FALSE) {
  columnsquo <- rlang::enquo(columns)
  helpers <- tidyselect::vars_select_helpers
  columnsquo_bur <- rlang::env_bury(columnsquo, !!! helpers)

  expr <- rlang::enquo(expression)

  rule <- structure(list(columns = columnsquo_bur,
                         expression = expr,
                         na.value = force(na.bold),
                         lockcells = force(lockcells)),
                    class = c("condformat_rule", "rule_text_bold"))

  x <- add_rule_to_condformat(x, rule)
  return(x)
}

applyrule.rule_text_bold <- function(rule, finalformat, xfiltered, xview, ...) {
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
  bold_or_not <- rlang::eval_tidy(rule$expression, data = xfiltered)
  stopifnot(identical(length(bold_or_not), nrow(xview)))
  # Recycle css values to fit all the columns:
  bold_or_not_mat_l <- matrix(bold_or_not, nrow = nrow(xview),
                              ncol = ncol(xview), byrow = FALSE)
  bold_or_not_mat <- matrix("normal", nrow = nrow(xview), ncol = ncol(xview))
  bold_or_not_mat[bold_or_not_mat_l] <- "bold"
  bold_or_not_mat[is.na(bold_or_not_mat_l)] <- rule$na.value

  finalformat <- fill_css_field_by_cols(finalformat,
                                        "font-weight", bold_or_not_mat,
                                        columns, xview, rule$lockcells)
  return(finalformat)
}
