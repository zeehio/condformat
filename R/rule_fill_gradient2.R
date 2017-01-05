#' Fill column with divergent colour gradient
#'
#' Fills the background color of a column using a three colors gradient based on
#' the values of an expression
#'
#' @family rule
#'
#' @param ... Comma separated list of unquoted column names.
#'            If \code{expression} is also given, then this list can use any of the
#'            \code{\link[dplyr]{select}} syntax possibilities.
#' @param expression an expression to be evaluated with the data.
#'                   It should evaluate to a numeric vector,
#'                   that will be used to determine the colour gradient level.
#' @inheritParams scales::div_gradient_pal
#' @param midpoint the value used for the middle color (the median by default)
#' @param limits range of limits that the gradient should cover
#' @param na.value fill color for missing values
#' @param lockcells logical value determining if no further rules should be applied to the affected cells.
#'
#' @return The condformat_tbl object, with the added formatting information
#' @examples
#' data(iris)
#' condformat(iris[c(1:5, 70:75, 120:125), ]) +
#'  rule_fill_gradient2(Sepal.Length) +
#'  rule_fill_gradient2(Species, expression=Sepal.Length - Sepal.Width)
#' @export
rule_fill_gradient2 <- function(...,
                                expression,
                                low = scales::muted("red"), mid="white", high = scales::muted("blue"),
                                midpoint = NA,
                                space = "Lab",
                                na.value = "#7F7F7F",
                                limits = NA,
                                lockcells = FALSE) {
  columns <- lazyeval::lazy_dots(...)
  if (missing(expression)) {
    if (length(columns) > 1) {
      warning("rule_fill_gradient applied to multiple variables, using the first given variable as expression")
    }
    expression <- columns[[1]]
  } else {
    expression <- lazyeval::lazy(expression)
  }

  rule <- structure(list(columns = columns, expression = expression,
                         low = force(low),
                         mid = force(mid), high = force(high),
                         midpoint = force(midpoint), space = force(space),
                         na.value = force(na.value),
                         limits = force(limits), lockcells = force(lockcells)),
                    class = c("condformat_rule", "rule_fill_gradient2"))
  return(rule)
}

#' Fill column with divergent colour gradient (standard evaluation)
#'
#' Fills the background color of a column using a three colors gradient based on
#' the values of an expression
#'
#' @family rule
#' @param columns a character vector with the column names or a list with
#'                dplyr select helpers given as formulas or a combination of both
#' @param expression a formula to be evaluated with the data that will be used
#'                   to determine which cells are to be coloured. See the examples
#'                   to use it programmatically
#' @inheritParams scales::div_gradient_pal
#' @inheritParams rule_fill_gradient2
#' @export
#' @examples
#' data(iris)
#' condformat(iris[1:10,]) + rule_fill_gradient2_(columns=c("Sepal.Length"))
#' condformat(iris[1:10,]) + rule_fill_gradient2_("Species", expression= ~Sepal.Length-Sepal.Width)
rule_fill_gradient2_ <- function(columns,
                                 expression=~.,
                                 low = scales::muted("red"),
                                 mid="white",
                                 high = scales::muted("blue"),
                                 midpoint = NA,
                                 space = "Lab",
                                 na.value = "#7F7F7F",
                                 limits = NA,
                                 lockcells = FALSE) {
  if (is.character(expression)) {
    suggested_formula <- paste0("~ ", expression)
    warning(
      paste0("Deprecation: Using a character as expression is deprecated. ",
             "It will not be supported in the future. Please use a formula instead. ",
             "If you need help to build formulas programmatically, see the example ",
             "in the ?rule_fill_discrete_ help page. Suggestion: expression=", suggested_formula)) # FIXME
    expression <- stats::as.formula(suggested_formula)
  }
  if (lazyeval::f_rhs(expression) == as.name(".")) {
    if (length(columns) > 1) {
      warning("rule_fill_discrete_ applied to multiple variables, using the first given variable as expression")
    }
    lazyeval::f_rhs(expression) <- as.name(columns[1])
  }
  rule <- structure(list(columns = columns,
                         expression = expression,
                         low = force(low),
                         mid = force(mid), high = force(high),
                         midpoint = force(midpoint), space = force(space),
                         na.value = force(na.value),
                         limits = force(limits), lockcells = force(lockcells)),
                    class = c("condformat_rule", "rule_fill_gradient2_"))
  return(rule)
}

applyrule.rule_fill_gradient2 <- function(rule, finalformat, xfiltered, xview, ...) {
  columns <- dplyr::select_vars_(colnames(xview), rule$columns)
  values_determining_color <- lazyeval::lazy_eval(rule$expression, xfiltered)
  values_determining_color <- rep(values_determining_color, length.out = nrow(xfiltered))
  rule_fill_gradient2_common(rule, finalformat, xview, columns, values_determining_color)
}

applyrule.rule_fill_gradient2_ <- function(rule, finalformat, xfiltered, xview, ...) {
  columns <- dplyr::select_vars_(colnames(xview), rule$columns)
  values_determining_color <- lazyeval::f_eval(f = rule$expression, data = xfiltered)
  values_determining_color <- rep(values_determining_color, length.out = nrow(xfiltered))
  rule_fill_gradient2_common(rule, finalformat, xview, columns, values_determining_color)
}

rule_fill_gradient2_common <- function(rule, finalformat, xview,
                                      columns, values_determining_color) {
  if (identical(rule$limits, NA)) {
    limits <- range(values_determining_color, na.rm = TRUE)
  } else {
    limits <- rule$limits
  }

  if (is.na(rule$midpoint)) {
    midpoint <- stats::median(values_determining_color, na.rm = TRUE)
  } else {
    midpoint <- rule$midpoint
  }

  col_scale <- scales::div_gradient_pal(low = rule$low, mid = rule$mid, high = rule$high, space = rule$space)

  values_rescaled <- scales::rescale_mid(x = values_determining_color,
                                         from = limits, mid = midpoint)

  colours_for_values <- col_scale(values_rescaled)
  stopifnot(identical(length(colours_for_values), nrow(xview)))
  colours_for_values <- matrix(colours_for_values,
                               nrow = nrow(xview), ncol = ncol(xview), byrow = FALSE)

  finalformat <- fill_css_field_by_cols(finalformat, "background-color",
                                        colours_for_values, columns, xview,
                                        rule$lockcells)
  return(finalformat)
}
