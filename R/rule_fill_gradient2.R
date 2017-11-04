#' Fill column with sequential color gradient
#'
#' Fills the background color of a column using a gradient based on
#' the values given by an expression
#'
#' The syntax in condformat rules has changed since v0.7. See [rule_fill_gradient_old()]
#'
#' @family rule
#'
#' @param x A condformat object, typically created with [condformat()]
#' @param columns A character vector with column names to be colored. Optionally
#'                [tidyselect::select_helpers()] can be used.
#' @param expression an expression to be evaluated with the data.
#'                   It should evaluate to a logical or an integer vector,
#'                   that will be used to determine which cells are to be colored.
#' @inheritParams scales::div_gradient_pal
#' @param midpoint the value used for the middle color (the median by default)
#' @param limits range of limits that the gradient should cover
#' @param na.value fill color for missing values
#' @param lockcells logical value determining if no further rules should be applied to the affected cells.
#'
#' @param ... Dots are used to transition from the old syntax [rule_fill_discrete_old()] to the new one
#'
#' @return The condformat_tbl object, with the added formatting information
#' @examples
#' data(iris)
#' condformat(iris[c(1:5, 70:75, 120:125), ]) %>%
#'  rule_fill_gradient2(Sepal.Length) %>%
#'  rule_fill_gradient2(Species, expression=Sepal.Length - Sepal.Width)
#'
#' condformat(iris[c(1:5, 70:75, 120:125), ]) %>%
#'   rule_fill_gradient2("Petal.Length") %>%
#'   rule_fill_gradient2(starts_with("Sepal"), expression=Sepal.Length - Sepal.Width)
#'
#' @export
rule_fill_gradient2 <- function(x, columns, expression,
                                low = scales::muted("red"),
                                mid = "white",
                                high = scales::muted("blue"),
                                midpoint = NA,
                                space = "Lab",
                                na.value = "#7F7F7F",
                                limits = NA,
                                lockcells = FALSE, ...) {
  return(api_dispatcher(rule_fill_gradient2_new, rule_fill_gradient2_old))
}

rule_fill_gradient2_new <- function(x, columns, expression,
                                    low = scales::muted("red"),
                                    mid = "white",
                                    high = scales::muted("blue"),
                                    midpoint = NA,
                                    space = "Lab",
                                    na.value = "#7F7F7F",
                                    limits = NA,
                                    lockcells = FALSE) {
  if (!inherits(x, "condformat_tbl")) {
    x <- condformat(x)
  }
  columnsquo <- rlang::enquo(columns)
  helpers <- tidyselect::vars_select_helpers
  columnsquo_bur <- rlang::env_bury(columnsquo, !!! helpers)
  expr <- rlang::enquo(expression)
  rule <- structure(list(columns = columnsquo_bur,
                         expression = expr,
                         low = force(low),
                         mid = force(mid), high = force(high),
                         midpoint = force(midpoint), space = force(space),
                         na.value = force(na.value),
                         limits = force(limits), lockcells = force(lockcells)),
                    class = c("condformat_rule", "rule_fill_gradient2"))
  x <- add_rule_to_condformat(x, rule)
  return(x)
}


#' Fill column with divergent color gradient (deprecated)
#'
#' Fills the background color of a column using a three colors gradient based on
#' the values of an expression
#'
#' @param ... Comma separated list of unquoted column names.
#'            If \code{expression} is also given, then this list can use any of the
#'            \code{\link[dplyr]{select}} syntax possibilities.
#' @param expression an expression to be evaluated with the data.
#'                   It should evaluate to a numeric vector,
#'                   that will be used to determine the color gradient level.
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
rule_fill_gradient2_old <- function(...,
                                expression,
                                low = scales::muted("red"), mid="white", high = scales::muted("blue"),
                                midpoint = NA,
                                space = "Lab",
                                na.value = "#7F7F7F",
                                limits = NA,
                                lockcells = FALSE) {
  # Deprecated
  columns <- lazyeval::lazy_dots(...) # D
  if (missing(expression)) {
    if (length(columns) > 1) {
      warning("rule_fill_gradient2 applied to multiple variables, using the first given variable as expression")
    }
    expression <- columns[[1]]
  } else {
    expression <- lazyeval::lazy(expression) # D
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

#' Fill column with divergent color gradient (deprecated)
#'
#' Fills the background color of a column using a three colors gradient based on
#' the values of an expression
#'
#' @param columns a character vector with the column names or a list with
#'                dplyr select helpers given as formulas or a combination of both
#' @param expression a formula to be evaluated with the data that will be used
#'                   to determine which cells are to be colored. See the examples
#'                   to use it programmatically
#' @inheritParams scales::div_gradient_pal
#' @inheritParams rule_fill_gradient2
#' @export
#' @examples
#' data(iris)
#' condformat(iris[1:10,]) + rule_fill_gradient2_(columns=c("Sepal.Length"))
#' condformat(iris[1:10,]) + rule_fill_gradient2_("Species",
#'    expression= ~Sepal.Length-Sepal.Width)
#'
#' # Use it programmatically
#' color_column <- function(x, column) {
#'   condformat(x) +
#'     rule_fill_gradient2_(column, expression=~ uq(as.name(column)))
#' }
#' color_column(iris[c(1,51,101),], "Sepal.Length")
#'
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
  warning("This condformat syntax is deprecated. See ?rule_fill_gradient2 for more information")
  col_expr <- parse_columns_and_expression_(columns, expression)
  rule <- structure(list(columns = col_expr[["columns"]],
                         expression = col_expr[["expression"]],
                         low = force(low),
                         mid = force(mid), high = force(high),
                         midpoint = force(midpoint), space = force(space),
                         na.value = force(na.value),
                         limits = force(limits), lockcells = force(lockcells)),
                    class = c("condformat_rule", "rule_fill_gradient2_"))
  return(rule)
}

applyrule.rule_fill_gradient2 <- function(rule, finalformat, xfiltered, xview, ...) {
  if (inherits(rule$expression, "lazy")) {
    # Deprecated
    columns <- dplyr::select_vars_(colnames(xview), rule$columns) # D
    values_determining_color <- lazyeval::lazy_eval(rule$expression, xfiltered) # D
    values_determining_color <- rep(values_determining_color, length.out = nrow(xfiltered))
    rule_fill_gradient2_common(rule, finalformat, xview, columns, values_determining_color)
  } else {
    columns <- tidyselect::vars_select(colnames(xview), !!! rule$columns)
    if (length(columns) == 0) {
      return(finalformat)
    }
    if (rlang::quo_is_missing(rule$expression)) {
      if (length(columns) > 1) {
        warning("rule_fill_gradient2 applied to multiple columns, using column ",
                columns[1], " values as expression. In the future this behaviour will change,",
                " please use a explicit expression instead.",
                call. = FALSE)
      }
      rule$expression <- as.symbol(as.name(columns[1]))
    }
    values_determining_color <- rlang::eval_tidy(rule$expression, data = xfiltered)
    values_determining_color <- rep(values_determining_color, length.out = nrow(xfiltered))
    rule_fill_gradient2_common(rule, finalformat, xview, columns, values_determining_color)
  }
}

applyrule.rule_fill_gradient2_ <- function(rule, finalformat, xfiltered, xview, ...) {
  # Deprecated
  columns <- dplyr::select_vars_(colnames(xview), rule$columns) # D
  values_determining_color <- lazyeval::f_eval(f = rule$expression, data = xfiltered) # D
  values_determining_color <- rep(values_determining_color, length.out = nrow(xfiltered))
  rule_fill_gradient2_common(rule, finalformat, xview, columns, values_determining_color)
}

#' @importFrom scales rescale_mid
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

  colors_for_values <- col_scale(values_rescaled)
  stopifnot(identical(length(colors_for_values), nrow(xview)))
  colors_for_values <- matrix(colors_for_values,
                              nrow = nrow(xview), ncol = ncol(xview), byrow = FALSE)

  finalformat <- fill_css_field_by_cols(finalformat, "background-color",
                                        colors_for_values, columns, xview,
                                        rule$lockcells)
  return(finalformat)
}
