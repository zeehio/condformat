#' Fill column with sequential colour gradient
#'
#' Fills the background color of a column using a gradient based on
#' the values given by an expression
#'
#' The syntax in condformat rules has changed since v0.7. See [rule_fill_gradient_old]
#'
#' @family rule
#'
#' @param x A condformat object, typically created with [condformat()]
#' @param columns A character vector with column names to be coloured. Optionally
#'                [tidyselect::select_helpers()] can be used.
#' @param expression an expression to be evaluated with the data.
#'                   It should evaluate to a numeric vector,
#'                   that will be used to determine the colour gradient level.
#' @inheritParams scales::seq_gradient_pal
#' @param limits range of limits that the gradient should cover
#' @param na.value fill color for missing values
#' @param lockcells logical value determining if no further rules should be applied to the affected cells.
#'
#' @param ... Dots are used to transition from the old syntax [rule_fill_discrete_old] to the new one
#'
#' @return The condformat_tbl object, with the added formatting information
#' @examples
#' data(iris)
#' condformat(iris[c(1:5, 70:75, 120:125), ]) %>%
#'   rule_fill_gradient(Sepal.Length) %>%
#'   rule_fill_gradient(Species, expression=Sepal.Length - Sepal.Width)
#'
#' condformat(iris[c(1:5, 70:75, 120:125), ]) %>%
#'   rule_fill_gradient("Petal.Length") %>%
#'   rule_fill_gradient(starts_with("Sepal"), expression=Sepal.Length - Sepal.Width)
#'
#' @export
rule_fill_gradient <- function(x, columns, expression,
                               low = "#132B43", high = "#56B1F7",
                               space = "Lab",
                               na.value = "#7F7F7F",
                               limits = NA,
                               lockcells = FALSE, ...) {
  return(api_dispatcher(rule_fill_gradient_new, rule_fill_gradient_old))
}


#' Fill column with sequential colour gradient (deprecated)
#'
#' Fills the background color of a column using a gradient based on
#' the values given by an expression
#'
#' @param ... Comma separated list of unquoted column names.
#'            If \code{expression} is also given, then this list can use any of the
#'            \code{\link[dplyr]{select}} syntax possibilities.
#' @param expression an expression to be evaluated with the data.
#'                   It should evaluate to a numeric vector,
#'                   that will be used to determine the colour gradient level.
#' @inheritParams scales::seq_gradient_pal
#' @param limits range of limits that the gradient should cover
#' @param na.value fill color for missing values
#' @param lockcells logical value determining if no further rules should be applied to the affected cells.
#'
#' @return The condformat_tbl object, with the added formatting information
#' @examples
#' data(iris)
#' condformat(iris[c(1:5, 70:75, 120:125), ]) +
#'   rule_fill_gradient(Sepal.Length) +
#'   rule_fill_gradient(Species, expression=Sepal.Length - Sepal.Width)
#' @export
rule_fill_gradient_old <- function(...,
                                   expression,
                                   low = "#132B43", high = "#56B1F7",
                                   space = "Lab",
                                   na.value = "#7F7F7F",
                                   limits = NA,
                                   lockcells = FALSE) {
  columns <- lazyeval::lazy_dots(...) # D
  if (missing(expression)) {
    if (length(columns) > 1) {
      warning("rule_fill_gradient applied to multiple variables, using the first given variable as expression")
    }
    expression <- columns[[1]]
  } else {
    expression <- lazyeval::lazy(expression) # D
  }

  rule <- structure(list(columns = columns, expression = expression,
                         low = force(low),
                         high = force(high),
                         space = force(space),
                         na.value = force(na.value),
                         limits = force(limits),
                         lockcells = force(lockcells)),
                    class = c("condformat_rule", "rule_fill_gradient"))
  return(rule)
}

rule_fill_gradient_new <- function(x, columns, expression,
                                   low = "#132B43", high = "#56B1F7",
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
                         high = force(high),
                         space = force(space),
                         na.value = force(na.value),
                         limits = force(limits),
                         lockcells = force(lockcells)),
                    class = c("condformat_rule", "rule_fill_gradient"))
  x <- add_rule_to_condformat(x, rule)
  return(x)
}


#' Fill column with sequential colour gradient (deprecated)
#'
#' Fills the background color of a column using a gradient based on
#' the values given by an expression
#'
#' @param columns a character vector with the column names or a list with
#'                dplyr select helpers given as formulas or a combination of both
#' @param expression a formula to be evaluated with the data that will be used
#'                   to determine which cells are to be coloured. See the examples
#'                   to use it programmatically
#' @inheritParams scales::seq_gradient_pal
#' @inheritParams rule_fill_gradient
#' @export
#' @examples
#' data(iris)
#' condformat(iris[1:5,]) + rule_fill_gradient_(columns=c("Sepal.Length"))
#' ex1 <- condformat(iris[1:5,]) +
#'   rule_fill_gradient_("Species", expression=~Sepal.Length-Sepal.Width)
#' # Use it programmatically:
#' gradient_color_column1_minus_column2 <- function(x, column_to_paint, column1, column2) {
#'   condformat(x) +
#'     rule_fill_discrete_(column_to_paint,
#'      expression=~ uq(as.name(column1)) - uq(as.name(column2)))
#' }
#' ex2 <- gradient_color_column1_minus_column2(iris[1:5,], "Species", "Sepal.Length", "Sepal.Width")
#' stopifnot(ex1 == ex2)
rule_fill_gradient_ <- function(columns,
                                expression=~.,
                                low = "#132B43", high = "#56B1F7",
                                space = "Lab",
                                na.value = "#7F7F7F",
                                limits = NA,
                                lockcells = FALSE) {
  warning("This condformat syntax is deprecated. See ?rule_fill_gradient for more information")
  col_expr <- parse_columns_and_expression_(columns, expression)
  rule <- structure(list(columns = col_expr[["columns"]],
                         expression = col_expr[["expression"]],
                         low = force(low), high = force(high),
                         space = force(space), na.value = force(na.value),
                         limits = force(limits), lockcells = force(lockcells)),
                    class = c("condformat_rule", "rule_fill_gradient_"))
  return(rule)
}

applyrule.rule_fill_gradient <- function(rule, finalformat, xfiltered, xview, ...) {
  if (inherits(rule[["expression"]], "lazy")) {
    # Deprecated
    columns <- dplyr::select_vars_(colnames(xview), rule[["columns"]]) # D
    values_determining_color <- lazyeval::lazy_eval(rule[["expression"]], xfiltered) # D
    values_determining_color <- rep(values_determining_color, length.out = nrow(xfiltered))
    rule_fill_gradient_common(rule, finalformat, xview, columns, values_determining_color)
  } else {
    columns <- tidyselect::vars_select(colnames(xview), !!! rule[["columns"]])
    if (length(columns) == 0) {
      return(finalformat)
    }
    if (rlang::quo_is_missing(rule[["expression"]])) {
      if (length(columns) > 1) {
        warning("rule_fill_gradient applied to multiple columns, using column ",
                columns[1], " values as expression. In the future this behaviour will change,",
                " please use a explicit expression instead.",
                call. = FALSE)
      }
      rule[["expression"]] <- as.symbol(as.name(columns[1]))
    }
    values_determining_color <- rlang::eval_tidy(rule[["expression"]], data = xfiltered)
    values_determining_color <- rep(values_determining_color, length.out = nrow(xfiltered))
    rule_fill_gradient_common(rule, finalformat, xview, columns, values_determining_color)
  }
}

applyrule.rule_fill_gradient_ <- function(rule, finalformat, xfiltered, xview, ...) {
  # Deprecated
  columns <- dplyr::select_vars_(colnames(xview), rule[["columns"]]) # D
  values_determining_color <- lazyeval::f_eval(f = rule[["expression"]], data = xfiltered) # D
  values_determining_color <- rep(values_determining_color, length.out = nrow(xfiltered))
  rule_fill_gradient_common(rule, finalformat, xview, columns, values_determining_color)
}

#' @importFrom scales rescale
rule_fill_gradient_common <- function(rule, finalformat, xview,
                                      columns, values_determining_color) {
  if (identical(rule[["limits"]], NA)) {
    limits <- range(values_determining_color, na.rm = TRUE)
  } else {
    limits <- rule[["limits"]]
  }

  col_scale <- scales::seq_gradient_pal(low = rule[["low"]], high = rule[["high"]], space = rule[["space"]])

  values_rescaled <- scales::rescale(x = values_determining_color, from = limits)
  colours_for_values <- col_scale(values_rescaled)
  stopifnot(identical(length(colours_for_values), nrow(xview)))
  colours_for_values <- matrix(colours_for_values,
                               nrow = nrow(xview), ncol = ncol(xview), byrow = FALSE)

  finalformat <- fill_css_field_by_cols(finalformat, "background-color",
                                        colours_for_values, columns,
                                        xview, rule[["lockcells"]])
  return(finalformat)
}
