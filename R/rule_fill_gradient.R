#' Fill column with sequential colour gradient
#'
#' Fills the background color of a column using a gradient based on
#' the values given by an expression
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

#' @importFrom scales rescale
rule_to_cf_field.rule_fill_gradient <- function(rule, xfiltered, xview, ...) {
  columns <- tidyselect::vars_select(colnames(xview), !!! rule[["columns"]])
  if (length(columns) == 0) {
    return(NULL)
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

  if (identical(rule[["limits"]], NA)) {
    limits <- range(values_determining_color, na.rm = TRUE)
  } else {
    limits <- rule[["limits"]]
  }

  col_scale <- scales::seq_gradient_pal(low = rule[["low"]], high = rule[["high"]], space = rule[["space"]])

  values_rescaled <- scales::rescale(x = values_determining_color, from = limits)
  colours_for_values <- col_scale(values_rescaled)
  stopifnot(identical(length(colours_for_values), nrow(xview)))
  colours_for_values_mat <- matrix(NA,
                                   nrow = nrow(xview), ncol = ncol(xview),
                                   byrow = FALSE)
  colnames(colours_for_values_mat) <- colnames(xview)
  colours_for_values_mat[, columns] <- colours_for_values
  cf_field <- structure(list(css_key = "background-color",
                             css_values = colours_for_values_mat,
                             lock_cells = rule[["lockcells"]]),
                        class = c("cf_field_rule_fill_solid",
                                  "cf_field_css", "cf_field"))
  return(cf_field)
}
