#' Fill column with a bar of a length proportional to a value
#'
#' Fills the background of a column cell using a bar proportional to the value
#' of the cell
#'
#' @family rule
#'
#' @param x A condformat object, typically created with [condformat()]
#' @param columns A character vector with column names to be coloured. Optionally
#'                [tidyselect::select_helpers()] can be used.
#' @param expression an expression to be evaluated with the data.
#'                   It should evaluate to a numeric vector,
#'                   that will be used to determine the colour gradient level.
#' @param low Colour for the beginning of the bar
#' @param high Colour for the end of the bar
#' @param background Background colour for the cell
#' @param na.value Colour for missing values
#' @param limits range of limits that the gradient should cover
#' @param lockcells logical value determining if no further rules should be applied to the affected cells.
#'
#' @return The condformat_tbl object, with the added formatting information
#' @examples
#' data(iris)
#' condformat(iris[c(1:5, 70:75, 120:125), ]) %>% rule_fill_bar("Sepal.Length")
#' @export
rule_fill_bar <- function(x, columns, expression,
                          low = "darkgreen",
                          high = "white",
                          background = "white",
                          na.value = "gray",
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
                         background = force(background),
                         na.value = force(na.value),
                         limits = force(limits),
                         lockcells = force(lockcells)),
                    class = c("condformat_rule", "rule_fill_bar"))
  x <- add_rule_to_condformat(x, rule)
  return(x)
}

applyrule.rule_fill_bar <- function(rule, finalformat, xfiltered, xview, ...) {
  columns <- tidyselect::vars_select(colnames(xview), !!! rule[["columns"]])
  if (length(columns) == 0) {
    return(finalformat)
  }
  if (rlang::quo_is_missing(rule[["expression"]])) {
    if (length(columns) > 1) {
      warning("rule_fill_bar applied to multiple columns, using column ",
              columns[1], " values as expression. In the future this behaviour will change,",
              " please use a explicit expression instead.",
              call. = FALSE)
    }
    rule[["expression"]] <- as.symbol(as.name(columns[1]))
  }
  values_determining_color <- rlang::eval_tidy(rule[["expression"]], data = xfiltered)
  values_determining_color <- rep(values_determining_color, length.out = nrow(xfiltered))
  rule_bar_gradient_common(rule, finalformat, xview, columns, values_determining_color)
}

#' @importFrom scales rescale
rule_bar_gradient_common <- function(rule, finalformat, xview,
                                     columns, values_determining_color) {
  if (identical(rule[["limits"]], NA)) {
    limits <- range(values_determining_color, na.rm = TRUE)
  } else {
    limits <- rule[["limits"]]
    if (is.na(limits[1])) {
      limits[1] <- min(values_determining_color, na.rm = TRUE)
    }
    if (is.na(limits[2])) {
      limits[2] <- max(values_determining_color, na.rm = TRUE)
    }
  }

  values_rescaled <- scales::rescale(x = values_determining_color, from = limits)

  background_sizes <- ifelse(is.na(values_rescaled),
                             NA,
                             sprintf("%d%% 100%%", round(100*values_rescaled)))
  cell_border <- ifelse(is.na(values_rescaled),
                        "1px solid black",
                        "1px solid black")

  col_low <- grDevices::col2rgb(rule[["low"]])
  col_high <- grDevices::col2rgb(rule[["high"]])

  # FIXME: The representation of a nice gradient in CSS requires of changing
  # many CSS attributes. This is the first time in condformat where the CSS rule
  # is not trivial to export to LaTeX code.
  # For now we are using CSS as the internal representation because it has always
  # been practical for HTML, and easy write a LaTeX conversion function.
  # If we want LaTeX support for rule_fill_bar, we probably need a better
  # representation than CSS.
  linear_gradient <- ifelse(
    is.na(values_rescaled),
    NA,
    sprintf("linear-gradient(to right, rgba(%d, %d, %d, 1) 0%%, rgba(%d, %d, %d, 1) 100%%)",
            col_low[1], col_low[2], col_low[3], col_high[1], col_high[2], col_high[3]))

  background_repeat <- ifelse(is.na(values_rescaled),
                              NA, "no-repeat")

  background_color <- ifelse(is.na(values_rescaled),
                             rule[["na.value"]],
                             rule[["background"]])

  # Repeat the vector as many times as columns we are applying this to:
  background_sizes_mat <- matrix(background_sizes, nrow = nrow(xview),
                                 ncol = ncol(xview), byrow = FALSE)
  cell_border_mat <- matrix(cell_border, nrow = nrow(xview), ncol = ncol(xview), byrow = FALSE)


  background_grad_mat <- matrix(linear_gradient, nrow = nrow(xview),
                                ncol = ncol(xview), byrow = FALSE)

  background_repeat_mat <- matrix(background_repeat, nrow = nrow(xview),
                                  ncol = ncol(xview), byrow = FALSE)

  background_color_mat <- matrix(background_color, nrow = nrow(xview),
                                 ncol = ncol(xview), byrow = FALSE)

  finalformat <- fill_css_field_by_cols(finalformat, "background-size",
                                        background_sizes_mat, columns,
                                        xview, FALSE)
  finalformat <- fill_css_field_by_cols(finalformat, "background-image",
                                        background_grad_mat, columns,
                                        xview, FALSE)
  finalformat <- fill_css_field_by_cols(finalformat, "border",
                                        cell_border_mat, columns,
                                        xview, FALSE)
  finalformat <- fill_css_field_by_cols(finalformat, "background-color",
                                        background_color_mat, columns,
                                        xview, FALSE)
  finalformat <- fill_css_field_by_cols(finalformat, "background-repeat",
                                        background_repeat_mat, columns,
                                        xview, rule[["lockcells"]])
  return(finalformat)
}


