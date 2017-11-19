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

rule_to_cf_field.rule_css <- function(rule, xfiltered, xview, ...) {
  columns <- tidyselect::vars_select(colnames(xview), !!! rule[["columns"]])
  if (length(columns) == 0) {
    return(NULL)
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
  css_values_mat <- matrix(NA,
                           nrow = nrow(xview), ncol = ncol(xview),
                           byrow = FALSE)
  colnames(css_values_mat) <- colnames(xview)
  css_values_mat[, columns] <- css_values
  cf_field <- structure(list(css_key = rule[["css_field"]],
                             css_values = css_values_mat,
                             lock_cells = rule[["lockcells"]]),
                        class = c("cf_field_css", "cf_field"))
  return(cf_field)
}

# This is used by all CSS based rules
cf_field_to_css.cf_field_css <- function(cf_field, xview, css_fields, unlocked) {
  css_key <- cf_field[["css_key"]]
  css_values <- cf_field[["css_values"]]

  mask <- unlocked
  # mask == TRUE if cell can be changed, false otherwise

  # if the css value is NA, ignore it as well
  # (so we don't override previous values)
  mask <- mask & !is.na(css_values)

  if (css_key %in% names(css_fields)) {
    prev_values <- css_fields[[css_key]]
  } else {
    prev_values <- matrix(NA, nrow = nrow(xview), ncol = ncol(xview))
  }
  prev_values[mask] <- css_values[mask]

  css_fields[[css_key]] <- prev_values
  if (identical(cf_field[["lock_cells"]], TRUE)) {
    unlocked[mask] <- FALSE
  }
  return(list(css_fields = css_fields, unlocked = unlocked))
}
