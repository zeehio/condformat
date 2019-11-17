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

rule_to_cf_field.rule_text_bold <- function(rule, xfiltered, xview, ...) {
  columns <- do.call(tidyselect::vars_select, c(list(colnames(xview)), rule[["columns"]]))
  if (length(columns) == 0) {
    return(NULL)
  }
  if (rlang::quo_is_missing(rule[["expression"]])) {
    if (length(columns) > 1) {
      warning("rule_text_bold applied to multiple columns, using column ",
              columns[1], " values as expression. In the future this behaviour will change,",
              "please use a explicit expression instead.",
              call. = FALSE)
    }
    rule[["expression"]] <- as.symbol(as.name(columns[1]))
  }
  bold_or_not <- rlang::eval_tidy(rule[["expression"]], data = xfiltered)
  stopifnot(identical(length(bold_or_not), nrow(xview)))
  # Recycle css values to fit all the columns:
  bold_or_not_mat_l <- matrix(bold_or_not, nrow = nrow(xview),
                              ncol = ncol(xview), byrow = FALSE)

  bold_or_not_mat <- matrix(NA, nrow = nrow(xview), ncol = ncol(xview))
  colnames(bold_or_not_mat) <- colnames(xview)
  bold_or_not_mat[bold_or_not, columns] <- "bold"
  bold_or_not_mat[!bold_or_not, columns] <- "normal"
  bold_or_not_mat[is.na(bold_or_not), columns] <- rule[["na.value"]]

  cf_field <- structure(list(css_key = "font-weight",
                             css_values = bold_or_not_mat,
                             lock_cells = rule[["lockcells"]]),
                        class = c("cf_field_rule_text_bold",
                                  "cf_field_css", "cf_field"))
  return(cf_field)
}

cf_field_to_latex.cf_field_rule_text_bold <- function(cf_field, xview, unlocked) {
  css_values <- cf_field[["css_values"]]
  to_lock <- !is.na(css_values)
  css_values[is.na(css_values) | !unlocked] <- ""
  before <- ifelse(css_values == "bold", "\\textbf{", "")
  after <- ifelse(css_values == "bold", "}", "")

  if (cf_field[["lock_cells"]]) {
    unlocked <- unlocked | to_lock
  }
  list(before = before, after = after, unlocked = unlocked)
}

cf_field_to_gtable.cf_field_rule_text_bold <- function(cf_field, xview, gridobj, unlocked, has_rownames, has_colnames) {
  css_values <- cf_field[["css_values"]]
  to_lock <- !is.na(css_values)
  css_values[is.na(css_values) | !unlocked] <- ""
  gpbold <- grid::gpar(fontface = "bold")

  row_col <- which(css_values == "bold", arr.ind = TRUE)
  for (tobold in seq_len(nrow(row_col))) {
    ind <- find_cell(gridobj,
                     as.integer(has_colnames) + row_col[tobold, 1],
                     as.integer(has_rownames) + row_col[tobold, 2],
                     name = "core-fg")
    gridobj$grobs[ind][[1]][["gp"]][["fontface"]] <- gpbold[["fontface"]]
    gridobj$grobs[ind][[1]][["gp"]][["font"]] <- gpbold[["font"]]
  }

  if (cf_field[["lock_cells"]]) {
    unlocked <- unlocked | to_lock
  }
  list(gridobj = gridobj, unlocked = unlocked)
}
