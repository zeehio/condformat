add_rule_to_condformat <- function(x, rule) {
  condformatopts <- attr(x, "condformat")
  condformatopts[["rules"]] <- c(condformatopts[["rules"]], list(rule))
  attr(x, "condformat") <- condformatopts
  x
}

# Evaluates `expr` once per column in `columns`, with the `.col` pronoun bound
# in the data mask to that column's own values from `xfiltered`. This lets a
# rule applied to several columns at once use an expression relative to each
# column (e.g. `.col > 3`), instead of a single expression whose result is
# broadcast identically to every selected column.
#
# @param expr A quosure or symbol to evaluate (typically `rule[["expression"]]`)
# @param xfiltered The data frame the expression is evaluated against
# @param columns A named integer vector of selected columns, as returned by
#                [tidyselect::eval_select()]
# @return A named list (one element per column in `columns`), each recycled
#         to `nrow(xfiltered)`
eval_expression_per_column <- function(expr, xfiltered, columns) {
  col_names <- names(columns)
  base_mask <- as.list(xfiltered)
  values <- lapply(col_names, function(col_name) {
    mask <- c(base_mask, list(.col = xfiltered[[col_name]]))
    value <- rlang::eval_tidy(expr, data = mask)
    rep(value, length.out = nrow(xfiltered))
  })
  names(values) <- col_names
  values
}
