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

# Resolves a rule's `limits` argument against a vector of values: `NA`
# computes both endpoints from `values`'s range, and either endpoint alone
# being `NA` fills in just that one from `values`, leaving the other as given.
#
# @param values A numeric vector (e.g. one column's own values, or the result
#               of evaluating a rule's `expression`)
# @param limits A rule's `limits` argument: `NA`, or a length-2 numeric vector
#               whose elements may individually be `NA`
# @return A length-2 numeric vector with no `NA`s
resolve_limits <- function(values, limits) {
  if (identical(limits, NA)) {
    return(range(values, na.rm = TRUE))
  }
  if (is.na(limits[1])) {
    limits[1] <- min(values, na.rm = TRUE)
  }
  if (is.na(limits[2])) {
    limits[2] <- max(values, na.rm = TRUE)
  }
  limits
}
