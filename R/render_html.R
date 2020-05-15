#' Converts the table to a htmlTable object
#'
#' @param x A condformat_tbl object
#' @return the htmlTable object
#' @examples
#' data(iris)
#' cf <- condformat2html(condformat(iris[1:5,]))
#' \dontrun{
#' print(cf)
#' }
#' @export
condformat2html <- function(x) {
  htmltable_ready <- condformat2htmlcommon(x)
  thetable <- rlang::exec(
    htmlTable::htmlTable,
    x = htmltable_ready[["xview"]],
    css.cell = htmltable_ready[["css_cell"]],
    !!!htmltable_ready[["htmlTableArgs"]]
  )
  return(thetable)
}

#' Converts the table to a htmlTableWidget
#'
#' @param x A condformat_tbl object
#' @param ... Deprecated: Arguments passed to htmlTable::htmlTableWidget
#' @return the htmlTable widget
#' @examples
#' \dontrun{
#' data(iris)
#' cf <- condformat2widget(condformat(iris[1:5,]))
#' \dontrun{
#' print(cf)
#' }
#' }
#' @export
condformat2widget <- function(x, ...) {
  # Deprecation path starts here:
  condformatwidget_args <- list(...)
  if (length(condformatwidget_args) > 0) {
    warning("Passing arguments to condformat2widget is deprecated. Use theme_htmlWidget(...) instead")
    x <- theme_htmlWidget(x, ...)
  }
  # Deprecation path ends here
  htmltable_ready <- condformat2htmlcommon(x)
  thewidget <- rlang::exec(
    htmlTable::htmlTableWidget,
    x = htmltable_ready[["xview"]],
    css.cell = htmltable_ready[["css_cell"]],
    !!!htmltable_ready[["htmlTableArgs"]],
    !!!htmltable_ready[["htmlWidget"]]
  )
  return(thewidget)
}

merge_css <- function(css_fields, dims) {
  css_keys <- names(css_fields)
  output <- matrix("", nrow = dims[1], ncol = dims[2])
  for (key in css_keys) {
    # This is straight CSS:
    key_value_pair <- css_fields[[key]]
    key_value_pair[is.na(key_value_pair)] <- ""
    have_values <- nchar(key_value_pair) > 0
    # Prepend key:
    key_value_pair[have_values] <- paste(key, key_value_pair[have_values],
                                         sep = ": ")
    had_other_values <- nchar(output) > 0
    output[had_other_values & have_values] <- paste0(output[had_other_values & have_values], "; ")
    output[have_values] <- paste0(output[have_values], key_value_pair[have_values])
  }
  output <- matrix(output, nrow = dims[1], ncol = dims[2])
  return(output)
}

render_cf_fields_to_css_fields <- function(cf_fields, xview) {
  dims <- dim(xview)
  css_fields <- list()
  unlocked <- matrix(TRUE, nrow = nrow(xview), ncol = ncol(xview))
  for (cf_field in cf_fields) {
    css_fields_and_unlocked <- cf_field_to_css(cf_field, xview, css_fields, unlocked)
    css_fields <- css_fields_and_unlocked[["css_fields"]]
    unlocked <- css_fields_and_unlocked[["unlocked"]]
  }
  css_fields
}

condformat2htmlcommon <- function(x) {
  xv_cf <- get_xview_and_cf_fields(x)
  xview <- xv_cf[["xview"]]
  cf_fields <- xv_cf[["cf_fields"]]
  final_colnames <- xv_cf[["final_colnames"]]

  css_fields <- render_cf_fields_to_css_fields(cf_fields, xview)
  css_values <- merge_css(css_fields, dim(xview))
  # Rename the columns according to show options:
  colnames(xview) <- final_colnames
  themes <- attr(x, "condformat")[["themes"]]
  finaltheme <- render_theme_condformat_tbl(themes, xview)
  htmlTableArgs <- finaltheme[["html"]]
  if ("css.cell" %in% names(htmlTableArgs)) {
    css_cell_dims <- dim(xview)
    css_cell <- paste0(htmlTableArgs[["css.cell"]], css_values)
    dim(css_cell) <- css_cell_dims
    htmlTableArgs[["css.cell"]] <- NULL
  } else {
    css_cell <- css_values
  }

  if (is.null(htmlTableArgs[["caption"]]) && !is.null(finaltheme[["caption"]])) {
    htmlTableArgs[["caption"]] <- finaltheme[["caption"]]
  }
  return(list(xview = format.data.frame(xview),
              css_cell = css_cell,
              htmlTableArgs = htmlTableArgs,
              htmlWidget = finaltheme[["htmlWidget"]]))
}


#' How to export a cf_field to CSS
#'
#' This method is exported so package users can generate their own rules
#'
#' @param cf_field A cf_field object. This is like a rule, but with the computed
#'                 colour values. It usually maps one-to-one to a CSS field.
#' @param xview A data frame with the columns to be printed and rows filtered
#' @param css_fields A list of matrices. The names of the list are CSS attributes and
#'   each matrix is of the size of xview and contains the respective CSS values.
#' @param unlocked A logical matrix of cells unlocked (that can still be modified by further
#'  rules).
#' @return A list with two elements: css_fields and unlocked (with updated values)
#'
#' @export
cf_field_to_css <- function(cf_field, xview, css_fields, unlocked) UseMethod("cf_field_to_css")

cf_field_to_css.default <- function(cf_field, xview, css_fields, unlocked) {
  warning("cf key ", class(cf_field)[1], " is not supported by condformat in this output format")
  list(css_fields = css_fields, unlocked = unlocked)
}

