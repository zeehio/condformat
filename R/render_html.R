#' Converts the table to a htmlTable object
#'
#' @param x A condformat_tbl object
#' @return the htmlTable object
#' @examples
#' data(iris)
#' condformat2html(condformat(iris[1:5,]))
#' @export
condformat2html <- function(x) {
  htmltable_ready <- condformat2htmlcommon(x)
  thetable <- do.call(htmlTable::htmlTable,
                      c(list(x = htmltable_ready[["xview"]],
                             css.cell = htmltable_ready[["css_cell"]]),
                        htmltable_ready[["htmlTableArgs"]]))
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
#' condformat2widget(condformat(iris[1:5,]))
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
  thewidget <- do.call(what = htmlTable::htmlTableWidget,
                       args = c(list(x = htmltable_ready[["xview"]],
                                     css.cell = htmltable_ready[["css_cell"]]),
                                htmltable_ready[["htmlTableArgs"]],
                                htmltable_ready[["htmlWidget"]]))
  return(thewidget)
}

merge_css_conditions <- function(initial_value, css_fields) {
  css_keys <- names(css_fields)
  output <- initial_value
  for (key in css_keys) {
    key_value_pair <- css_fields[[key]]
    have_values <- nchar(key_value_pair) > 0
    # Prepend key:
    key_value_pair[have_values] <- paste(key, key_value_pair[have_values],
                                         sep = ": ")
    had_other_values <- nchar(output) > 0
    output[had_other_values & have_values] <- paste0(output[had_other_values & have_values], "; ")
    output[have_values] <- paste0(output[have_values], key_value_pair[have_values])
  }
  output <- matrix(output, nrow = nrow(initial_value), ncol = ncol(initial_value))
  return(output)
}

condformat2htmlcommon <- function(x) {
  finalshow <- render_show_condformat_tbl(x)
  xfiltered <- finalshow[["xfiltered"]]
  xview <- xfiltered[, finalshow[["cols"]], drop = FALSE]
  rules <- attr(x, "condformat")[["rules"]]
  finalformat <- render_rules_condformat_tbl(rules, xfiltered, xview)
  if (length(finalformat[["css_fields"]]) > 0) {
    finalformat[["css_cell"]] <- merge_css_conditions(finalformat[["css_cell"]],
                                                      finalformat[["css_fields"]])
  }
  # Rename the columns according to show options:
  colnames(xview) <- names(finalshow[["cols"]])
  themes <- attr(x, "condformat")[["themes"]]
  finaltheme <- render_theme_condformat_tbl(themes, xview)
  htmlTableArgs <- finaltheme[["html"]]
  if ("css.cell" %in% names(htmlTableArgs)) {
    css_cell_dims <- dim(finalformat[["css_cell"]])
    css_cell <- paste0(htmlTableArgs[["css.cell"]], finalformat[["css_cell"]])
    dim(css_cell) <- css_cell_dims
    htmlTableArgs[["css.cell"]] <- NULL
  } else {
    css_cell <- finalformat[["css_cell"]]
  }

  if (is.null(htmlTableArgs[["caption"]]) && !is.null(finaltheme[["caption"]])) {
    htmlTableArgs[["caption"]] <- finaltheme[["caption"]]
  }
  return(list(xview = format.data.frame(xview),
              css_cell = css_cell,
              htmlTableArgs = htmlTableArgs,
              htmlWidget = finaltheme[["htmlWidget"]]))
}
