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
                      c(list(x = htmltable_ready$xview,
                             css.cell = htmltable_ready$css_cell),
                        htmltable_ready$htmlTableArgs))
  return(thetable)
}

#' Converts the table to a htmlTableWidget
#'
#' @param x A condformat_tbl object
#' @param ... Arguments passed to htmlTable::htmlTableWidget
#' @return the htmlTable widget
#' @examples
#' \dontrun{
#' data(iris)
#' condformat2widget(condformat(iris[1:5,]))
#' }
#' @export
condformat2widget <- function(x, ...) {
  htmltable_ready <- condformat2htmlcommon(x)
  thewidget <- do.call(what = htmlTable::htmlTableWidget,
                       args = c(list(x = htmltable_ready$xview,
                                     css.cell = htmltable_ready$css_cell),
                                htmltable_ready$htmlTableArgs,
                                list(...)))
  return(thewidget)
}

condformat2htmlcommon <- function(x) {
  finalshow <- render_show_condformat_tbl(x)
  xfiltered <- finalshow$xfiltered
  xview <- xfiltered[, finalshow$cols, drop = FALSE]
  rules <- attr(x, "condformat")$rules
  finalformat <- render_rules_condformat_tbl(rules, xfiltered, xview,
                                             format = "html")
  # Rename the columns according to show options:
  colnames(xview) <- names(finalshow$cols)
  themes <- attr(x, "condformat")$themes
  finaltheme <- render_theme_condformat_tbl(themes, xview)
  if ("css.cell" %in% names(finaltheme)) {
    css_cell_dims <- dim(finalformat$css_cell)
    css_cell <- paste0(finaltheme$css.cell, finalformat$css_cell)
    dim(css_cell) <- css_cell_dims
    finaltheme$css.cell <- NULL
  } else {
    css_cell <- finalformat$css_cell
  }
  return(list(xview = format.data.frame(xview),
              css_cell = css_cell,
              htmlTableArgs = finaltheme))
}
