#' Selects the rows to be printed
#'
#' Keeps the rows you mention in the printed table.
#' Compared to \code{\link[dplyr]{filter}}, show_rows does not remove the
#' rows from the actual data frame, they are removed only for printing.
#' @param x condformat_tbl object
#' @param ... Expressions used for filtering
#'
#' @return A condformat_show_rows object, usually to be added to a condformat_tbl object
#'         as shown in the examples
#' @examples
#' library(condformat)
#' data(iris)
#' x <- head(iris)
#' condformat(x) %>% show_rows(Sepal.Length > 4.5, Species == "setosa")
#' # Use it programatically
#' expr_as_text <- 'Sepal.Length > 4.5'
#' expr <- rlang::parse_expr(expr_as_text)
#' condformat(x) %>% show_rows(!! expr)
#' # With multiple arguments:
#' expr_as_text <- c('Sepal.Length > 4.5', 'Species == "setosa"')
#' exprs <- lapply(expr_as_text, rlang::parse_expr)
#' condformat(x) %>% show_rows(!!! exprs)
#' @export
#' @seealso \code{\link[dplyr]{filter}}
show_rows <- function(x, ...) {
  expr <- rlang::quos(...)
  showobj <- structure(list(row_expr = expr),
                       class = c("condformat_show_rows", "condformat_show_rows_filter"))
  x2 <- x
  if (!inherits(x2, "condformat_tbl")) {
    x2 <- condformat(x2)
  }
  condformatopts <- attr(x2, "condformat")
  condformatopts[[c("show", "rows")]] <- c(condformatopts[[c("show", "rows")]], list(showobj))
  attr(x2, "condformat") <- condformatopts
  return(x2)
}

render_show.condformat_show_rows_filter <- function(showobj, finalshow, x) {
  xfiltered <- dplyr::filter(x, !!! showobj[["row_expr"]])
  finalshow[["xfiltered"]] <- xfiltered
  return(finalshow)
}
