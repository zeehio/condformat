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
#' expr <- rlang::parse_quosure(expr_as_text)
#' condformat(x) %>% show_rows(!! expr)
#' # With multiple arguments:
#' expr_as_text <- c('Sepal.Length > 4.5', 'Species == "setosa"')
#' exprs <- lapply(expr_as_text, rlang::parse_quosure)
#' condformat(x) %>% show_rows(!!! exprs)
#' @export
#' @seealso \code{\link[dplyr]{filter}}
show_rows <- function(...) {
  quoted_args <- rlang::quos(...)
  condformat_api <- "0.6"
  tryCatch({
    possible_condformat <- quoted_args[[1]]
    x <- rlang::eval_tidy(possible_condformat)
    stopifnot(inherits(x, "condformat_tbl"))
    condformat_api <- "0.7"
  }, error = function(err) {
    condformat_api <- "0.6"
  })
  if (condformat_api == "0.7") {
    return(show_rows_new(...))
  } else if (condformat_api == "0.6") {
    return(show_rows_old(...))
  } else {
    stop("Unknown condformat API")
  }
}

#' @rdname show_rows
show_rows_new <- function(x, ...) {
  expr <- rlang::quos(...)
  showobj <- structure(list(row_expr = expr),
                       class = c("condformat_show_rows", "condformat_show_rows_filter"))
  x2 <- x
  condformatopts <- attr(x2, "condformat")
  condformatopts$show$rows <- c(condformatopts$show$rows, list(showobj))
  attr(x2, "condformat") <- condformatopts
  return(x2)
}

#' Selects the rows to be printed (deprecated)
#'
#' This function is deprecated. Use \code{\link{show_rows}} instead
#'
#' Keeps the rows you mention in the printed table.
#' Compared to \code{\link[dplyr]{filter}}, show_rows does not remove the
#' rows from the actual data frame, they are removed only for printing.
#'
#' @param ... Expressions used for filtering
#'
#' @return A condformat_show_rows object, usually to be added to a condformat_tbl object
#'         as shown in the examples
#' @examples
#' library(condformat)
#' data(iris)
#' x <- head(iris)
#' condformat(x) + show_rows(Sepal.Length > 4.5, Species == "setosa")
#' @seealso \code{\link[dplyr]{filter}}
show_rows_old <- function(...) {
  show_rows_(.dots = lazyeval::lazy_dots(...))  #,row_names = row_names)
}

#' @rdname show_rows_old
#' @param .dots A list of lazy objects. See examples
#' @export
#' @examples
#' library(condformat)
#' data(iris)
#' x <- head(iris)
#' condformat(x) + show_rows_(.dots = c("Sepal.Length > 4.5", "Species == 'setosa'"))
show_rows_ <- function(..., .dots) {
  warning("This condformat syntax is deprecated. See ?show_rows for more information", call. = FALSE)
  dots <- lazyeval::all_dots(.dots, ...)
#   if (missing(row_names)) {
#     row_names <- NA
#   }
  showobj <- structure(list(row_expr = dots),
                       class = c("condformat_show_rows", "condformat_show_rows_filter"))
  return(showobj)
}

render_show.condformat_show_rows_filter <- function(showobj, finalshow, x, ...) {
  if (inherits(showobj$row_expr, "lazy_dots")) {
    # Deprecated
    xfiltered <- dplyr::filter_(x, .dots = showobj$row_expr) # D
    finalshow$xfiltered <- xfiltered
    return(finalshow)
  } else {
    xfiltered <- dplyr::filter(x, !!! showobj$row_expr)
    finalshow$xfiltered <- xfiltered
    return(finalshow)
  }
}
