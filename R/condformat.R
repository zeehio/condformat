#' Conditional formatting for data frames
#'
#' A \code{condformat_tbl} object is a data frame with attributes regarding
#' the formatting of their cells, that can be viewed when the \code{condformat_tbl}
#' object is printed.
#'
#' @param x A matrix or data.frame
#'
#' @return The condformat_tbl object. This object can be piped to apply
#'  conditional formatting rules. It can also be used as a conventional
#'  data frame.
#'
#'  The condformat_tbl print method generates an htmlTable, to be
#'  viewed using RStudio Viewer or an HTML browser, as available.
#' @examples
#' data(iris)
#' condformat(iris[1:5,])
#'
#' condformat(iris[1:5,]) %>% rule_fill_gradient(Sepal.Length)
#'
#' condformat(iris[1:5,]) %>%
#'  rule_fill_discrete(Sepal.Length, expression=Sepal.Width > 2)
#' @export
condformat <- function(x) {
  x <- tibble::as_tibble(x)
  attr(x, "condformat") <- list(show = list(rows = list(),
                                            cols = list()),
                                rules = list(),
                                themes = list())

  class(x) <- c("condformat_tbl", class(x))
  return(x)
}


#' Combines data with formatting rules (deprecated)
#'
#' This is deprecated
#'
#' @param x A condformat_tbl object
#' @param obj A condformat_show or a condformat_rule object to be combined
#'            Any other type of object will be added as expected to the data frame.
#' @return x, with extended condformat_tbl attributes
#' @examples
#' data(iris)
#' condformat(iris[1:5,]) + show_columns(Species)
#' @method + condformat_tbl
#' @export
"+.condformat_tbl" <- function(x, obj) {
  #Deprecated
  if (inherits(obj, "condformat_show_columns")) {
    condformatopts <- attr(x, "condformat")
    condformatopts[[c("show", "cols")]] <- c(condformatopts[[c("show", "cols")]], list(obj))
    attr(x, "condformat") <- condformatopts
    return(x)
  } else if (inherits(obj, "condformat_show_rows")) {
    condformatopts <- attr(x, "condformat")
    condformatopts[[c("show", "rows")]] <- c(condformatopts[[c("show", "rows")]], list(obj))
    attr(x, "condformat") <- condformatopts
    return(x)
  } else if (inherits(obj, "condformat_rule")) {
    condformatopts <- attr(x, "condformat")
    condformatopts[["rules"]] <- c(condformatopts[["rules"]], list(obj))
    attr(x, "condformat") <- condformatopts
    return(x)
  } else if (inherits(obj, "condformat_theme")) {
    condformatopts <- attr(x, "condformat")
    condformatopts[["themes"]] <- c(condformatopts[["themes"]], list(obj))
    attr(x, "condformat") <- condformatopts
    return(x)
  } else {
    NextMethod()
  }
}

# Deprecated
#' @importFrom lazyeval uq
#' @export
lazyeval::uq # D

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`
