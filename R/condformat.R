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

