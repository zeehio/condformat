#' Converts a data frame or matrix to a condformat_tbl class.
#' condformat_tbl objects allow to add conditional formatting information,
#' that can be viewed when the condformat_tbl object is printed.
#'
#' They also have all properties from conventional data frames.
#'
#' @param x A matrix or data.frame
#' @param select a character string or vector with the column names to be printed.
#'               Columns not listed here may be used in conditional rules, although
#'               will not appear in the generated table when printing. Default: colnames(x)
#' @param col.names a character string or vector of the same length of `select` with the desired column names to be shown
#'                  in the printed table.
#' @param ... additional arguments passed to htmlTable
#'
#' @return The condformat_tbl object, that can be printed to generate the output.
#' @importFrom dplyr tbl_df
#' @examples
#' data(iris)
#' library(magrittr)
#' condformat(iris,
#'            select=c("Sepal.Length", "Species"),
#'            col.names=c("Sepal length", "Species")) %>% print
#' @export
condformat <- function(x, select, col.names, ...) {
  # We will show only selected columns if given:
  if (missing(select)) {
    select <- colnames(x)
  }

  if (missing(col.names)) {
    col.names <- select
  }

  x <- dplyr::tbl_df(x)
  attr(x, "condformat") <- list(view_dim=c(nrow(x), length(select)),
                                view_select=select,
                                view_names=col.names,
                                rules=list(),
                                htmltable_args=list(...))

  if (! ("condformat_tbl" %in% class(x))) {
    class(x) <- c("condformat_tbl", class(x))
  }
  return(x)
}

applyrule <- function(rule, finalformat, x, ...) UseMethod("applyrule")

applyrule.default <- function(rule, finalformat, x, ...) {
  finalformat
}

#' Prints the data frame in an html page and shows it.
#'
#' @param x A condformat_tbl object
#' @param rule The rule to be applied
#' @return x, with an attribute defining the rule
#' @examples
#' data(iris)
#' condformat(iris) + rule_column("Species", "background:red")
#' @method + condformat_tbl
#' @export
"+.condformat_tbl" <- function(x, rule) {
  if (inherits(rule, "condformat_rule")) {
    condformatopts <- attr(x, "condformat")
    condformatopts$rules <- c(condformatopts$rules, list(rule))
    attr(x, "condformat") <- condformatopts
    return(x)
  } else {
    NextMethod()
  }
}


#' Prints the data frame in an html page and shows it.
#'
#' @param x A condformat_tbl object
#' @param ... optional arguments to print
#' @return the value returned by htmlTable
#' @importFrom htmlTable htmlTable
#' @examples
#' data(iris)
#' library(dplyr)
#' condformat(iris) %>% print
#' @export
print.condformat_tbl <- function(x, ...) {
  condformatopts <- attr(x, "condformat")
  xview <- x[,condformatopts$view_select]
  colnames(xview) <- condformatopts$view_names

  rules <- condformatopts$rules

  finalformat <- list(css_cell = matrix(data="",
                                        nrow=nrow(xview),
                                        ncol=ncol(xview)),
                      css_cell_unlocked = matrix(data=TRUE,
                                                 nrow=nrow(xview),
                                                 ncol=ncol(xview)))

  for (rule in rules) {
    finalformat <- applyrule(rule, finalformat, x)
  }

  thetable <- do.call(htmlTable::htmlTable, c(list(format(xview),
                                                   rnames=FALSE,
                                                   css.cell=finalformat$css_cell),
                                              condformatopts$htmltable_args))
  invisible(print(thetable))
}

#' @importFrom knitr knit_print
#' @export
knit_print.condformat_tbl <- function(x, ...) {
  knitr::knit_print(print(x), ...)
}



#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
NULL
