#' Formats a whole column of a condformat_tbl
#'
#' @param column a character string or vector with the column names to be coloured.
#' @param css a character string with the CSS code to be applied to those columns
#' @param lockcells logical value determining if no further rules should be applied to the affected cells.
#'
#' @return The condformat_tbl object, with the added information
#' @examples
#' data(iris)
#' library(magrittr)
#' condformat(iris) %>% rule_column(column="Species", css="background: red")
#' @export
rule_column <- function(column, css, lockcells=FALSE) {
  rule <- structure(list(column=column, css=css, lockcells=lockcells),
                    class=c("condformat_rule", "rule_column"))
  return(rule)
}

applyrule.rule_column <- function(rule, finalformat, x) {
  condformatopts <- attr(x, "condformat")
  index.j <- match(rule$column, condformatopts$view_select)
  if (is.na(index.j)) {
    return(finalformat)
  }

  apply_to <- matrix(data=FALSE,
                     nrow=nrow(x),
                     ncol=length(condformatopts$view_select))

  apply_to[, index.j] <- TRUE

  finalformat$css_cell[apply_to & finalformat$css_cell_unlocked] <- rule$css

  if (rule$lockcells) {
    finalformat$css_cell_unlocked[apply_to & finalformat$css_cell_unlocked] <- FALSE
  }
  return(finalformat)
}
