#' Formats a whole column of a condformat_tbl
#'
#' @param x A condformat_tbl, or an object that can be coerced to that
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
rule_column <- function(x, column, css, lockcells=FALSE) {
  rule <- structure(list(column=column, css=css, lockcells=lockcells),
                    class=c("condformat_rule", "rule_column"))

  condformatopts <- attr(x, "condformat")
  index.j <- match(rule$column, condformatopts$view_select)
  if (is.na(index.j)) {
    return(x)
  }

  apply_to <- matrix(data=FALSE,
                     nrow=nrow(x),
                     ncol=length(condformatopts$view_select))

  apply_to[, index.j] <- TRUE

  condformatopts$css.cell[apply_to & condformatopts$css_cell_unlocked] <- rule$css

  if (rule$lockcells) {
    condformatopts$css_cell_unlocked[apply_to & condformatopts$css_cell_unlocked] <- FALSE
  }
  attr(x, "condformat") <- condformatopts
  return(x)
}
