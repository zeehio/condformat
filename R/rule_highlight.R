#' Highlights cells of a column based on a given condition
#'
#' @param x A condformat_tbl object.
#' @param column a character string or vector with the column names to be coloured.
#' @param condition a character string with the condition to be evaluated to apply the given css format.
#' @param css a character string with the CSS code to be applied to those columns
#' @param lockcells logical value determining if no further rules should be applied to the affected cells.
#'
#' @return The condformat_tbl object, with the added information
#' @examples
#' data(iris)
#' library(magrittr)
#' condformat(iris) %>%
#'   rule_highlight(column="Species",
#'                  condition="Sepal.Width > Sepal.Length - 2.25",
#'                  css="background: #7D00FF") %>% print
#' @export
rule_highlight <- function(x, column, condition, css, lockcells=FALSE) {
  rule <- structure(list(column=column, condition=condition,
                         css=css, lockcells=lockcells),
                    class=c("condformat_rule", "rule_highlight"))
  condformatopts <- attr(x, "condformat")
  index.j <- match(rule$column, condformatopts$view_select)
  if (is.na(index.j)) {
    return(x)
  }

  apply_to <- matrix(data=FALSE, nrow=nrow(x), ncol=length(condformatopts$view_select))
  indexes.i <- which(with(x, eval(parse(text=rule$condition))))
  apply_to[indexes.i, index.j] <- TRUE

  condformatopts$css.cell[apply_to & condformatopts$css_cell_unlocked] <- rule$css
  if (rule$lockcells) {
    condformatopts$css_cell_unlocked[apply_to] <- FALSE
  }
  attr(x, "condformat") <- condformatopts
  return(x)
}
