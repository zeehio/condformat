#' Fills the background color of a column using a gradient based on
#' the values of a given column
#'
#' @param x A condformat_tbl object.
#' @param column a character string or vector with the column names to be coloured.
#' @param colour_by a character string with the column name of the values to be used to generate the gradient
#' @param low,high a character string with the CSS color code
#' @param space the color space passed to scales::seq_gradient_pal
#' @param na.value a character string with the CSS color to be used in missing values
#' @param limits range of limits that the gradient should cover
#' @param lockcells logical value determining if no further rules should be applied to the affected cells.
#'
#' @return The condformat_tbl object, with the added gradient information
#' @examples
#' data(iris)
#' library(magrittr)
#' condformat(iris) %>% rule_fill_gradient(column="Sepal.Length") %>% print
#' @export
#' @importFrom scales seq_gradient_pal rescale
rule_fill_gradient <- function(x, column,
                               colour_by=column,
                               low = "#132B43", high = "#56B1F7",
                               space = "Lab",
                               na.value = "#7F7F7F",
                               limits=range(x[,colour_by], na.rm = TRUE),
                               lockcells=FALSE) {

  rule <- structure(list(column = column, colour_by = column,
                         low = low, high = high, space = space, na.value = na.value,
                         limits = limits, lockcells = lockcells),
                    class = c("condformat_rule", "rule_fill_gradient"))
  condformatopts <- attr(x, "condformat")
  index.j <- match(rule$column, condformatopts$view_select)
  if (is.na(index.j)) {
    return(x)
  }

  col_scale <- scales::seq_gradient_pal(low = rule$low, high = rule$high, space = rule$space)

  values_determining_color <- x[[rule$colour_by]]
  values_rescaled <- scales::rescale(x = values_determining_color, from = rule$limits)
  colours_for_values <- col_scale(values_rescaled)

  unlocked_rows <- condformatopts$css_cell_unlocked[,index.j]

  condformatopts$css.cell[unlocked_rows, index.j] <-
    paste0("background: ", colours_for_values[unlocked_rows])
  if (rule$lockcells) {
    condformatopts$css_cell_unlocked[unlocked_rows, index.j] <- FALSE
  }
  attr(x, "condformat") <- condformatopts
  return(x)
}
