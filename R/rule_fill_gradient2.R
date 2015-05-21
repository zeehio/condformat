#' Fills the background color of a column using a three colors gradient based on
#' the values of a given column
#'
#' @param x A condformat_tbl object.
#' @param column a character string or vector with the column names to be coloured.
#' @param colour_by a character string with the column name of the values to be used to generate the gradient
#' @param low,mid,high a character string with the CSS color code for the three levels of the gradient
#' @param midpoint the value used for the middle color
#' @param space the color space passed to scales::div_gradient_pal
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
#' @importFrom scales muted div_gradient_pal rescale_mid
#' @importFrom stats median
rule_fill_gradient2 <- function(x, column,
                                colour_by=column,
                                low = scales::muted("red"), mid="white", high = scales::muted("blue"),
                                midpoint = stats::median(x[[colour_by]], na.rm=TRUE),
                                space = "rgb",
                                na.value = "#7F7F7F",
                                limits=range(x[,colour_by], na.rm = TRUE),
                                lockcells = FALSE) {

  rule <- structure(list(column = column, colour_by = column,
                         low = low, mid = mid, high = high, midpoint = midpoint, space = space,
                         na.value = na.value, limits = limits, lockcells = lockcells),
                    class = c("condformat_rule", "rule_fill_gradient2"))
  condformatopts <- attr(x, "condformat")
  index.j <- match(rule$column, condformatopts$view_select)
  if (is.na(index.j)) {
    return(x)
  }

  col_scale <- scales::div_gradient_pal(low = rule$low, mid = rule$mid, high = rule$high, space = rule$space)

  values_determining_color <- x[[rule$colour_by]]
  values_rescaled <- scales::rescale_mid(x = values_determining_color,
                                         from = rule$limits, mid = rule$midpoint)
  colours_for_values <- col_scale(values_rescaled)

  unlocked_rows <- condformatopts$css_cell_unlocked[,index.j]

  condformatopts$css.cell[unlocked_rows, index.j] <- paste0("background: ", colours_for_values[unlocked_rows])
  if (rule$lockcells) {
    condformatopts$css_cell_unlocked[unlocked_rows, index.j] <- FALSE
  }
  attr(x, "condformat") <- condformatopts
  return(x)
}

