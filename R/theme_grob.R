#' Customizes appearance of condformat object
#'
#' This is only used on grob output.
#'
#' @param x The condformat object
#' @param ... Arguments to be passed to gridExtra::tableGrob (see examples)
#' @seealso \code{\link[gridExtra]{tableGrob}}
#' @examples
#' data(iris)
#' cf <- condformat(head(iris)) %>%
#'   theme_grob(base_size = 10, base_colour = "red")
#' \dontrun{
#' print(cf)
#' }
#' @export
theme_grob <- function(x, ...) {
  if (!inherits(x, "condformat_tbl")) {
    x <- condformat(x)
  }
  given_args <- list(...)
  theme <- structure(list(tableGrobArgs = given_args),
                     class = c("theme_grob", "condformat_theme"))
  x <- add_theme_to_condformat(x, theme)
  return(x)
}

render_theme.theme_grob <- function(themeobj, finaltheme, xview, ...) {
  if (!"tableGrobArgs" %in% names(finaltheme)) {
    finaltheme[["tableGrobArgs"]] <- list()
  }
  for (paramname in names(themeobj[["tableGrobArgs"]])) {
    if (is.null(themeobj[["tableGrobArgs"]][[paramname]])) {
      finaltheme[["tableGrobArgs"]][paramname] <- list(NULL)
    } else {
      finaltheme[["tableGrobArgs"]][[paramname]] <- themeobj[["tableGrobArgs"]][[paramname]]
    }
  }
  finaltheme
}
