#' Sets the caption of a condformat object
#'
#' The advantage with respect to theme_htmlTable(caption = "My table") is that
#' this works with HTML and LaTeX outputs
#'
#' @param x The condformat object
#' @param caption The caption to show
#' @examples
#' data(iris)
#' cf <- condformat(head(iris)) %>%
#'   theme_caption(caption = "My Caption")
#' \dontrun{
#' print(cf)
#' }
#' @export
theme_caption <- function(x, caption = "") {
  if (!inherits(x, "condformat_tbl")) {
    x <- condformat(x)
  }
  theme <- structure(list(caption = caption),
                     class = c("theme_caption", "condformat_theme"))
  x <- add_theme_to_condformat(x, theme)
  return(x)
}

render_theme.theme_caption <- function(themeobj, finaltheme, xview, ...) {
  finaltheme[["caption"]] <- themeobj[["caption"]]
  finaltheme
}
