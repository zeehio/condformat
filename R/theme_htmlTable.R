#' Customizes appearance of condformat object
#'
#' @param x The condformat object
#' @param ... Arguments to be passed to htmlTable
#' @seealso \code{\link[htmlTable]{htmlTable}}
#' @examples
#' data(iris)
#' condformat(head(iris)) %>% theme_htmlTable(caption="Table 1: My iris table", rnames=FALSE)
#' @export
theme_htmlTable <- function(x, ...) {
  api_dispatcher(theme_htmlTable_new, theme_htmlTable_old)
}

add_theme_to_condformat <- function(x, theme) {
  condformatopts <- attr(x, "condformat")
  condformatopts$themes <- c(condformatopts$themes, list(theme))
  attr(x, "condformat") <- condformatopts
  x
}

theme_htmlTable_new <- function(x, ...) {
  if (!inherits(x, "condformat_tbl")) {
    x <- condformat(x)
  }

  htmlargs <- list(...)
  theme <- structure(list(htmlargs = htmlargs),
                     class = c("theme_htmlTable", "condformat_theme"))
  x <- add_theme_to_condformat(x, theme)
  return(x)
}

theme_htmlTable_old <- function(...) {
  # Deprecated
  theme <- structure(list(htmlargs = list(...)),
                     class = c("theme_htmlTable", "condformat_theme"))
  return(theme)
}

render_theme.theme_htmlTable <- function(themeobj, finaltheme, xview, ...) {
  for (paramname in names(themeobj$htmlargs)) {
    finaltheme[[paramname]] <- themeobj$htmlargs[[paramname]]
  }
  finaltheme
}
