#' Customizes appearance of condformat object
#'
#' @param x The condformat object
#' @param ... Arguments to be passed to htmlTable
#' @seealso \code{\link[htmlTable]{htmlTable}}
#' @examples
#' data(iris)
#' condformat(head(iris)) %>% theme_htmlTable(caption="Table 1: My iris table", rnames=FALSE)
#' @export
theme_htmlTable <- function(...) {
  htmlargs <- list(...)
  if (length(htmlargs) > 0) {
    if (inherits(htmlargs[[1]], "condformat_tbl")) {
      return (theme_htmlTable_new(...))
    }
  }
  warning("The condformat syntax using '+' is deprecated. See ?theme_htmlTable for more information")
  # Deprecated
  theme <- structure(list(htmlargs = htmlargs),
                     class = c("theme_htmlTable", "condformat_theme"))
  return(theme)
}

add_theme_to_condformat <- function(x, theme) {
  condformatopts <- attr(x, "condformat")
  condformatopts$themes <- c(condformatopts$themes, list(theme))
  attr(x, "condformat") <- condformatopts
  x
}

#' @rdname theme_htmlTable
theme_htmlTable_new <- function(x, ...) {
  htmlargs <- list(...)
  theme <- structure(list(htmlargs = htmlargs),
                     class = c("theme_htmlTable", "condformat_theme"))
  x <- add_theme_to_condformat(x, theme)
  return(x)
}

render_theme.theme_htmlTable <- function(themeobj, finaltheme, xview, ...) {
  for (paramname in names(themeobj$htmlargs)) {
    finaltheme[[paramname]] <- themeobj$htmlargs[[paramname]]
  }
  finaltheme
}
