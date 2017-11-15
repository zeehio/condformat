#' Customizes appearance of condformat object
#'
#' This is only used on LaTeX output.
#'
#' @param x The condformat object
#' @param ... Arguments to be passed to knitr::kable (see examples)
#' @seealso \code{\link[htmlTable]{htmlTable}}
#' @examples
#' data(iris)
#' condformat(head(iris)) %>%
#'   theme_kable(booktabs = TRUE, caption = "My Caption")
#' @export
theme_kable <- function(x, ...) {
  if (!inherits(x, "condformat_tbl")) {
    x <- condformat(x)
  }
  given_args <- list(...)
  theme <- structure(list(kable_args = given_args),
                     class = c("theme_kable", "condformat_theme"))
  x <- add_theme_to_condformat(x, theme)
  return(x)
}

render_theme.theme_kable <- function(themeobj, finaltheme, xview, ...) {
  if (!"kable_args" %in% names(finaltheme)) {
    finaltheme[["kable_args"]] <- list()
  }
  for (paramname in names(themeobj[["kable_args"]])) {
    finaltheme[["kable_args"]][[paramname]] <- themeobj[["kable_args"]][[paramname]]
  }
  finaltheme
}
