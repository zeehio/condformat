#' Customizes appearance of condformat object
#'
#' @param x The condformat object
#' @param ... Arguments to be passed to htmlTable::htmlTableWidget (see examples)
#' @seealso [htmlTable::htmlTable()]
#' @examples
#' data(iris)
#' cf <- condformat(head(iris)) %>%
#'   theme_htmlWidget(number_of_entries = c(10, 25, 100),
#'                    width = NULL, height = NULL, elementId = NULL)
#' \dontrun{
#' print(cf)
#' }
#' @export
theme_htmlWidget <- function(x, ...) {
  if (!inherits(x, "condformat_tbl")) {
    x <- condformat(x)
  }
  valid_args <- setdiff(
    names(formals(htmlTable::htmlTableWidget)),
    c("x", "..."))
  given_args <- list(...)
  given_arg_names <- names(given_args)
  # Use full names (so abbreviations are expanded):
  full_arg_names <- valid_args[pmatch(given_arg_names, valid_args)]
  if (any(is.na(full_arg_names))) {
    wrong_args <- paste(given_arg_names[is.na(full_arg_names)], collapse = ", ")
    warning("The following arguments are unknown by htmlTable::htmlTableWidget: ", wrong_args)
  }

  given_args[is.na(given_args)] <- NULL
  theme <- structure(list(widget_args = given_args),
                     class = c("theme_htmlWidget", "condformat_theme"))
  x <- add_theme_to_condformat(x, theme)
  return(x)
}

render_theme.theme_htmlWidget <- function(themeobj, finaltheme, xview, ...) {
  if (!"htmlWidget" %in% names(finaltheme)) {
    finaltheme[["htmlWidget"]] <- list()
  }
  for (paramname in names(themeobj[["widget_args"]])) {
    finaltheme[["htmlWidget"]][[paramname]] <- themeobj[["widget_args"]][[paramname]]
  }
  finaltheme
}
