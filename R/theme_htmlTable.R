#' Customizes appearance of condformat object
#'
#' @param x The condformat object
#' @param ... Arguments to be passed to htmlTable
#' @seealso [htmlTable::htmlTable()]
#' @examples
#' data(iris)
#' cf <- condformat(head(iris)) %>%
#'   theme_htmlTable(caption="Table 1: My iris table", rnames=FALSE)
#' \dontrun{
#' print(cf)
#' }
#' @export
theme_htmlTable <- function(x, ...) {
  if (!inherits(x, "condformat_tbl")) {
    x <- condformat(x)
  }
  valid_htmltable_args <- setdiff(
    names(formals(utils::getS3method("htmlTable", "default"))),
    c("x", "..."))
  htmlargs <- list(...)
  given_arg_names <- names(htmlargs)
  # Use full htmlTable names (so abbreviations are expanded):
  full_arg_names <- valid_htmltable_args[pmatch(given_arg_names,
                                                valid_htmltable_args)]
  if (any(is.na(full_arg_names))) {
    wrong_args <- given_arg_names[is.na(full_arg_names)]
    # Deprecation code path starts here: # D
    valid_htmlwidget_args <- c("number_of_entries", "width", "height", "elementId")
    deprecated_args <- wrong_args %in% valid_htmlwidget_args
    if (any(deprecated_args)) {
      warning("The following arguments should be given to theme_htmlWidget() instead of htmlTable: ",
              paste(wrong_args[!deprecated_args], collapse = ", "),
              ". This will be an error in a future condformat version")
      htmlwidgetargs <- htmlargs[wrong_args[deprecated_args]]
      x <- rlang::exec(theme_htmlWidget, x = x, !!!htmlwidgetargs)
      wrong_args <- wrong_args[!deprecated_args]
    }
    # Deprecation code path ends here
    if (length(wrong_args) > 0 ) {
      stop("The following arguments are unknown by htmlTable: ",
           paste(wrong_args, collapse = ", "))
    }
    htmlargs[is.na(full_arg_names)] <- NULL
  }
  theme <- structure(list(htmlargs = htmlargs),
                     class = c("theme_htmlTable", "condformat_theme"))
  x <- add_theme_to_condformat(x, theme)
  return(x)
}

add_theme_to_condformat <- function(x, theme) {
  condformatopts <- attr(x, "condformat")
  condformatopts[["themes"]] <- c(condformatopts[["themes"]], list(theme))
  attr(x, "condformat") <- condformatopts
  x
}

#' @importFrom htmlTable htmlTable
render_theme.theme_htmlTable <- function(themeobj, finaltheme, xview, ...) {
  if (!"html" %in% finaltheme) {
    finaltheme[["html"]] <- list()
  }
  for (paramname in names(themeobj[["htmlargs"]])) {
    finaltheme[["html"]][[paramname]] <- themeobj[["htmlargs"]][[paramname]]
  }
  finaltheme
}
