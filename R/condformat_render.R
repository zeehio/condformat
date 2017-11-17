#' Prints the data frame in an html page and shows it.
#'
#' @param x A condformat_tbl object
#' @param ... optional arguments to print
#' @param paginate A logical value. If TRUE the printing will be paginated
#' @return the value returned by htmlTable
#' @examples
#' data(iris)
#' print(condformat(iris[1:5,]))
#' @export
print.condformat_tbl <- function(x, ..., paginate = TRUE) {
  if (paginate) {
    htmltools::html_print(htmltools::as.tags(condformat2widget(x),
                                             standalone = TRUE))

  } else {
    htmltools::html_print(htmltools::HTML(condformat2html(x)))
  }
  invisible(x)
}


#' Print method for knitr, exporting to HTML or LaTeX as needed
#' @param x Object to print
#' @param ... Arguments passed for compatibility with knit_print
#'
#' @importFrom knitr knit_print
#' @export
knit_print.condformat_tbl <- function(x, ...) {
  if (knitr::is_latex_output()) {
    latex_dependencies <- list(rmarkdown::latex_dependency(name = "xcolor",
                                                           options = "table"))
    use_longtable <- knitr::opts_current$get("longtable")
    if (is.null(use_longtable) || use_longtable == TRUE) {
      latex_dependencies <- c(latex_dependencies,
                              list(rmarkdown::latex_dependency(name = "longtable")))
      use_longtable <- TRUE
    }
    return(knitr::asis_output(
      condformat2latex(x %>% theme_kable(longtable = use_longtable)),
      meta = latex_dependencies))
  } else if (knitr::is_html_output()) {
    return(knitr::asis_output(condformat2html(x)))
  } else {
    warning("Output format not supported by condformat. Printing regular table")
    return(knitr::knit_print(knitr::kable(x), ...))
  }
}

render_theme_condformat_tbl <- function(themes, xview) {
  finaltheme <- list()
  for (themeobj in themes) {
    finaltheme <- render_theme(themeobj, finaltheme, xview)
  }
  return(finaltheme)
}


render_show_condformat_tbl <- function(x) {
  condformatopts <- attr(x, "condformat")

  finalshow <- list(xfiltered = x,
                    cols = colnames(x))
  names(finalshow[["cols"]]) <- colnames(x)

  # First we filter, then we select so we can
  # filter by variables not selected
  showobjs <- c(condformatopts[[c("show", "rows")]],
                condformatopts[[c("show", "cols")]])
  for (showobj in showobjs) {
    finalshow <- render_show(showobj, finalshow, finalshow[["xfiltered"]])
  }
  return(finalshow)
}

# Renders the css matrix to format the xview table
#
# @param rules List of rules to be applied
# @param xview Data frame with the rows and columns that will be printed
# @param xfiltered Like xview, but with all the columns (rules
#                  will use columns that won't be printed)
# @return List with the CSS information
render_rules_condformat_tbl <- function(rules, xfiltered, xview) {
  finalformat <- list(css_fields = list(),
                      css_cell = matrix(data = "", nrow = nrow(xview), ncol = ncol(xview)),
                      css_cell_unlocked = matrix(data = TRUE,
                                                 nrow = nrow(xview),
                                                 ncol = ncol(xview)))

  for (rule in rules) {
    finalformat <- applyrule(rule, finalformat, xfiltered, xview)
  }
  return(finalformat)
}

render_show <- function(showobj, finalshow, x, ...) UseMethod("render_show")

render_show.default <- function(showobj, finalshow, x , ...) {
  finalshow
}

applyrule <- function(rule, finalformat, xfiltered, xview, ...) UseMethod("applyrule")


applyrule.default <- function(rule, finalformat, xfiltered, xview, ...) {
   finalformat
}

render_theme <- function(themeobj, finaltheme, xview, ...) UseMethod("render_theme")


render_theme.default <- function(themeobj, finaltheme, xview, ...) {
  finaltheme
}
