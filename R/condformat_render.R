#' Prints the data frame in an html page and shows it.
#'
#' @param x A condformat_tbl object
#' @inheritDotParams htmltools::html_print -html
#' @param paginate A logical value. If TRUE the printing will be paginated
#' @return the value returned by htmlTable
#' @examples
#' data(iris)
#' \dontrun{
#' print(condformat(iris[1:5,]))
#' }
#' @export
print.condformat_tbl <- function(x, ..., paginate = TRUE) {
  if (!is.null(getOption("knitr.in.progress"))) {
    return(knit_print(x, paginate = paginate))
  } else {
    htmltools::html_print(condformat2html_or_widget(x, paginate = paginate), ...)
  }
  invisible(x)
}

condformat2html_or_widget <- function(x, paginate = TRUE) {
  if (paginate) {
    htmltools::as.tags(condformat2widget(x), standalone = TRUE)
  } else {
    htmltools::HTML(condformat2html(x))
  }
}

#' Print method for knitr, exporting to HTML or LaTeX as needed
#' @param x Object to print
#' @param ... On a LaTeX output these are unused. On an HTML output can have "paginate=TRUE" or "paginate = FALSE"
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
    dots <- list(...)
    paginate <- ifelse(isTRUE(dots$paginate), TRUE, FALSE)
    if (paginate) {
      return(condformat2widget(x))
    } else {
      return(knitr::asis_output(condformat2html(x)))
    }
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

get_xview_and_cf_fields <- function(x) {
  finalshow <- render_show_condformat_tbl(x)
  xfiltered <- finalshow[["xfiltered"]]
  xview <- xfiltered[, finalshow[["cols"]], drop = FALSE]
  rules <- attr(x, "condformat")[["rules"]]
  cf_fields <- rules_to_cf_fields(rules, xfiltered, xview)

  list(cf_fields = cf_fields, xview = xview,
       final_colnames = names(finalshow[["cols"]]))
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
rules_to_cf_fields <- function(rules, xfiltered, xview) {
  cf_fields <- lapply(rules,
                      function(rule) rule_to_cf_field(rule, xfiltered, xview))
  return(cf_fields)
}

render_show <- function(showobj, finalshow, x, ...) UseMethod("render_show")

render_show.default <- function(showobj, finalshow, x , ...) {
  finalshow
}

rule_to_cf_field <- function(rule, xfiltered, xview, ...) UseMethod("rule_to_cf_field")

render_theme <- function(themeobj, finaltheme, xview, ...) UseMethod("render_theme")


render_theme.default <- function(themeobj, finaltheme, xview, ...) {
  finaltheme
}
