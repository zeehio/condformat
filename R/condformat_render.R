#' Prints the data frame in an html page and shows it.
#'
#' @param x A condformat_tbl object
#' @param ... optional arguments to print
#' @return the value returned by htmlTable
#' @examples
#' data(iris)
#' print(condformat(iris[1:5,]))
#' @export
print.condformat_tbl <- function(x, ...) {
  outfmt <- guess_output_format()
  if (outfmt != "") {
    return(knitr::knit_print(x))
  } else {
    thetable <- condformat2html(x)
    print(thetable)
    invisible(x)
  }
}


#' Converts the table to a htmlTable object
#'
#' @param x A condformat_tbl object
#' @return the htmlTable object
#' @examples
#' data(iris)
#' condformat2html(condformat(iris[1:5,]))
#' @export
condformat2html <- function(x) {
  finalshow <- render_show_condformat_tbl(x)
  xfiltered <- finalshow$xfiltered
  xview <- xfiltered[, finalshow$cols, drop = FALSE]
  rules <- attr(x, "condformat")$rules
  finalformat <- render_rules_condformat_tbl(rules, xfiltered, xview,
                                             format = "html")
  # Rename the columns according to show options:
  colnames(xview) <- names(finalshow$cols)
  themes <- attr(x, "condformat")$themes
  finaltheme <- render_theme_condformat_tbl(themes, xview)
  if ("css.cell" %in% names(finaltheme)) {
    css_cell_dims <- dim(finalformat$css_cell)
    css_cell <- paste0(finaltheme$css.cell, finalformat$css_cell)
    dim(css_cell) <- css_cell_dims
    finaltheme$css.cell <- NULL
  } else {
    css_cell <- finalformat$css_cell
  }
  thetable <- do.call(htmlTable::htmlTable, c(list(x = format.data.frame(xview),
                                                   css.cell = css_cell),
                                              finaltheme))
  return(thetable)
}

#' Converts the table to a htmlTableWidget
#'
#' @param x A condformat_tbl object
#' @return the htmlTable widget
#' @examples
#' \dontrun{
#' data(iris)
#' condformat2widget(condformat(iris[1:5,]))
#' }
#' @export
condformat2widget <- function(x) {
  if (utils::packageVersion("htmlTable") <= "1.8") {
    stop("htmlTable>1.8 is required for widget support")
  }
  finalshow <- render_show_condformat_tbl(x)
  xfiltered <- finalshow$xfiltered
  xview <- xfiltered[, finalshow$cols, drop = FALSE]
  rules <- attr(x, "condformat")$rules
  finalformat <- render_rules_condformat_tbl(rules, xfiltered, xview,
                                             format = "html")
  # Rename the columns according to show options:
  colnames(xview) <- names(finalshow$cols)
  themes <- attr(x, "condformat")$themes
  finaltheme <- render_theme_condformat_tbl(themes, xview)
  thewidget <- do.call(what = htmlTable::htmlTableWidget,
                       args = c(list(x = format.data.frame(xview),
                                     css.cell = finalformat$css_cell),
                                finaltheme))
  return(thewidget)
}


#' Writes the table to an Excel sheet
#'
#' @param x A condformat_tbl object
#' @param filename The xlsx file name
#' @export
#'
condformat2excel <- function(x, filename) {
  if (!requireNamespace("xlsx", quietly = TRUE)) {
    stop("Please install the xlsx package in order to export to excel")
  }
  # We need this until https://github.com/dragua/xlsx/pull/76 is merged
  # We can drop rJava from suggests once this is fixed
  if (!requireNamespace("rJava", quietly = TRUE)) {
    stop("Please install the rJava package in order to export to excel")
  }
  rJava::.jpackage("xlsx")
  # Until here

  if (!grepl(pattern = '\\.xlsx$', filename)) { # endsWith(filename, ".xlsx")
    filename <- paste0(filename, ".xlsx")
  }
  finalshow <- render_show_condformat_tbl(x)
  xfiltered <- finalshow$xfiltered
  xview <- xfiltered[, finalshow$cols, drop = FALSE]
  rules <- attr(x, "condformat")$rules
  finalformat <- render_rules_condformat_tbl(rules, xfiltered, xview,
                                             format = "excel")
  wb <- xlsx::createWorkbook(type = "xlsx")
  sheet <- xlsx::createSheet(wb, sheetName = "Sheet1")

  xlsx::addDataFrame(x = as.data.frame(xview),
                     sheet = sheet, row.names = F, col.names = T)
  if ("background-color" %in% names(finalformat$css_fields)) {
    for (i in 1:nrow(xview)) {
      for (j in 1:ncol(xview)) {
        background_color <- ifelse(finalformat$css_fields$`background-color`[i,j] == "", NA, finalformat$css_fields$`background-color`[i,j])
        if (!is.na(background_color)) {
          cb <- xlsx::CellBlock.default(sheet, startRow = i + 1, startColumn = j,
                                        noRows = 1, noColumns = 1, create = FALSE)
          fill <- xlsx::Fill(backgroundColor = background_color, foregroundColor = background_color)
          xlsx::CB.setFill(cellBlock = cb,
                           fill = fill,
                           rowIndex = 1, colIndex = 1)
        }
      }
    }
  }
  xlsx::saveWorkbook(wb, file = filename)
  return(x)
}

#' Converts the table to LaTeX code
#' @param x A condformat_tbl object
#' @param ... arguments passed to knitr::kable
#' @return A character vector of the table source code
#' @export
condformat2latex <- function(x, ...) {
  finalshow <- render_show_condformat_tbl(x)
  xfiltered <- finalshow$xfiltered
  xview <- xfiltered[, finalshow$cols, drop = FALSE]
  rules <- attr(x, "condformat")$rules
  finalformat <- render_rules_condformat_tbl(rules, xfiltered, xview,
                                             format = "latex")
  # Rename the columns according to show options:
  colnames(finalformat) <- names(finalshow$cols)
  # Theme is ignored in LaTeX
  # themes <- attr(x, "condformat")$themes
  # finaltheme <- render_theme_condformat_tbl(themes, xview)
  return(knitr::kable(finalformat, format = "latex",
                      escape = FALSE, ...))
}


guess_output_format <- function() {
  outfmt <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if (is.null(outfmt)) {
    outfmt <- knitr::opts_knit$get("out.format")
  }
  if (is.null(outfmt)) {
    return("")
  }
  if (outfmt == "latex" || outfmt == "beamer") {
    return("latex")
  } else if (outfmt == "html" || substr(outfmt, 1, nchar("markdown")) == "markdown") {
    return("html")
  } else {
    return("unsupported")
  }
}

#' Print method for knitr, exporting to HTML or LaTeX as needed
#' @param x Object to print
#' @param ... Provided for knitr_print compatibility
#'
#' @importFrom knitr knit_print
#' @export
knit_print.condformat_tbl <- function(x, ...) {
  outfmt <- guess_output_format()
  if (outfmt == "latex") {
    latex_dependencies <- list(rmarkdown::latex_dependency(name = "xcolor",
                                                           options = "table"))
    use_longtable <- knitr::opts_current$get("longtable")
    if (is.null(use_longtable) || use_longtable == TRUE) {
      latex_dependencies <- c(latex_dependencies,
                              list(rmarkdown::latex_dependency(name = "longtable")))
      use_longtable <- TRUE
    }
    return(knitr::asis_output(condformat2latex(x, longtable = use_longtable),
                              meta = latex_dependencies))
  } else if (outfmt == "html") {
    return(knitr::asis_output(condformat2html(x)))
  } else {
    warning("knitr format not supported by condformat")
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
  names(finalshow$cols) <- colnames(x)

  # First we filter, then we select so we can
  # filter by variables not selected
  showobjs <- c(condformatopts$show$rows,
                condformatopts$show$cols)
  for (showobj in showobjs) {
    finalshow <- render_show(showobj, finalshow, finalshow$xfiltered)
  }
  return(finalshow)
}

merge_css_conditions <- function(initial_value, css_fields) {
  css_keys <- names(css_fields)
  output <- initial_value
  for (key in css_keys) {
    thisfield <- paste(key, css_fields[[key]], sep = ": ")
    output <- paste(output, thisfield, sep = "; ") # I don't care about a leading "; "
  }
  output <- matrix(output, nrow = nrow(initial_value), ncol = ncol(initial_value))
  return(output)
}

merge_css_conditions_to_latex <- function(css_fields, raw_text) {
  css_keys <- names(css_fields)
  output <- ""
  before <- ""
  after <- ""
  for (key in css_keys) {
    if (key == 'background-color') {
      # Get the colors
      colors <- css_fields[[key]]
      # Convert to hex:
      colors[nchar(colors) > 0] <- gplots::col2hex(colors[nchar(colors) > 0])
      # remove initial hash "#......"
      colors <- substr(colors, 2, nchar(colors))
      # if color, wrap latex code:
      colors[nchar(colors) > 0] <- paste0("\\cellcolor[HTML]{", colors[nchar(colors) > 0], "}")
      before <- colors
    }
    #thisfield <- paste(key, css_fields[[key]], sep = ": ")
    #output <- paste(output, thisfield, sep = "; ") # I don't care about a leading "; "
  }
  output <- paste0(before, raw_text, after)
  output <- matrix(output, nrow = nrow(raw_text), ncol = ncol(raw_text))
  return(output)
}


# escape special LaTeX characters:
# from https://github.com/yihui/knitr (R/utils.R)
escape_latex = function(x, newlines = FALSE, spaces = FALSE) {
  x = gsub('\\\\', '\\\\textbackslash', x)
  x = gsub('([#$%&_{}])', '\\\\\\1', x)
  x = gsub('\\\\textbackslash', '\\\\textbackslash{}', x)
  x = gsub('~', '\\\\textasciitilde{}', x)
  x = gsub('\\^', '\\\\textasciicircum{}', x)
  if (newlines) x = gsub('(?<!\n)\n(?!\n)', '\\\\\\\\', x, perl = TRUE)
  if (spaces) x = gsub('  ', '\\\\ \\\\ ', x)
  x
}


#' Renders the css matrix to format the xview table
#'
#' @param rules List of rules to be applied
#' @param xview Data frame with the rows and columns that will be printed
#' @param xfiltered Like xview, but with all the columns (rules
#'                  will use columns that won't be printed)
#' @param format Output format (either "html" or "latex")
#' @return List with the CSS information
render_rules_condformat_tbl <- function(rules, xfiltered, xview, format) {

  finalformat <- list(css_fields = list(),
                      css_cell = matrix(data = "", nrow = nrow(xview), ncol = ncol(xview)),
                      css_cell_unlocked = matrix(data = TRUE,
                                                 nrow = nrow(xview),
                                                 ncol = ncol(xview)))

  for (rule in rules) {
    finalformat <- applyrule(rule, finalformat, xfiltered, xview)
  }
  if (format == "html") {
    if (length(finalformat$css_fields) > 0) {
      finalformat$css_cell <- merge_css_conditions(finalformat$css_cell, finalformat$css_fields)
    }
    return(finalformat)
  } else if (format == "latex") {
    raw_text <- escape_latex(as.matrix(format(xview)))
    # Need to wrap raw_text with formatting rules
    formatted_text <- merge_css_conditions_to_latex(css_fields = finalformat$css_fields, raw_text = raw_text)
    return(formatted_text)
  } else if (format == "excel") {
    return(finalformat)
  } else {
    stop("Unsupported format:", format)
  }
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

