#' Converts the table to LaTeX code
#' @param x A condformat_tbl object
#' @return A character vector of the table source code
#' @export
condformat2latex <- function(x) {
  xv_cf <- get_xview_and_cf_fields(x)
  xview <- xv_cf[["xview"]]
  cf_fields <- xv_cf[["cf_fields"]]
  final_colnames <- xv_cf[["final_colnames"]]

  raw_text <- as.matrix(format.data.frame(xview))
  themes <- attr(x, "condformat")[["themes"]]
  finaltheme <- render_theme_condformat_tbl(themes, xview)
  kable_args <- finaltheme[["kable_args"]]
  if ("escape" %in% names(kable_args)) {
    escape <- kable_args[["escape"]]
    # We do the escape here, because we add LaTeX code to the cells
    kable_args[["escape"]] <- NULL
  } else {
    escape <- TRUE
  }
  if (isTRUE(escape)) {
    raw_text <- escape_latex(raw_text)
  }
  # Need to wrap raw_text with formatting rules
  formatted_text <- merge_cf_conditions_to_latex(
    cf_fields = cf_fields, xview = xview, raw_text = raw_text)

  # Rename the columns according to show options:
  colnames(formatted_text) <- final_colnames

  caption <- kable_args[["caption"]]
  if (is.null(caption)) {
    caption <- finaltheme[["caption"]]
  }

  if (isTRUE(escape)) {
    colnames(formatted_text) <- escape_latex(colnames(formatted_text))
    if (!is.null(caption)) {
      caption <- escape_latex(caption)
    }
  }

  if (!is.null(caption)) {
    kable_args[["caption"]] <- caption
  }
  rlang::exec(
    knitr::kable,
    x = formatted_text,
    format = "latex",
    escape = FALSE,
    !!!kable_args
  )
}

paste0mat <- function(x,y) {
  stopifnot(all(dim(x) == dim(y)))
  dims <- dim(x)
  out <- paste0(x, y)
  dim(out) <- dims
  return(out)
}

merge_cf_conditions_to_latex <- function(cf_fields, xview, raw_text) {
  output <- ""
  before <- matrix("", nrow = nrow(raw_text), ncol = ncol(raw_text))
  after <- matrix("", nrow = nrow(raw_text), ncol = ncol(raw_text))
  unlocked <- matrix(TRUE, nrow = nrow(raw_text), ncol = ncol(raw_text))
  for (cf_field in cf_fields) {
    bef_after <- cf_field_to_latex(cf_field, xview, unlocked)
    before <- paste0mat(before, bef_after[["before"]])
    after <- paste0mat(bef_after[["after"]], after)
    unlocked <- bef_after[["unlocked"]]
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

#' How to export cf values to latex
#'
#' @inheritParams cf_field_to_css
#' @return A list with two character matrices named `before` and `after`. Both
#'         of these matrices must be of the same size as `xview`.
#'
#' @export
cf_field_to_latex <- function(cf_field, xview, unlocked) UseMethod("cf_field_to_latex")

#' @export
cf_field_to_latex.default <- function(cf_field, xview, unlocked) {
  warning("cf key ", class(cf_field)[1], " is not supported by condformat in LaTeX output")
  before <- matrix("", nrow = nrow(xview), ncol = ncol(xview))
  after <- matrix("", nrow = nrow(xview), ncol = ncol(xview))
  list(before = before, after = after, unlocked = unlocked)
}
