#' Converts the table to a grid object
#'
#' @param x A condformat_tbl object
#' @return the grid object
#' @examples
#' library(condformat)
#' data.frame(Student = c("Alice", "Bob", "Charlie"),
#'            Evaluation = c("Great", "Well done", "Good job!")) %>%
#'  condformat %>%
#'  condformat2grob
#' @export
condformat2grob <- function(x) {
  xv_cf <- get_xview_and_cf_fields(x)
  xview <- xv_cf[["xview"]]
  cf_fields <- xv_cf[["cf_fields"]]
  final_colnames <- xv_cf[["final_colnames"]]
  themes <- attr(x, "condformat")[["themes"]]
  finaltheme <- render_theme_condformat_tbl(themes, xview)
  tableGrobArgs <- finaltheme[["tableGrobArgs"]]

  gridobj <- do.call(gridExtra::tableGrob,
                     c(list(d = xview,
                            cols = final_colnames),
                       tableGrobArgs))
  has_rownames <- !("rows" %in% names(tableGrobArgs) && is.null(tableGrobArgs[["rows"]]))
  has_colnames <- !is.null(final_colnames)
  gridobj <- render_cf_fields_to_grob(cf_fields, xview, gridobj, has_rownames, has_colnames)
  grid::grid.newpage()
  grid::grid.draw(gridobj)
  invisible(gridobj)
}

render_cf_fields_to_grob <- function(cf_fields, xview, gridobj, has_rownames, has_colnames) {
  unlocked <- matrix(TRUE, nrow = nrow(xview), ncol = ncol(xview))
  for (cf_field in cf_fields) {
    gridobj_and_unlocked <- cf_field_to_gtable(cf_field, xview, gridobj, unlocked, has_rownames, has_colnames)
    gridobj <- gridobj_and_unlocked[["gridobj"]]
    unlocked <- gridobj_and_unlocked[["unlocked"]]
  }
  gridobj
}


#' How to export a cf_field to grob
#'
#' This method is exported so package users can generate their own rules
#'
#' @param cf_field A cf_field object. This is like a rule, but with the computed
#'                 colour values. It usually maps one-to-one to a CSS field.
#' @param xview A data frame with the columns to be printed and rows filtered
#' @param gridobj The tableGrob object
#' @param unlocked A logical matrix of cells unlocked (that can still be modified by further
#'  rules).
#' @param has_rownames Whether or not the gridobj has a first column with row names
#' @param has_colnames Whether or not the gridobj has a first row with column names
#' @return A list with two elements: gridobj and unlocked (with updated values)
#'
#' @export
cf_field_to_gtable <- function(cf_field, xview, gridobj, unlocked, has_rownames, has_colnames) UseMethod("cf_field_to_gtable")

cf_field_to_gtable.default <- function(cf_field, xview, gridobj, unlocked, has_rownames, has_colnames) {
  warning("cf key ", class(cf_field)[1], " is not supported by condformat in this output format")
  list(gridobj = gridobj, unlocked = unlocked)
}

# Helper used by cf_field_to_gtable functions
find_cell <- function(table, row, col, name="core-fg"){
  l <- table$layout
  which(l$t == row & l$l == col & l$name == name)
}
