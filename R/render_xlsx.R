require_xlsx <- function() {
  if (!requireNamespace("xlsx", quietly = TRUE)) {
    stop("Please install the xlsx package in order to export to excel")
  }
  # We need this until https://github.com/dragua/xlsx/pull/76 is released
  # We can drop rJava from suggests once this is fixed
  if (!requireNamespace("rJava", quietly = TRUE)) {
    stop("Please install the rJava package in order to export to excel")
  }
  rJava::.jpackage("xlsx")
  # Until here
}

#' Writes the table to an Excel workbook
#'
#' @param x A condformat_tbl object
#' @param filename The xlsx file name.
#' @param sheet_name The name of the sheet where the table will be written
#' @param overwrite_wb logical to overwrite the workbook file
#' @param overwrite_sheet logical to overwrite the sheet
#' @export
#'
condformat2excel <- function(x, filename, sheet_name = "Sheet1",
                             overwrite_wb = FALSE,
                             overwrite_sheet = TRUE) {
  require_xlsx()

  if (!grepl(pattern = '\\.xlsx$', filename)) { # endsWith(filename, ".xlsx")
    filename <- paste0(filename, ".xlsx")
  }

  if (file.exists(filename) && identical(overwrite_wb, FALSE)) {
    wb <- xlsx::loadWorkbook(filename)
  } else {
    wb <- xlsx::createWorkbook(type = "xlsx")
  }

  # getSheets cat's a message I don't want nor care about.
  noSheets <- wb$getNumberOfSheets()
  if (noSheets > 0) {
    sheet_list <- xlsx::getSheets(wb)
  } else {
    sheet_list <- list()
  }
  if (sheet_name %in% names(sheet_list)) {
    if (overwrite_sheet) {
      xlsx::removeSheet(wb = wb, sheetName = sheet_name)
      sheet <- xlsx::createSheet(wb, sheetName = sheet_name)
    } else {
      sheet <- sheet_list[[sheet_name]]
    }
  } else {
    sheet <- xlsx::createSheet(wb, sheetName = sheet_name)
  }
  condformat2excelsheet(x, sheet)
  xlsx::saveWorkbook(wb, file = filename)
  return(invisible(x))
}

# Writes the table to an Excel sheet
#
# @param x A condformat_tbl object
# @param sheet The sheet object
# @examples
# \dontrun{
# x <- condformat(iris[1:5,])
# library(xlsx)
# wb <- xlsx::createWorkbook(type = "xlsx")
# sheet <- xlsx::createSheet(wb, sheetName = "Sheet1")
# condformat2excelsheet(x, sheet)
# xlsx::saveWorkbook(wb, file = "iris.xlsx")
# }
condformat2excelsheet <- function(x, sheet) {
  require_xlsx()
  if (!"jobjRef" %in% class(sheet)) {
    stop("sheet must be an jobjRef object, as the one returned with xls::createSheet()")
  }
  finalshow <- render_show_condformat_tbl(x)
  xfiltered <- finalshow[["xfiltered"]]
  xview <- xfiltered[, finalshow[["cols"]], drop = FALSE]
  rules <- attr(x, "condformat")[["rules"]]
  cf_fields <- rules_to_cf_fields(rules, xfiltered, xview)
  css_fields <- render_cf_fields_to_css_fields(cf_fields, xview)

  xlsx::addDataFrame(x = as.data.frame(xview),
                     sheet = sheet, row.names = FALSE, col.names = TRUE)
  if ("background-color" %in% names(css_fields)) {
    for (i in seq_len(nrow(xview))) {
      for (j in seq_len(ncol(xview))) {
        background_color <- ifelse(css_fields[["background-color"]][i,j] == "",
                                   NA,
                                   css_fields[["background-color"]][i,j])
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
  invisible(x)
}

