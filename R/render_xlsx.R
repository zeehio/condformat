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

  if (!grepl(pattern = '\\.xlsx$', filename)) { # endsWith(filename, ".xlsx")
    filename <- paste0(filename, ".xlsx")
  }

  if (file.exists(filename) && identical(overwrite_wb, FALSE)) {
    wb <- openxlsx::loadWorkbook(filename)
  } else {
    wb <- openxlsx::createWorkbook(creator = "")
  }

  if (sheet_name %in% names(wb)) {
    if (overwrite_sheet) {
      openxlsx::removeWorksheet(wb = wb, sheet = sheet_name)
      openxlsx::addWorksheet(wb, sheetName = sheet_name)
    }
  } else {
    openxlsx::addWorksheet(wb, sheetName = sheet_name)
  }
  condformat2excelsheet(x, wb, sheet_name)
  openxlsx::saveWorkbook(
    wb,
    file = filename,
    overwrite = overwrite_wb | overwrite_sheet
  )
  return(invisible(x))
}

# Writes the table to an Excel sheet
#
# @param x A condformat_tbl object
# @param sheet The sheet object
# @examples
# \dontrun{
# x <- condformat(iris[1:5,])
# library(openxlsx)
# wb <- openxlsx::createWorkbook(creator = "")
# openxlsx::addWorksheet(wb, sheetName = "sheet name")
# condformat2excelsheet(x, wb, "sheet name")
# openxlsx::saveWorkbook(wb, file = "iris.xlsx")
# }
condformat2excelsheet <- function(x, wb, sheet_name) {
  xv_cf <- get_xview_and_cf_fields(x)
  xview <- xv_cf[["xview"]]
  cf_fields <- xv_cf[["cf_fields"]]

  css_fields <- render_cf_fields_to_css_fields(cf_fields, xview)
  openxlsx::writeData(wb, sheet_name, as.data.frame(xview),
                      rowNames = FALSE, colNames = TRUE)
  for (css_key in names(css_fields)) {
    if (css_key == "background-color") {
      for (i in seq_len(nrow(xview))) {
        for (j in seq_len(ncol(xview))) {
          background_color <- ifelse(css_fields[["background-color"]][i,j] == "",
                                     NA,
                                     css_fields[["background-color"]][i,j])
          if (!is.na(background_color)) {
            sty <- openxlsx::createStyle(fgFill = background_color)
            openxlsx::addStyle(wb, sheet_name, style = sty, rows = i + 1, cols = j)
          }
        }
      }
    } else {
      warning("The CSS attribute ", css_key, " is not supported in xlsx format")
    }
  }
  invisible(x)
}

