#' Writes the table to an Excel workbook
#'
#' @param x A condformat_tbl object
#' @param filename The xlsx file name.
#' @param sheet_name The name of the sheet where the table will be written
#' @param overwrite_wb logical to overwrite the whole workbook file
#' @param overwrite_sheet logical to overwrite the sheet
#' @seealso [condformat2excelsheet()], to write into a worksheet of an
#'   `openxlsx` workbook you're building yourself, so you can add more
#'   sheets or apply extra `openxlsx` formatting before saving.
#' @export
#'
condformat2excel <- function(x, filename, sheet_name = "Sheet1",
                             overwrite_wb = FALSE,
                             overwrite_sheet = TRUE) {

  if (!grepl(pattern = '\\.xlsx$', filename)) { # endsWith(filename, ".xlsx")
    filename <- paste0(filename, ".xlsx")
  }
  workbook <- NULL
  if (file.exists(filename) && identical(overwrite_wb, FALSE)) {
    if (identical(overwrite_sheet, FALSE)) {
      stop(paste0(
        "File ", filename, " already exists. Set overwrite_wb=TRUE ",
        "to overwrite the whole workbook or overwrite_sheet=TRUE to overwrite ",
        "just the the sheet ", sheet_name))
    }
    workbook <- openxlsx::loadWorkbook(filename)
  } else {
    workbook <- openxlsx::createWorkbook(creator = "")
  }

  if (sheet_name %in% names(workbook)) {
    if (overwrite_sheet) {
      openxlsx::removeWorksheet(wb = workbook, sheet = sheet_name)
      openxlsx::addWorksheet(workbook, sheetName = sheet_name)
    }
  } else {
    openxlsx::addWorksheet(workbook, sheetName = sheet_name)
  }
  condformat2excelsheet(x, workbook, sheet_name)
  openxlsx::saveWorkbook(
    workbook,
    file = filename,
    overwrite = overwrite_wb | overwrite_sheet
  )
  return(invisible(x))
}

#' Writes the table to a worksheet of an existing Excel workbook
#'
#' Unlike [condformat2excel()], this does not create the workbook or save it
#' to disk: you pass in an `openxlsx` workbook (and an already-added
#' worksheet) yourself, so you can add other sheets, or apply additional
#' `openxlsx` formatting, before saving it with [openxlsx::saveWorkbook()].
#'
#' [openxlsx::addStyle()] replaces any style already present at a cell
#' unless you pass `stack = TRUE`, in which case it merges the new style
#' with the existing one instead. Since this function already applies a
#' style (fill colour, bold, font colour) to every cell, remember to pass
#' `stack = TRUE` to your own `openxlsx::addStyle()` calls if you want your
#' formatting (e.g. a number format) to combine with condformat's, rather
#' than replacing it. See the example below.
#'
#' @param x A condformat object, typically created with [condformat()]
#' @param workbook An `openxlsx` Workbook object, as created with
#'   [openxlsx::createWorkbook()] or loaded with [openxlsx::loadWorkbook()]
#' @param sheet_name The name of a worksheet already present in `workbook`
#'   (for instance added with [openxlsx::addWorksheet()]) where the table
#'   will be written
#' @seealso [condformat2excel()], which creates the workbook, writes a single
#'   sheet with this function, and saves it to disk in one call.
#' @export
#'
#' @examples
#' data(iris)
#' cf <- condformat(iris[1:5, ]) |>
#'   rule_fill_gradient(Sepal.Width)
#' \dontrun{
#' workbook <- openxlsx::createWorkbook(creator = "")
#' openxlsx::addWorksheet(workbook, sheetName = "iris")
#' condformat2excelsheet(cf, workbook, "iris")
#' # Combine condformat's own fill colour on Sepal.Width (column 2) with a
#' # percentage number format, using stack = TRUE so it doesn't replace
#' # condformat's own styling:
#' openxlsx::addStyle(
#'   workbook, "iris",
#'   style = openxlsx::createStyle(numFmt = "0%"),
#'   rows = 2:6, cols = 2, stack = TRUE
#' )
#' openxlsx::saveWorkbook(workbook, file = "iris.xlsx", overwrite = TRUE)
#' }
condformat2excelsheet <- function(x, workbook, sheet_name) {
  xlsx_supported_rules <- c("rule_fill_discrete", "rule_fill_gradient",
                            "rule_fill_gradient2", "rule_text_bold", "rule_text_color")

  # Check for unsupported rules and warn accordingly:
  rules <- attr(x, "condformat")[["rules"]]
  rules_used <- unlist(
    lapply(rules, function(x) setdiff(class(x), "condformat_rule"))
  )
  rules_to_report <- setdiff(rules_used, xlsx_supported_rules)
  if (length(rules_to_report) > 0) {
    warning(paste0(
      "condformat2excel does not support the following rules: ",
      paste0(rules_to_report, collapse = ","))
    )
  }
  xv_cf <- get_xview_and_cf_fields(x)
  xview <- xv_cf[["xview"]]
  cf_fields <- xv_cf[["cf_fields"]]
  css_fields <- render_cf_fields_to_css_fields(cf_fields, xview)

  openxlsx::writeData(workbook, sheet_name, as.data.frame(xview),
                      rowNames = FALSE, colNames = TRUE)
  created_styles <- list()
  # For each cell
  for (i in seq_len(nrow(xview))) {
    for (j in seq_len(ncol(xview))) {
      # We build a string that contains all the style information
      # "hash_background-color:#FF0000;another-css-field:itsvalue;..."
      fields_vals <- vapply(
        rlang::set_names(rlang::names2(css_fields)),
        function(x) css_fields[[x]][i,j],
        FUN.VALUE = character(1L)
      )
      # Remove css keys with either missing values or "":
      fields_vals <- fields_vals[!is.na(fields_vals) & fields_vals != ""]
      style_hash <- paste0("hash_",
        paste(names(fields_vals), fields_vals, sep = ":", collapse = ";")
      )
      # If the same style has been defined in another cell, we just use it.
      # Otherwise we create the style object and we save it:
      if (style_hash %in% names(created_styles)) {
        cell_style <- created_styles[[style_hash]]
      } else {
        fgFill <- NULL
        if ("background-color" %in% names(fields_vals)) {
          fgFill <- fields_vals["background-color"]
        }
        textDecoration <- NULL
        if (grepl("bold", fields_vals["font-weight"], fixed = TRUE)) {
          textDecoration <- c(textDecoration, "bold")
        }
        fontColour <- NULL
        if ("color" %in% names(fields_vals)) {
          fontColour <- fields_vals["color"]
        }
        cell_style <- openxlsx::createStyle(
          fgFill = fgFill,
          textDecoration = textDecoration,
          fontColour = fontColour
        )
        created_styles[[style_hash]] <- cell_style
      }
      openxlsx::addStyle(workbook, sheet_name, style = cell_style, rows = i + 1, cols = j)
    }
  }
  openxlsx::setColWidths(workbook, sheet = sheet_name, cols = seq_len(ncol(xview)), widths = "auto")
  invisible(x)
}

