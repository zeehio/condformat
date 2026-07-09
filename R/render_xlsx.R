#' Writes the table to an Excel workbook
#'
#' Requires the `openxlsx` package (`install.packages("openxlsx")`).
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
  require_suggested_package("openxlsx", "condformat2excel() and condformat2excelsheet()")

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
#' Requires the `openxlsx` package (`install.packages("openxlsx")`).
#'
#' Unlike [condformat2excel()], this does not create the workbook or save it
#' to disk: you pass in an `openxlsx` workbook (and an already-added
#' worksheet) yourself, so you can add other sheets, or apply additional
#' `openxlsx` formatting, before saving it with [openxlsx::saveWorkbook()].
#'
#' This function applies its own styling (fill colour, bold, font colour) to
#' every cell using `stack = TRUE`, so it merges with, rather than replaces,
#' any formatting you already applied (e.g. a number format on a Date column,
#' set either by you or automatically by [openxlsx::writeData()]).
#'
#' [openxlsx::addStyle()] itself defaults to replacing, not merging, any
#' style already present at a cell. So if you add your own `openxlsx`
#' formatting *after* calling this function, remember to pass
#' `stack = TRUE` to your own call too, or it will silently replace
#' condformat's own styling instead of combining with it. See the example
#' below.
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
  require_suggested_package("openxlsx", "condformat2excel() and condformat2excelsheet()")
  xlsx_supported_rules <- c("rule_fill_discrete", "rule_fill_gradient",
                            "rule_fill_gradient2", "rule_text_bold", "rule_text_color",
                            "rule_fill_bar")

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
  xfiltered <- xv_cf[["xfiltered"]]
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
      openxlsx::addStyle(workbook, sheet_name, style = cell_style, rows = i + 1, cols = j, stack = TRUE)
    }
  }
  for (rule in rules) {
    if (inherits(rule, "rule_fill_bar")) {
      add_rule_fill_bar_databars(rule, xview, xfiltered, workbook, sheet_name)
    }
  }
  openxlsx::setColWidths(workbook, sheet = sheet_name, cols = seq_len(ncol(xview)), widths = "auto")
  invisible(x)
}

# rule_fill_bar()'s gradient bar has no solid-fill CSS equivalent (it's a
# `background-image: linear-gradient(...)`), which is why the per-cell CSS
# loop above only carries over its flat `background`/`na.value` colours.
# Excel has an actual native "data bar" conditional formatting feature, which
# we use here instead -- but unlike a CSS/gtable bar, it always reflects the
# cell's own displayed value against a range, not an arbitrary expression, so
# we only add it when `expression` is the default (missing or `.col`).
#
# Excel data bars are solid-coloured, not a `low`-to-`high` blend along their
# length, so only `low` is used as the bar's colour (`high` only affects
# CSS/gtable output). `openxlsx::conditionalFormatting(type = "databar")`
# also requires `style` and `rule` (the numeric range) to have the same
# length, but when `style` has 2 colours it silently keeps only the second
# one -- so `style` is passed as `low` repeated twice, to specify an
# explicit numeric range while still ending up with `low` as the one colour
# that's actually used.
add_rule_fill_bar_databars <- function(rule, xview, xfiltered, workbook, sheet_name) {
  columns <- tidyselect::eval_select(expr = rule[["columns"]], data = xview)
  if (length(columns) == 0) {
    return(invisible(NULL))
  }
  expr_is_default_col <- rlang::quo_is_missing(rule[["expression"]]) ||
    identical(rlang::quo_get_expr(rule[["expression"]]), quote(.col))
  if (!expr_is_default_col) {
    warning(paste0(
      "condformat2excel: rule_fill_bar() with a custom `expression` only ",
      "gets its background/na.value colours applied in Excel output; the ",
      "data bar itself is skipped, since Excel data bars always reflect a ",
      "cell's own displayed value, not an arbitrary expression."
    ))
    return(invisible(NULL))
  }
  for (col_name in names(columns)) {
    values <- xfiltered[[col_name]]
    if (all(is.na(values))) {
      next
    }
    # openxlsx::conditionalFormatting() always applies to the full
    # min(rows):max(rows) span, so there's no way to exclude individual NA
    # rows from the middle of the range here. That's fine: writeData() writes
    # NA as a genuinely blank cell (no value at all), and Excel's data bars
    # skip blank cells natively, so they're simply not drawn a bar.
    limits <- resolve_limits(values, rule[["limits"]])
    openxlsx::conditionalFormatting(
      workbook, sheet_name,
      cols = columns[[col_name]],
      rows = seq_len(nrow(xview)) + 1L, # +1 for the header row
      type = "databar",
      style = rep(rule[["low"]], 2),
      rule = limits
    )
  }
  invisible(NULL)
}

