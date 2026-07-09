test_that("condformat2excel generates a file", {
  data(iris)
  filename <- tempfile(fileext = ".xlsx")
  rows_to_write <- 6
  on.exit(unlink(filename))
  out <- condformat2excel(condformat(head(iris, n = rows_to_write)), filename = filename)
  expect_true(file.exists(filename))
  expect_equal(nrow(openxlsx::read.xlsx(filename, 1)), rows_to_write)
})

test_that("condformat2excel can write multiple sheets", {
  data(iris)
  filename <- tempfile(fileext = ".xlsx")
  rows_to_write <- 6
  on.exit(unlink(filename))
  condformat2excel(
    condformat(head(iris, n = rows_to_write)),
    filename = filename,
    sheet_name = "head_iris"
  )
  condformat2excel(
    condformat(tail(iris, n = rows_to_write)),
    filename = filename,
    sheet_name = "tail_iris"
  )
  expect_true(file.exists(filename))
  workbook <- openxlsx::loadWorkbook(filename)
  expect_equal(names(workbook), c("head_iris", "tail_iris"))
  expect_equal(nrow(openxlsx::read.xlsx(filename, 1)), rows_to_write)
})

test_that("condformat2excel can overwrite a sheet without deleting others", {
  data(iris)
  rows_to_write <- 6
  iris$Species <- as.character(iris$Species)
  iris_head <- head(iris, n = rows_to_write)
  iris_tail <- tail(iris, n = rows_to_write)
  filename <- tempfile(fileext = ".xlsx")
  on.exit(unlink(filename))
  condformat2excel(
    condformat(iris_tail),
    filename = filename,
    sheet_name = "iris_tail"
  )
  condformat2excel(
    condformat(iris_tail),
    filename = filename,
    sheet_name = "iris_head"
  )
  condformat2excel(
    condformat(iris_head),
    filename = filename,
    sheet_name = "iris_head"
  )
  expect_true(file.exists(filename))
  workbook <- openxlsx::loadWorkbook(filename)
  expect_equal(names(workbook), c("iris_tail", "iris_head"))
  expect_equal(openxlsx::read.xlsx(filename, "iris_head"), iris_head)
})


test_that("condformat2excel warns if unsupported rule is used", {
  data(iris)
  rows_to_write <- 6
  filename <- tempfile(fileext = ".xlsx")
  on.exit(unlink(filename))
  expect_warning(
    condformat2excel(
      rule_fill_bar(condformat(head(iris)), "Sepal.Width"),
      filename = filename
    ),
    regexp = "condformat2excel does not support the following rules"
  )
})

test_that("condformat2excel appends .xlsx to the filename if missing", {
  base <- tempfile()
  filename <- paste0(base, ".xlsx")
  on.exit(unlink(filename))
  condformat2excel(condformat(head(iris)), filename = base)
  expect_true(file.exists(filename))
})

test_that("condformat2excel errors if the file exists and no overwrite is allowed", {
  filename <- tempfile(fileext = ".xlsx")
  on.exit(unlink(filename))
  condformat2excel(condformat(head(iris)), filename = filename)
  expect_error(
    condformat2excel(condformat(head(iris)), filename = filename,
                     overwrite_wb = FALSE, overwrite_sheet = FALSE),
    "already exists"
  )
})

test_that("condformat2excel overwrite_wb replaces the whole workbook", {
  filename <- tempfile(fileext = ".xlsx")
  on.exit(unlink(filename))
  condformat2excel(condformat(head(iris)), filename = filename, sheet_name = "first")
  condformat2excel(condformat(head(iris)), filename = filename, sheet_name = "second",
                   overwrite_wb = TRUE)
  workbook <- openxlsx::loadWorkbook(filename)
  expect_equal(names(workbook), "second")
})

test_that("condformat2excel applies and caches fill/bold/color styles across repeated rows", {
  filename <- tempfile(fileext = ".xlsx")
  on.exit(unlink(filename))
  x <- data.frame(a = rep(c("Dog", "Cat"), 3)) |>
    condformat() |>
    rule_fill_discrete("a", colours = c("Dog" = "#FF0000", "Cat" = "#00FF00")) |>
    rule_text_bold("a", expression = a == "Dog") |>
    rule_text_color("a", expression = ifelse(a == "Dog", "blue", "purple"))
  condformat2excel(x, filename = filename)
  expect_true(file.exists(filename))
  workbook <- openxlsx::loadWorkbook(filename)
  expect_true(length(workbook$styleObjects) > 0)
  expect_equal(nrow(openxlsx::read.xlsx(filename, 1)), 6)
})

test_that("condformat2excel preserves openxlsx's automatic Date column format", {
  # openxlsx::writeData() auto-applies a Date number format to Date columns.
  # condformat2excelsheet's own per-cell styling (applied to every cell, even
  # ones with no rule) must not silently reset it back to General.
  df <- data.frame(
    id = 1:3,
    d = as.Date(c("2024-01-01", "2024-06-15", "2024-12-31")),
    grp = c("a", "b", "a")
  )
  cf <- condformat(df) |> rule_fill_discrete("grp")
  filename <- tempfile(fileext = ".xlsx")
  on.exit(unlink(filename))
  condformat2excel(cf, filename = filename)
  workbook <- openxlsx::loadWorkbook(filename)
  date_col <- which(names(df) == "d")
  date_styles <- Filter(function(s) date_col %in% s$cols, workbook$styleObjects)
  expect_true(length(date_styles) > 0)
  expect_true(all(vapply(date_styles, function(s) {
    !is.null(s$style$numFmt) && identical(s$style$numFmt$numFmtId, "14")
  }, logical(1))))
})

test_that("condformat2excelsheet merges with, rather than replaces, pre-existing cell styles", {
  # In-memory workbook$styleObjects can contain multiple overlapping, not yet
  # reconciled entries for the same cell; only a save+reload round trip
  # reflects what actually ends up rendered, so we check that.
  df <- data.frame(id = 1:3, note = c("a", "b", "c"))
  cf <- condformat(df) # no rules at all: every cell gets an "empty" style
  workbook <- openxlsx::createWorkbook(creator = "")
  openxlsx::addWorksheet(workbook, sheetName = "sheet1")
  # Simulate a user pre-applying their own formatting before calling
  # condformat2excelsheet, e.g. a currency-like number format on "id":
  openxlsx::addStyle(
    workbook, "sheet1",
    style = openxlsx::createStyle(numFmt = "$#,##0.00"),
    rows = 2:4, cols = 1
  )
  condformat2excelsheet(cf, workbook, "sheet1")
  filename <- tempfile(fileext = ".xlsx")
  on.exit(unlink(filename))
  openxlsx::saveWorkbook(workbook, file = filename, overwrite = TRUE)
  reloaded <- openxlsx::loadWorkbook(filename)
  id_styles <- Filter(function(s) 1 %in% s$cols, reloaded$styleObjects)
  expect_true(length(id_styles) > 0)
  expect_true(all(vapply(id_styles, function(s) {
    !is.null(s$style$numFmt) && identical(s$style$numFmt$formatCode, "$#,##0.00")
  }, logical(1))))
})
