test_that("condformat2excel errors with an install hint if openxlsx is missing", {
  testthat::local_mocked_bindings(requireNamespace = function(package, ...) FALSE, .package = "base")
  expect_error(
    condformat2excel(condformat(head(iris)), filename = tempfile()),
    regexp = 'openxlsx.*install\\.packages\\("openxlsx"\\)'
  )
})

test_that("condformat2excelsheet errors with an install hint if openxlsx is missing", {
  skip_if_not_installed("openxlsx")
  workbook <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(workbook, sheetName = "sheet1")
  testthat::local_mocked_bindings(requireNamespace = function(package, ...) FALSE, .package = "base")
  expect_error(
    condformat2excelsheet(condformat(head(iris)), workbook, "sheet1"),
    regexp = 'openxlsx.*install\\.packages\\("openxlsx"\\)'
  )
})

test_that("condformat2grob errors with an install hint if gridExtra is missing", {
  testthat::local_mocked_bindings(requireNamespace = function(package, ...) FALSE, .package = "base")
  expect_error(
    condformat2grob(condformat(head(iris)), draw = FALSE),
    regexp = 'gridExtra.*install\\.packages\\("gridExtra"\\)'
  )
})

test_that("knit_print errors with an install hint if rmarkdown is missing for LaTeX output", {
  skip_if_not_installed("knitr")
  testthat::local_mocked_bindings(
    is_latex_output = function(...) TRUE,
    .package = "knitr"
  )
  testthat::local_mocked_bindings(requireNamespace = function(package, ...) FALSE, .package = "base")
  expect_error(
    knitr::knit_print(condformat(head(iris))),
    regexp = 'rmarkdown.*install\\.packages\\("rmarkdown"\\)'
  )
})
