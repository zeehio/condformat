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
  wb <- openxlsx::loadWorkbook(filename)
  expect_equal(names(wb), c("head_iris", "tail_iris"))
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
  wb <- openxlsx::loadWorkbook(filename)
  expect_equal(names(wb), c("iris_tail", "iris_head"))
  expect_equal(openxlsx::read.xlsx(filename, "iris_head"), iris_head)
})
