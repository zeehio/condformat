# requireNamespace() is mocked selectively (only faking absence of the one
# package under test) rather than blanket-replaced, since shiny/promises
# themselves call requireNamespace() internally and a blind mock breaks that.
fake_missing <- function(missing_package) {
  real_rn <- base::requireNamespace
  function(package, ...) {
    if (identical(package, missing_package)) return(FALSE)
    real_rn(package, ...)
  }
}

test_that("condformatOutput returns an htmlTableWidgetOutput", {
  out <- condformatOutput("myid")
  expect_true(inherits(out, "shiny.tag.list"))
})

test_that("condformatOutput errors if shiny is missing", {
  local_mocked_bindings(requireNamespace = fake_missing("shiny"), .package = "base")
  expect_error(condformatOutput("id"), "shiny package required")
})

test_that("renderCondformat renders a condformat_tbl expression", {
  rc <- renderCondformat({ condformat(iris[1:3, ]) })
  expect_s3_class(rc, "shiny.render.function")
  result <- rc()
  expect_s3_class(result, "json")
})

test_that("renderCondformat handles a promise-returning expression", {
  skip_if_not_installed("promises")
  rc <- renderCondformat({ promises::promise_resolve(condformat(iris[1:3, ])) })
  result <- rc()
  expect_s3_class(result, "promise")
})

test_that("renderCondformat's promise branch errors if promises is missing", {
  skip_if_not_installed("promises")
  local_mocked_bindings(requireNamespace = fake_missing("promises"), .package = "base")
  rc <- renderCondformat({ promises::promise_resolve(condformat(iris[1:3, ])) })
  expect_error(rc(), "Please install the promises package")
})

test_that("renderCondformat errors if shiny is missing", {
  local_mocked_bindings(requireNamespace = fake_missing("shiny"), .package = "base")
  expect_error(renderCondformat(condformat(iris)), "shiny package required")
})

test_that("condformat_example errors if shiny is missing", {
  local_mocked_bindings(requireNamespace = fake_missing("shiny"), .package = "base")
  expect_error(condformat_example(), "shiny package required")
})
