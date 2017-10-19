# Tests:
context("show column")

test_that("show_column works (0.6 API)", {
  data(iris)
  expect_warning(rs <- show_columns(-Sepal.Length))
  x <- condformat(head(iris)) + rs
  expect_true("Sepal.Length" %in% colnames(x))
  out <- as.character(condformat2html(x))
  expect_failure(expect_match(out, "Sepal.Length"))
  expect_match(out, "Sepal.Width")
  expect_match(out, "Petal.Length")
  expect_match(out, "Petal.Width")
  expect_match(out, "Species")
})


test_that("show_column works", {
  data(iris)
  x <- condformat(head(iris, 1)) %>% show_columns(-Sepal.Length)
  expect_true("Sepal.Length" %in% colnames(x))
  out <- as.character(condformat2html(x))
  expect_failure(expect_match(out, "Sepal.Length"))
  expect_match(out, "Sepal.Width")
  expect_match(out, "Petal.Length")
  expect_match(out, "Petal.Width")
  expect_match(out, "Species")
})

test_that("show_column_ works (0.6 API)", {
  data(iris)
  expect_warning(rs <- show_columns_(.dots = c("Sepal.Length", "Petal.Length")))
  x <- condformat(head(iris)) + rs
  expect_true("Sepal.Width" %in% colnames(x))
  out <- condformat2html(x)
  expect_failure(expect_match(out, "Sepal.Width"))
  expect_match(out, "Petal.Length")
})

test_that("show_column works with custom names (0.6 API)", {
  data(iris)
  expect_warning(r1 <- show_columns(Sepal.Length, Petal.Width, Species,
                                    col_names = c("MySepLen", "MyPetWi", "MySpe")))
  x <- condformat(head(iris)) + r1
  expect_true("Sepal.Length" %in% colnames(x))
  expect_true("Petal.Length" %in% colnames(x))
  out <- condformat2html(x)
  for (col in c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")) {
    expect_failure(expect_match(out, col))
  }
  expect_match(out, "MySepLen")
  expect_match(out, "MyPetWi")
  expect_match(out, "MySpe")
})

test_that("show_column works with custom names", {
  data(iris)
  x <- condformat(head(iris)) %>% show_columns(c(Sepal.Length, Petal.Width, Species),
                                             col_names = c("MySepLen", "MyPetWi", "MySpe"))
  expect_true("Sepal.Length" %in% colnames(x))
  expect_true("Petal.Length" %in% colnames(x))
  out <- condformat2html(x)
  for (col in c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")) {
    expect_failure(expect_match(out, col))
  }
  expect_match(out, "MySepLen")
  expect_match(out, "MyPetWi")
  expect_match(out, "MySpe")
})


context("show row")

test_that("show_row works (old API)", {
  data(iris)
  expect_warning(
    rs <- show_rows(Sepal.Length == 5.1, Sepal.Width == 3.5,
                    Petal.Length == 1.4, Petal.Width == 0.2))
  x <- condformat(head(iris, n = 10)) + rs
  # in the data frame nothing is filtered
  expect_equal(nrow(x), 10)
  out <- condformat2html(x)
  # the html code only shows one row (that does not have any 8 digit)
  expect_failure(expect_match(out, "8"))
})

test_that("show_row works", {
  data(iris)
  x <- condformat(head(iris, n = 10)) %>%
    show_rows(Sepal.Length == 5.1, Sepal.Width == 3.5,
              Petal.Length == 1.4, Petal.Width == 0.2)
  # in the data frame nothing is filtered
  expect_equal(nrow(x), 10)
  out <- condformat2html(x)
  # the html code only shows one row (that does not have any 8 digit)
  expect_failure(expect_match(out, "8"))
})

test_that("show_row works with strings, thanks to rlang", {
  data(iris)
  x <- condformat(head(iris, n = 10)) %>%
    show_rows(!! rlang::parse_quosure("Sepal.Length == 5.1"),
              !! rlang::parse_quosure("Sepal.Width == 3.5"),
              !! rlang::parse_quosure("Petal.Length == 1.4"),
              !! rlang::parse_quosure("Petal.Width == 0.2"))
  # in the data frame nothing is filtered
  expect_equal(nrow(x), 10)
  out <- condformat2html(x)
  # the html code only shows one row (that does not have any 8 digit)
  expect_failure(expect_match(out, "8"))
})


test_that("show_row works after modifying data frame", {
  data(iris)
  x <- condformat(head(iris, n = 10))
  x$Sepal.Length <- x$Sepal.Length + 1
  x <- x %>% show_rows(Sepal.Length == 6.1, Sepal.Width == 3.5,
                       Petal.Length == 1.4, Petal.Width == 0.2)
  # in the data frame nothing is filtered
  expect_equal(nrow(x), 10)
  out <- condformat2html(x)
  # the html code only shows one row (that does not have any 8 digit)
  expect_failure(expect_match(out, "8"))
})


test_that("show_row works after modifying data frame (old API)", {
  data(iris)
  x <- condformat(head(iris, n = 10))
  x$Sepal.Length <- x$Sepal.Length + 1
  expect_warning(
    rs <- show_rows(Sepal.Length == 6.1, Sepal.Width == 3.5,
                    Petal.Length == 1.4, Petal.Width == 0.2))
  x <- x + rs
  # in the data frame nothing is filtered
  expect_equal(nrow(x), 10)
  out <- condformat2html(x)
  # the html code only shows one row (that does not have any 8 digit)
  expect_failure(expect_match(out, "8"))
})

test_that("custom show_ passes doing nothing", {
    custom_showobj <- structure(list(),
                                class = c("condformat_show_columns"))
    data(iris)
    x <- condformat(head(iris))
    y <- x + custom_showobj
    out_x <- condformat2html(x)
    out_y <- condformat2html(y)
    expect_identical(out_x, out_y)
})

