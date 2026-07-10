test_that("condformat2latex works", {
  data(iris)
  x <- condformat(head(iris, n = 2))
  out <- condformat2latex(x)
  expect_match(out, "Sepal.Length & Sepal.Width & Petal.Length & Petal.Width & Species")
})

test_that("condformat2latex does not use longtable if disabled", {
  data(iris)
  on.exit({
    knitr::opts_knit$set(rmarkdown.pandoc.to = NULL, out.format = NULL)
    knitr::opts_current$set(longtable = NULL)
  })
  knitr::opts_knit$set(out.format = "latex", rmarkdown.pandoc.to = "latex")
  knitr::opts_current$set(longtable = FALSE)
  out <- knitr::knit_print(condformat(head(iris)))
  expect_match(out[1], "tabular")
})

test_that("knitr latex uses longtable by default and adds its LaTeX dependency", {
  data(iris)
  on.exit({
    knitr::opts_knit$set(rmarkdown.pandoc.to = NULL, out.format = NULL)
    knitr::opts_current$set(longtable = NULL)
  })
  knitr::opts_knit$set(out.format = "latex", rmarkdown.pandoc.to = "latex")
  out <- knitr::knit_print(condformat(head(iris)))
  dep_names <- vapply(attr(out, "knit_meta"), function(dep) dep[["name"]], character(1))
  expect_true("longtable" %in% dep_names)
})

test_that("knitr latex returns LaTeX code", {
  data(iris)
  on.exit({
    knitr::opts_knit$set(rmarkdown.pandoc.to = NULL, out.format = NULL)
    knitr::opts_current$set(longtable = NULL)
  })
  knitr::opts_knit$set(out.format = "latex", rmarkdown.pandoc.to = "latex")
  out <- knitr::knit_print(condformat(head(iris)))
  expect_match(out[1], "Sepal.Length & Sepal.Width & Petal.Length & Petal.Width & Species")
})
