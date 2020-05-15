test_that("basic condformat works", {
  # Blocked by: https://github.com/r-lib/vdiffr/pull/37
  #skip_if_not_installed("vdiffr")
  cf <- condformat(iris[c(1:5,70:75, 120:125),])
  cfg <- condformat2grob(cf)
  expect_equal(nrow(cfg), 18)
  expect_equal(length(cfg), 216)
  #vdiffr::expect_doppelganger(title = "Basic condformat image", cfg)
})
