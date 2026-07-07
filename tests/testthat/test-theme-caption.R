test_that("theme_caption works with HTML output", {
  out <- data.frame(a = 1) %>%
    condformat() %>%
    theme_caption("My Caption") %>%
    condformat2html()
  expect_match(out, "My Caption", fixed = TRUE)
})

test_that("theme_caption works with LaTeX output", {
  out <- data.frame(a = 1) %>%
    condformat() %>%
    theme_caption("My Caption") %>%
    condformat2latex()
  expect_match(out, "My Caption", fixed = TRUE)
})

test_that("theme_caption's last chained call wins", {
  out <- data.frame(a = 1) %>%
    condformat() %>%
    theme_caption("First") %>%
    theme_caption("Second") %>%
    condformat2html()
  expect_false(grepl("First", out, fixed = TRUE))
  expect_match(out, "Second", fixed = TRUE)
})
