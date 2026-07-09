# The "warn once per session" state is tracked in a package-level rlang
# environment, so it must be reset before asserting the warning fires,
# regardless of what earlier tests (or example runs) in this same R
# session may already have triggered.
reset_pipe_warning_state <- function() {
  freq_env <- get("warning_freq_env", envir = asNamespace("rlang"))
  if (exists("condformat_pipe_deprecated", envir = freq_env, inherits = FALSE)) {
    rm("condformat_pipe_deprecated", envir = freq_env)
  }
}

test_that("%>% pipes a value into a function call", {
  reset_pipe_warning_state()
  result <- suppressWarnings(5 %>% sqrt())
  expect_equal(result, sqrt(5))
})

test_that("%>% preserves magrittr's `.` placeholder semantics", {
  reset_pipe_warning_state()
  result <- suppressWarnings(5 %>% seq(1, .))
  expect_equal(result, 1:5)
})

test_that("%>% chains correctly", {
  reset_pipe_warning_state()
  result <- suppressWarnings(c(4, 9, 16) %>% sqrt() %>% sum())
  expect_equal(result, 2 + 3 + 4)
})

test_that("%>% warns once per session, not on every call", {
  reset_pipe_warning_state()
  expect_warning(5 %>% sqrt(), "deprecated")
  expect_no_warning(5 %>% sqrt())
})
