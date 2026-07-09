#' Pipe operator (soft-deprecated re-export)
#'
#' condformat re-exports magrittr's `%>%` for backward compatibility with existing code that relies on `library(condformat)` making it available (condformat's own code uses the native pipe, `|>`, internally, since it requires R 4.1 or newer). Using it through condformat is deprecated: it emits a one-time-per-session warning, then behaves exactly like [magrittr::%>%]. Please import `%>%` from magrittr or dplyr yourself, or switch to `|>`, since this re-export will be removed in a future release.
#'
#' @param lhs A value, or the magrittr placeholder `.`
#' @param rhs A function call using magrittr semantics
#' @return The result of piping `lhs` into `rhs`, per [magrittr::%>%]
#' @name pipe
#' @rdname pipe
#' @export
`%>%` <- function(lhs, rhs) {
  rlang::warn(
    paste0(
      "The `%>%` pipe re-exported by condformat is deprecated and will be ",
      "removed in a future release. Import it from magrittr or dplyr ",
      "yourself, or use the native pipe (`|>`) instead."
    ),
    .frequency = "once",
    .frequency_id = "condformat_pipe_deprecated"
  )
  # Reconstruct the call against magrittr's own %>% and evaluate it in the
  # caller's frame, rather than just forwarding the (already-matched) lhs/rhs
  # promises: magrittr's %>% inspects the *unevaluated* call itself (for the
  # `.` placeholder and lazy right-hand-side evaluation), so calling it
  # normally here would have it see this wrapper's own formals instead of
  # the original, literal expressions the caller wrote.
  call <- sys.call()
  call[[1]] <- quote(magrittr::`%>%`)
  eval(call, envir = parent.frame())
}
