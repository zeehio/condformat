#' Shiny bindings for condformat
#'
#' Output and render functions for using condformat within Shiny
#' applications and interactive Rmd documents.
#'
#'
#' @param outputId output variable to read from
#' @param expr An expression that generates a condformat object
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @param ... arguments passed to htmlOutput
#'
#' @name condformat-shiny
#'
#' @export
condformatOutput <- function(outputId, ...) {
  if (!requireNamespace("shiny")) {
    stop("shiny package required. Please install it.")
  }
  htmlTable::htmlTableWidgetOutput(outputId = outputId, ...)
}

require_promises <- function() {
  if (!requireNamespace("promises", quietly = TRUE)) {
    stop("Please install the promises package in order to use promises with shiny")
  }
}


#' @rdname condformat-shiny
#' @export
renderCondformat <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!requireNamespace("shiny")) {
    stop("shiny package required. Please install it")
  }
  func <- shiny::exprToFunction(expr, env, quoted)
  renderFunc <- function() {
    condformatobj <- func()
    if (inherits(condformatobj, "condformat_tbl")) {
      y <- condformat2widget(condformatobj)
    } else if (inherits(condformatobj, "promise")) {
      require_promises()
      y <- promises::then(condformatobj, condformat2widget)
    }
    y
  }
  htmlTable::renderHtmlTableWidget(expr = renderFunc())
}

#' @inheritParams shiny::runApp
#' @rdname condformat-shiny
#' @export
condformat_example <- function(display.mode = "normal") {
  if (!requireNamespace("shiny")) {
    stop("shiny package required. Please install it")
  }
  appDir <- system.file("shinyexample", package = "condformat")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `condformat`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = display.mode)
}
