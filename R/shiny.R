#' Shiny bindings for condformat
#'
#' Output and render functions for using condformat within Shiny
#' applications and interactive Rmd documents.
#'
#' Pagination will be used if a recent enough htmlTable package is available.
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
  if (utils::packageVersion("htmlTable") > "1.8") {
    htmlTable::htmlTableWidgetOutput(outputId = outputId, ...)
  } else {
    shiny::htmlOutput(outputId = outputId, ...)
  }
}

#' @rdname condformat-shiny
#' @export
renderCondformat <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!requireNamespace("shiny")) {
    stop("shiny package required. Please install it")
  }
  if (utils::packageVersion("htmlTable") > "1.8") {
    func <- NULL
    shiny::installExprFunction(expr, "func", env, quoted)
    renderFunc <- function() {
      condformatobj <- func()
      condformat2widget(condformatobj)
    }
    htmlTable::renderHtmlTableWidget(expr = renderFunc())
  } else {
    func <- NULL
    shiny::installExprFunction(expr, "func", env, quoted)
    renderFunc <- function() {
      condformatobj <- func()
      shiny::HTML(as.character(condformat2html(condformatobj)))
    }
    shiny::markRenderFunction(condformatOutput, renderFunc)
  }
}
