parse_columns_and_expression_ <- function(columns, expression) {
  # Deprecated
  if (is.factor(columns)) {
    columns <- as.character(columns)
  }
  if (is.character(expression)) {
    suggested_formula <- paste0("~ ", expression)
    expression <- stats::as.formula(suggested_formula)
  }

  if (!lazyeval::is_formula(expression)) { # D
    expression <- as.factor(expression)
  }

  if (lazyeval::is_formula(expression) && # D
      identical(lazyeval::f_rhs(expression), as.name("."))) { # D
    if (length(columns) > 1) {
      warning("rule applied to multiple columns, using the first given variable as expression")
    }
    lazyeval::f_rhs(expression) <- as.name(columns[1]) # D
  }
  return(list(columns = columns, expression = expression))
}

add_rule_to_condformat <- function(x, rule) {
  condformatopts <- attr(x, "condformat")
  condformatopts[["rules"]] <- c(condformatopts[["rules"]], list(rule))
  attr(x, "condformat") <- condformatopts
  x
}


api_dispatcher <- function(func_new, func_old) {
  # Deprecated
  call <- sys.call(-1)
  if (length(call) < 2) {
    stop("Function ", as.character(call[[1]]),
         " needs arguments. See ?", as.character(call[[1]]),
         " for more information", call. = FALSE)
  }
  condformat_api <- "0.6"
  tryCatch({
    # Check if the first argument is a condformat_tbl object:
    if ("x" %in% names(call)) {
      x <- eval.parent(call[["x"]], 2)
    } else {
      x <- eval.parent(call[[2]], 2)
    }
    stopifnot(inherits(x, "condformat_tbl") || inherits(x, "data.frame"))
    condformat_api <- "0.7"
  }, error = function(err) {
    condformat_api <- "0.6"
  })

  # Choose the right function for the given arguments
  if (condformat_api == "0.7") {
    call[[1]] <- func_new
  } else if (condformat_api == "0.6") {
    warning("This condformat syntax is deprecated. See ?",
            as.character(call[[1]]),
            " for more information", call. = FALSE)
    call[[1]] <- func_old
  } else {
    stop("Unknown condformat API")
  }
  # evaluate:
  return(eval.parent(call, 2))
}
