# Returns a matrix with the value of the CSS field for each element
# of our data frame.
#
# @param finalformat list with the css_fields
# @param field the name of the CSS field to be returned
# @return a matrix of size xview with the CSS field information
get_css_field <- function(finalformat, field) {
  if (!(field %in% names(finalformat$css_fields))) {
    field_matrix <- matrix(data = "",
                           nrow = nrow(finalformat$css_cell),
                           ncol = ncol(finalformat$css_cell))
  } else {
    field_matrix <- finalformat$css_fields[[field]]
  }
  return(field_matrix)
}

# If rule_locks is TRUE, locks the cells specified by mask
# so no further rules modify them
#
# @param rule_locks a logical. If FALSE, lock_cells does nothing
# @param mask a logical matrix. TRUE if the cell is affected by the rule
# @param finalformat The finalformat list, with the format options
#
# @return finalformat, the list with format options
#
lock_cells <- function(rule_locks, mask, finalformat) {
  if (identical(rule_locks, TRUE)) {
    finalformat$css_cell_unlocked[mask] <- FALSE
  }
  return(finalformat)
}

fill_css_field_by_cols <- function(finalformat, field, values, columns, xview, lockcells) {
  index.j <- match(columns, colnames(xview))
  index.j <- index.j[!is.na(index.j)]

  mask <- finalformat$css_cell_unlocked
  # mask == TRUE if cell can be changed, false otherwise
  # We can't change columns not affected:
  mask[,-index.j] <- FALSE

  backgr <- get_css_field(finalformat, field)
  backgr[mask] <- values[mask]

  finalformat$css_fields[[field]] <- backgr
  finalformat <- lock_cells(lockcells, mask, finalformat)
  return(finalformat)
}

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
  condformatopts$rules <- c(condformatopts$rules, list(rule))
  attr(x, "condformat") <- condformatopts
  x
}


api_dispatcher <- function(func_new, func_old) {
  # Deprecated
  call <- sys.call(-1)
  condformat_api <- "0.6"
  tryCatch({
    if (length(call) < 2) {
      stop("Unexpected error")
    }
    # Check if the first argument is a condformat_tbl object:
    x <- eval(call[[2]], envir = parent.frame(n = 2))
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
  return(eval(call, envir = parent.frame(n = 2)))
}
