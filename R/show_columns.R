#' Selects the variables to be printed
#'
#' Keeps the variables you mention in the printed table.
#' Compared to \code{\link[dplyr]{select}}, show_columns does not remove the
#' columns from the data frame, so formatting rules can still depend
#' on them.
#' @param x A condformat object, typically created with `condformat(x)`
#' @param columns A character vector with column names to be coloured. An expression
#'                can be used that will be parsed like in `tidyselect::vars_select`
#' @param ... Dots are used to transition from the old syntax \code{\link{show_columns_old}} to the new one
#'
#' @return The condformat object with the rule added
#' @examples
#'
#' data(iris)
#' x <- head(iris)
#'
#' # Include some columns:
#' condformat(x) %>% show_columns(c(Sepal.Length, Sepal.Width, Species))
#'
#' # Rename columns:
#' condformat(x) %>%
#'   show_columns(c(Sepal.Length, Species),
#'                col_names = c("Length", "Spec."))
#'
#' # Exclude some columns:
#' condformat(x) %>% show_columns(c(-Petal.Length, -Petal.Width))
#'
#' condformat(x) %>% show_columns(c(starts_with("Petal"), Species))
#'
#' @export
#' @seealso \code{\link[dplyr]{select}}
show_columns <- function(...) {
  quoted_args <- rlang::quos(...)
  condformat_api <- "0.6"
  tryCatch({
    possible_condformat <- quoted_args[[1]]
    x <- rlang::eval_tidy(possible_condformat)
    stopifnot(inherits(x, "condformat_tbl"))
    condformat_api <- "0.7"
  }, error = function(err) {
    condformat_api <- "0.6"
  })
  if (condformat_api == "0.7") {
    return(show_columns_new(...))
  } else if (condformat_api == "0.6") {
    return(show_columns_old(...))
  } else {
    stop("Unknown condformat API")
  }
}

show_columns_new <- function(x, columns, col_names) {
  columnsquo <- rlang::enquo(columns)
  helpers <- tidyselect::vars_select_helpers
  columnsquo_bur <- rlang::env_bury(columnsquo, !!! helpers)

  if (missing(col_names)) {
    col_names <- NA
  }

  showobj <- structure(list(column_expr = columnsquo_bur,
                            col_names = col_names),
                            class = c("condformat_show_columns",
                                      "condformat_show_columns_select"))
  x2 <- x
  condformatopts <- attr(x2, "condformat")
  condformatopts$show$cols <- c(condformatopts$show$cols, list(showobj))
  attr(x2, "condformat") <- condformatopts
  return(x2)
}


#' Selects the variables to be printed
#'
#' Keeps the variables you mention in the printed table.
#' Compared to \code{\link[dplyr]{select}}, show_columns does not remove the
#' columns from the data frame, so formatting rules can still depend
#' on them.
#' @param ... Comma separated list of unquoted expressions
#' @param col_names Character vector with the column names for the selected columns
#'
#' @return A condformat_show_columns object, usually to be added to a condformat_tbl object
#' @examples
#'
#' library(dplyr) # for starts_with()
#' data(iris)
#' x <- head(iris)
#'
#' # Include some columns:
#' condformat(x) + show_columns(Sepal.Length, Sepal.Width, Species)
#'
#' # Rename columns:
#' condformat(x) + show_columns(Sepal.Length, Species, col_names = c("Length", "Spec."))
#'
#' # Exclude some columns:
#' condformat(x) + show_columns(-Petal.Length, -Petal.Width)
#'
#' # Select columns using dplyr syntax:
#' condformat(x) + show_columns(starts_with("Petal"), Species)
#'
#' @export
#' @seealso \code{\link[dplyr]{select}}
show_columns_old <- function(..., col_names) {
  if (missing(col_names)) {
    col_names = NA
  }
  show_columns_(.dots = lazyeval::lazy_dots(...), col_names = col_names)
}

#' Show columns (SE)
#'
#' @inheritParams show_columns
#' @param .dots A character vector with columns to show
#' @export
#' @examples
#'
#' data(iris)
#' x <- head(iris)
#' # Use standard evaluation (columns as strings):
#' condformat(x) +
#'   show_columns_(.dots = c("Sepal.Length", "Species"), col_names = c("Sepal Length", "Species"))
show_columns_ <- function(..., .dots, col_names) {
  warning("This condformat syntax is deprecated. See ?show_columns for more information", call. = FALSE)
  dots <- lazyeval::all_dots(.dots, ...)
  if (missing(col_names)) {
    col_names <- NA
  }
  showobj <- structure(list(column_expr = dots, col_names = col_names),
                       class = c("condformat_show_columns",
                                 "condformat_show_columns_select"))
  return(showobj)
}

render_show.condformat_show_columns_select <- function(showobj, finalshow, x, ...) {
  if (inherits(showobj$column_expr, "quosure")) {
    columns <- tidyselect::vars_select(colnames(x), !!! showobj$column_expr)
    if (!identical(showobj$col_names, NA)) {
      names(columns) <- showobj$col_names
    } else {
      names(columns) <- columns
    }
    # If a variable had already been excluded, do not show it:
    columns <- columns[columns %in% finalshow$cols]
    finalshow$cols <- columns
    return(finalshow)
  } else {
    # Deprecated
    # col_to_show: The columns that this show_columns would keep:
    col_to_show <- dplyr::select_vars_(colnames(x), showobj$column_expr) # D

    # Assign the names we want to use for those columns:
    if (!identical(showobj$col_names, NA)) {
      names(col_to_show) <- showobj$col_names
    } else {
      names(col_to_show) <- col_to_show
    }

    # If a variable had already been excluded, do not show it:
    col_to_show <- col_to_show[col_to_show %in% finalshow$cols]
    finalshow$cols <- col_to_show
    return(finalshow)
  }
}
