#' Selects the variables to be printed
#'
#' Keeps the variables you mention in the printed table.
#' Compared to \code{\link[dplyr]{select}}, show_columns does not remove the
#' columns from the data frame, so formatting rules can still depend
#' on them.
#' @param x A condformat object, typically created with [condformat()]
#' @param columns A character vector with column names to be to show. It can also be an expression
#'                can be used that will be parsed like in [tidyselect::vars_select()]. See examples.
#' @param col_names Character vector with the column names for the selected columns
#'
#' @return The condformat object with the rule added
#' @examples
#'
#' data(iris)
#' x <- head(iris)
#'
#' # Include some columns:
#' condformat(x) %>% show_columns(c(Sepal.Length, Sepal.Width, Species))
#' condformat(x) %>% show_columns(c("Sepal.Length", "Sepal.Width", "Species"))
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
#' petal_width <- "Petal.Width"
#' condformat(x) %>% show_columns(!! petal_width)
#'
#' @export
#' @seealso \code{\link[dplyr]{select}}
show_columns <- function(x, columns, col_names) {
  if (!inherits(x, "condformat_tbl")) {
    x <- condformat(x)
  }

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
  condformatopts[[c("show", "cols")]] <- c(condformatopts[[c("show", "cols")]], list(showobj))
  attr(x2, "condformat") <- condformatopts
  return(x2)
}

render_show.condformat_show_columns_select <- function(showobj, finalshow, x, ...) {
  columns <- tidyselect::vars_select(colnames(x), !!! showobj[["column_expr"]])
  if (!identical(showobj[["col_names"]], NA)) {
    names(columns) <- showobj[["col_names"]]
  } else {
    names(columns) <- columns
  }
  # If a variable had already been excluded, do not show it:
  columns <- columns[columns %in% finalshow[["cols"]]]
  finalshow[["cols"]] <- columns
  return(finalshow)
}
