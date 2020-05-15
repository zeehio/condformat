#' Selects the variables to be printed
#'
#' Keeps the variables you mention in the printed table.
#' Compared to \code{\link[dplyr]{select}}, show_columns does not remove the
#' columns from the data frame, so formatting rules can still depend
#' on them.
#' @param x A condformat object, typically created with [condformat()]
#' @param columns A character vector with column names to be to show. It can also be an expression
#'                can be used that will be parsed according to [tidyselect::language()]. See examples.
#' @param col_names Character vector with the column names for the selected columns
#'
#' @return The condformat object with the rule added
#' @examples
#'
#' data(iris)
#' x <- head(iris)
#'
#' # Include some columns:
#' cf <- condformat(x) %>% show_columns(c(Sepal.Length, Sepal.Width, Species))
#' \dontrun{
#' print(cf)
#' }
#' cf <- condformat(x) %>% show_columns(c("Sepal.Length", "Sepal.Width", "Species"))
#' \dontrun{
#' print(cf)
#' }
#'
#' # Rename columns:
#' cf <- condformat(x) %>%
#'   show_columns(c(Sepal.Length, Species),
#'                col_names = c("Length", "Spec."))
#' \dontrun{
#' print(cf)
#' }
#'
#' # Exclude some columns:
#' cf <- condformat(x) %>% show_columns(c(-Petal.Length, -Petal.Width))
#' \dontrun{
#' print(cf)
#' }
#'
#' cf <- condformat(x) %>% show_columns(c(starts_with("Petal"), Species))
#' \dontrun{
#' print(cf)
#' }
#'
#' petal_width <- "Petal.Width"
#' cf <- condformat(x) %>% show_columns(!! petal_width)
#' \dontrun{
#' print(cf)
#' }
#'
#' @export
#' @seealso \code{\link[dplyr]{select}}
show_columns <- function(x, columns, col_names) {
  if (!inherits(x, "condformat_tbl")) {
    x <- condformat(x)
  }

  columnsquo <- rlang::enquo(columns)

  if (missing(col_names)) {
    col_names <- NA
  }

  showobj <- structure(list(column_expr = columnsquo,
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
  columns <- tidyselect::eval_select(expr = showobj[["column_expr"]], data = x)
  columns <- rlang::set_names(names(columns))
  if (!identical(showobj[["col_names"]], NA)) {
    names(columns) <- showobj[["col_names"]]
  }
  # If a variable had already been excluded, do not show it:
  columns <- columns[columns %in% finalshow[["cols"]]]
  finalshow[["cols"]] <- columns
  return(finalshow)
}
