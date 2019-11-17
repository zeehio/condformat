#' Fill column with discrete colors
#'
#' Fills a column or columns of a data frame using a discrete
#' colour palette, based on an expression.
#'
#' @family rule
#' @param x A condformat object, typically created with [condformat()]
#' @param columns A character vector with column names to be coloured. Optionally
#'                [tidyselect::select_helpers()] can be used.
#' @param expression an expression to be evaluated with the data.
#'                   It should evaluate to a logical or an integer vector,
#'                   that will be used to determine which cells are to be coloured.
#' @param colours a character vector with colours as values and the expression
#'                possible results as names.
#' @inheritParams scales::hue_pal
#' @param na.value a character string with the CSS color to be used in missing values
#' @param lockcells logical value determining if no further rules should be applied to the affected cells.
#'
#' @return The condformat_tbl object, with the added formatting information
#' @examples
#' data(iris)
#' condformat(iris[c(1:5, 70:75, 120:125), ]) %>%
#'  rule_fill_discrete("Species", colours = c("setosa" = "red",
#'                                          "versicolor" = "blue",
#'                                          "virginica" = "green")) %>%
#'  rule_fill_discrete("Sepal.Length", expression = Sepal.Length > 4.6,
#'                     colours=c("TRUE"="red"))
#'
#' condformat(iris[c(1:5, 70:75, 120:125), ]) %>%
#'  rule_fill_discrete(c(starts_with("Sepal"), starts_with("Petal")),
#'                     expression = Sepal.Length > 4.6,
#'                     colours=c("TRUE"="red"))
#' @export
rule_fill_discrete <- function(x, columns, expression, colours = NA,
                               na.value = "#FFFFFF",
                               h = c(0, 360) + 15, c = 100, l = 65,
                               h.start = 0, direction = 1,
                               lockcells=FALSE) {
  if (!inherits(x, "condformat_tbl")) {
    x <- condformat(x)
  }
  columnsquo <- rlang::enquo(columns)
  helpers <- tidyselect::vars_select_helpers
  columnsquo_bur <- rlang::env_bury(columnsquo, !!! helpers)

  expr <- rlang::enquo(expression)
  rule <- structure(list(columns = columnsquo_bur,
                         expression = expr,
                         colours = force(colours),
                         h = force(h),
                         c = force(c), l = force(l),
                         h.start = force(h.start),
                         direction = force(direction),
                         na.value = force(na.value),
                         lockcells = force(lockcells)),
                    class = c("condformat_rule", "rule_fill_discrete"))
  x <- add_rule_to_condformat(x, rule)
  return(x)
}

rule_to_cf_field.rule_fill_discrete <- function(rule, xfiltered, xview, ...) {
  columns <- do.call(tidyselect::vars_select, c(list(colnames(xview)), rule[["columns"]]))
  if (length(columns) == 0) {
    return(NULL)
  }
  if (rlang::quo_is_missing(rule[["expression"]])) {
    if (length(columns) > 1) {
      warning("rule_fill_discrete applied to multiple columns, using column ",
              columns[1], " values as expression. In the future this behaviour will change,",
              "please use a explicit expression instead.",
              call. = FALSE)
    }
    rule[["expression"]] <- as.symbol(as.name(columns[1]))
  }
  values_determining_color <- as.factor(rlang::eval_tidy(rule[["expression"]], data = xfiltered))
  values_determining_color <- rep(values_determining_color, length.out = nrow(xfiltered))

  colours_for_values <- NA
  if (identical(rule[["colours"]], NA)) {
    # colours not given: Create a palette
    number_colours <- length(unique(values_determining_color))
    col_scale <- scales::hue_pal(h = rule[["h"]], c = rule[["c"]], l = rule[["l"]],
                                 h.start = rule[["h.start"]],
                                 direction = rule[["direction"]])(number_colours)
    colours_for_values <- col_scale[as.integer(values_determining_color)]
  } else if (is.character(rule[["colours"]])) {
    colours_for_values <- rule[["colours"]][match(values_determining_color, names(rule[["colours"]]))]
  } else if (is.function(rule[["colours"]])) {
    colours_for_values <- rule[["colours"]](values_determining_color)
    if (is.factor(colours_for_values)) {
      colours_for_values <- as.character(colours_for_values)
    }
  }
  colours_for_values[is.na(colours_for_values)] <- rule[["na.value"]]
  stopifnot(identical(length(colours_for_values), nrow(xview)))
  colours_for_values_mat <- matrix(NA,
                                   nrow = nrow(xview), ncol = ncol(xview),
                                   byrow = FALSE)
  colnames(colours_for_values_mat) <- colnames(xview)
  colours_for_values_mat[, columns] <- colours_for_values
  cf_field <- structure(list(css_key = "background-color",
                             css_values = colours_for_values_mat,
                             lock_cells = rule[["lockcells"]]),
                        class = c("cf_field_rule_fill_solid",
                                  "cf_field_css", "cf_field"))
  return(cf_field)
}

# Used by rule_fill_discrete, rule_fill_gradient and rule_fill_gradient2 functions
cf_field_to_latex.cf_field_rule_fill_solid <- function(cf_field, xview, unlocked) {
  colours <- cf_field[["css_values"]]
  to_lock <- !is.na(colours)
  colours[is.na(colours) | !unlocked] <- ""
  before <- colours
  # Convert colors  to hex strings:
  # c("green", "yellow", "#00FF00") to c("0000FF", "FFFF00", "00FF00")
  # leaving empty strings aside
  before[nchar(before) > 0] <- apply(
    grDevices::col2rgb(before[nchar(before) > 0]),
    MARGIN = 2,
    function(x) sprintf("\\cellcolor[HTML]{%02X%02X%02X}", x[1],x[2],x[3]))
  after <- matrix("", nrow = nrow(colours), ncol = ncol(colours))
  if (cf_field[["lock_cells"]]) {
    unlocked <- unlocked | to_lock
  }
  list(before = before, after = after, unlocked = unlocked)
}

# Used by rule_fill_discrete, rule_fill_gradient and rule_fill_gradient2 functions
cf_field_to_gtable.cf_field_rule_fill_solid <- function(cf_field, xview, gridobj, unlocked, has_rownames, has_colnames) {
  colours <- cf_field[["css_values"]]
  to_lock <- !is.na(colours)
  colours[is.na(colours) | !unlocked] <- ""

  row_col <- which(nchar(colours) > 0, arr.ind = TRUE)
  for (tocolor in seq_len(nrow(row_col))) {
    ind <- find_cell(gridobj,
                     as.integer(has_colnames) + row_col[tocolor, 1],
                     as.integer(has_rownames) + row_col[tocolor, 2],
                     name = "core-bg")
    fill <- grDevices::col2rgb(colours[row_col[tocolor, 1], row_col[tocolor, 2]])
    fill <- sprintf("#%02X%02X%02X", fill[1], fill[2], fill[3])
    gridobj$grobs[ind][[1]][["gp"]] <- grid::gpar(fill = fill)
  }
  list(gridobj = gridobj, unlocked = unlocked)
}
