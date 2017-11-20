#' Give a color to the text according to some expression
#' @family rule
#' @inheritParams rule_fill_discrete
#' @param expression Condition that evaluates to color names for the rows where text should be colored
#'
#' @param na.color Color for missing values
#' @examples
#' data(iris)
#' condformat(iris[c(1:5, 51:55, 101:105),]) %>%
#'   rule_text_color(Species, expression = ifelse(Species == "setosa", "blue", ""))
#' @export
rule_text_color <- function(x, columns, expression,
                            na.color = "",
                            lockcells = FALSE) {
  columnsquo <- rlang::enquo(columns)
  helpers <- tidyselect::vars_select_helpers
  columnsquo_bur <- rlang::env_bury(columnsquo, !!! helpers)

  expr <- rlang::enquo(expression)

  rule <- structure(list(columns = columnsquo_bur,
                         expression = expr,
                         na.value = force(na.color),
                         lockcells = force(lockcells)),
                    class = c("condformat_rule", "rule_text_color"))

  x <- add_rule_to_condformat(x, rule)
  return(x)
}

rule_to_cf_field.rule_text_color <- function(rule, xfiltered, xview, ...) {
  columns <- tidyselect::vars_select(colnames(xview), !!! rule[["columns"]])
  if (length(columns) == 0) {
    return(NULL)
  }
  if (rlang::quo_is_missing(rule[["expression"]])) {
    if (length(columns) > 1) {
      warning("rule_text_color applied to multiple columns, using column ",
              columns[1], " values as expression. In the future this behaviour will change,",
              "please use a explicit expression instead.",
              call. = FALSE)
    }
    rule[["expression"]] <- as.symbol(as.name(columns[1]))
  }
  colors <- rlang::eval_tidy(rule[["expression"]], data = xfiltered)
  colors[is.na(colors)] <- rule[["na.value"]]
  stopifnot(identical(length(colors), nrow(xview)))
  # Recycle css values to fit all the columns:
  colors_mat <- matrix(NA, nrow = nrow(xview),
                       ncol = ncol(xview))
  colnames(colors_mat) <- colnames(xview)
  colors_mat[, columns] <- colors
  cf_field <- structure(list(css_key = "color",
                             css_values = colors_mat,
                             lock_cells = rule[["lockcells"]]),
                        class = c("cf_field_rule_text_color",
                                  "cf_field_css", "cf_field"))
  return(cf_field)
}


cf_field_to_latex.cf_field_rule_text_color <- function(cf_field, xview, unlocked) {
  # \textcolor[RGB]{0,255,0}{This text will appear green-colored}
  css_values <- cf_field[["css_values"]]
  to_lock <- !is.na(css_values)
  css_values[is.na(css_values) | !unlocked] <- ""
  before <- css_values
  before[nchar(css_values) > 0] <- apply(
    grDevices::col2rgb(before[nchar(css_values) > 0]),
    MARGIN = 2,
    function(x) sprintf("\\textcolor[RGB]{%d,%d,%d}{", x[1],x[2],x[3]))
  after <- css_values
  after[nchar(css_values) > 0 ] <- "}"

  if (cf_field[["lock_cells"]]) {
    unlocked <- unlocked | to_lock
  }
  list(before = before, after = after, unlocked = unlocked)
}

cf_field_to_gtable.cf_field_rule_text_color <- function(cf_field, xview, gridobj, unlocked, has_rownames, has_colnames) {
  css_values <- cf_field[["css_values"]]
  to_lock <- !is.na(css_values)
  css_values[is.na(css_values) | !unlocked] <- ""

  row_col <- which(nchar(css_values) > 0, arr.ind = TRUE)
  for (tocolor in seq_len(nrow(row_col))) {
    ind <- find_cell(gridobj,
                     as.integer(has_colnames) + row_col[tocolor, 1],
                     as.integer(has_rownames) + row_col[tocolor, 2],
                     name = "core-fg")
    gridobj$grobs[ind][[1]][["gp"]][["col"]] <- css_values[row_col[tocolor, 1], row_col[tocolor, 2]]
  }

  if (cf_field[["lock_cells"]]) {
    unlocked <- unlocked | to_lock
  }
  list(gridobj = gridobj, unlocked = unlocked)
}
