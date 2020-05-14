#' Fill column with a bar of a length proportional to a value
#'
#' Fills the background of a column cell using a bar proportional to the value
#' of the cell
#'
#' @family rule
#'
#' @param x A condformat object, typically created with [condformat()]
#' @param columns A character vector with column names to be coloured. Optionally
#'                [tidyselect::language()] can be used.
#' @param expression an expression to be evaluated with the data.
#'                   It should evaluate to a numeric vector,
#'                   that will be used to determine the colour gradient level.
#' @param low Colour for the beginning of the bar
#' @param high Colour for the end of the bar
#' @param background Background colour for the cell
#' @param na.value Colour for missing values
#' @param limits range of limits that the gradient should cover
#' @param lockcells logical value determining if no further rules should be applied to the affected cells.
#'
#' @return The condformat_tbl object, with the added formatting information
#' @examples
#' data(iris)
#' cf <- condformat(iris[c(1:5, 70:75, 120:125), ]) %>% rule_fill_bar("Sepal.Length")
#' \dontrun{
#' print(cf)
#' }
#' @export
rule_fill_bar <- function(x, columns, expression,
                          low = "darkgreen",
                          high = "white",
                          background = "white",
                          na.value = "gray",
                          limits = NA,
                          lockcells = FALSE) {
  if (!inherits(x, "condformat_tbl")) {
    x <- condformat(x)
  }
  columnsquo <- rlang::enquo(columns)
  helpers <- tidyselect::vars_select_helpers
  columnsquo_bur <- rlang::env_bury(columnsquo, !!! helpers)

  expr <- rlang::enquo(expression)
  rule <- structure(list(columns = columnsquo_bur,
                         expression = expr,
                         low = force(low),
                         high = force(high),
                         background = force(background),
                         na.value = force(na.value),
                         limits = force(limits),
                         lockcells = force(lockcells)),
                    class = c("condformat_rule", "rule_fill_bar"))
  x <- add_rule_to_condformat(x, rule)
  return(x)
}

#' @importFrom scales rescale
rule_to_cf_field.rule_fill_bar <- function(rule, xfiltered, xview, ...) {
  columns <- do.call(tidyselect::vars_select, c(list(colnames(xview)), rule[["columns"]]))
  if (length(columns) == 0) {
    return(NULL)
  }
  if (rlang::quo_is_missing(rule[["expression"]])) {
    if (length(columns) > 1) {
      warning("rule_fill_bar applied to multiple columns, using column ",
              columns[1], " values as expression. In the future this behaviour will change,",
              " please use a explicit expression instead.",
              call. = FALSE)
    }
    rule[["expression"]] <- as.symbol(as.name(columns[1]))
  }
  values_determining_color <- rlang::eval_tidy(rule[["expression"]], data = xfiltered)
  values_determining_color <- rep(values_determining_color, length.out = nrow(xfiltered))

  if (identical(rule[["limits"]], NA)) {
    limits <- range(values_determining_color, na.rm = TRUE)
  } else {
    limits <- rule[["limits"]]
    if (is.na(limits[1])) {
      limits[1] <- min(values_determining_color, na.rm = TRUE)
    }
    if (is.na(limits[2])) {
      limits[2] <- max(values_determining_color, na.rm = TRUE)
    }
  }

  values_rescaled <- scales::rescale(x = values_determining_color, from = limits)
  stopifnot(identical(length(values_rescaled), nrow(xview)))

  bar_width_percent <- matrix(NA,
                              nrow = nrow(xview), ncol = ncol(xview),
                              byrow = FALSE)
  colnames(bar_width_percent) <- colnames(xview)
  bar_width_percent[, columns] <- values_rescaled
  pbar_is_na <- matrix(NA,
                       nrow = nrow(xview), ncol = ncol(xview),
                       byrow = FALSE)
  colnames(pbar_is_na) <- colnames(xview)

  pbar_is_na[, columns] <- is.na(values_rescaled)

  col_low <- grDevices::col2rgb(rule[["low"]])
  col_high <- grDevices::col2rgb(rule[["high"]])
  col_background <- grDevices::col2rgb(rule[["background"]])
  col_na_value <- grDevices::col2rgb(rule[["na.value"]])

  cf_field <- structure(list(col_low = col_low,
                             col_high = col_high,
                             col_background = col_background,
                             col_na_value = col_na_value,
                             border = TRUE,
                             bar_width_percent = bar_width_percent,
                             pbar_is_na = pbar_is_na,
                             lock_cells = rule[["lockcells"]]),
                        class = c("cf_field_rule_bar_gradient",
                                  "cf_field"))
  return(cf_field)
}

# This is used by all CSS based rules
cf_field_to_css.cf_field_rule_bar_gradient <- function(cf_field, xview, css_fields, unlocked) {
  bar_width_percent <- cf_field[["bar_width_percent"]]

  mask <- unlocked
  # mask == TRUE if cell can be changed, false otherwise

  # if the css value is NA, ignore it as well
  # (so we don't override previous values)
  mask <- mask & !is.na(bar_width_percent)

  get_css_key <- function(css_fields, css_key, dims) {
    if (css_key %in% names(css_fields)) {
      prev_values <- css_fields[[css_key]]
    } else {
      prev_values <- matrix(NA, nrow = dims[1], ncol = dims[2])
    }
    return(prev_values)
  }

  border <- get_css_key(css_fields, "border", dim(xview))
  border[mask] <- "1px solid black"
  css_fields[["border"]] <- border


  pbar_is_na <- cf_field[["pbar_is_na"]]
  background_repeat <- get_css_key(css_fields, "background-repeat", dim(xview))
  background_repeat[mask & !pbar_is_na] <- "no-repeat"
  css_fields[["background-repeat"]] <- background_repeat

  col_low <- cf_field[["col_low"]]
  col_high <- cf_field[["col_high"]]
  col_na <- cf_field[["col_na_value"]]
  col_bg <- cf_field[["col_background"]]

  background_color <- get_css_key(css_fields, "background-color", dim(xview))
  background_color[mask & pbar_is_na] <- sprintf("#%02X%02X%02X", col_na[1], col_na[2], col_na[3])
  background_color[mask & !pbar_is_na] <- sprintf("#%02X%02X%02X", col_bg[1], col_bg[2], col_bg[3])
  css_fields[["background-color"]] <- background_color

  background_image <- get_css_key(css_fields, "background-image", dim(xview))
  background_image[mask & !pbar_is_na] <- sprintf(
    "linear-gradient(to right, rgba(%d, %d, %d, 1) 0%%, rgba(%d, %d, %d, 1) 100%%)",
    col_low[1], col_low[2], col_low[3], col_high[1], col_high[2], col_high[3])
  css_fields[["background-image"]] <- background_image

  background_size <- get_css_key(css_fields, "background-size", dim(xview))
  bg_size_val <- sprintf("%d%% 100%%", as.integer(round(100*cf_field[["bar_width_percent"]])))
  background_size[mask & !pbar_is_na] <- bg_size_val[mask & !pbar_is_na]
  css_fields[["background-size"]] <- background_size

  if (identical(cf_field[["lock_cells"]], TRUE)) {
    unlocked[mask] <- FALSE
  }
  return(list(css_fields = css_fields, unlocked = unlocked))
}

cf_field_to_gtable.cf_field_rule_bar_gradient <- function(
  cf_field, xview, gridobj, unlocked, has_rownames, has_colnames) {

  bar_width_percent <- cf_field[["bar_width_percent"]]

  mask <- unlocked
  # mask == TRUE if cell can be changed, false otherwise

  # if the css value is NA, ignore it as well
  # (so we don't override previous values)
  mask <- mask & !is.na(bar_width_percent)

  pbar_is_na <- cf_field[["pbar_is_na"]]
  col_low <- cf_field[["col_low"]]
  col_low <- sprintf("#%02X%02X%02X", col_low[1], col_low[2], col_low[3])
  col_high <- cf_field[["col_high"]]
  col_high <- sprintf("#%02X%02X%02X", col_high[1], col_high[2], col_high[3])
  col_na <- cf_field[["col_na_value"]]
  col_na <- sprintf("#%02X%02X%02X", col_na[1], col_na[2], col_na[3])
  col_bg <- cf_field[["col_background"]]
  col_bg <- sprintf("#%02X%02X%02X", col_bg[1], col_bg[2], col_bg[3])

  row_col <- which(!is.na(pbar_is_na), arr.ind = TRUE)
  for (tocolor in seq_len(nrow(row_col))) {
    ind <- find_cell(gridobj,
                     as.integer(has_colnames) + row_col[tocolor, 1],
                     as.integer(has_rownames) + row_col[tocolor, 2],
                     name = "core-bg")
    if (pbar_is_na[row_col[tocolor, 1], row_col[tocolor, 2]]) {
      gridobj$grobs[ind][[1]][["gp"]] <- grid::gpar(fill = col_na)
    } else {
      rect <- gridobj$grobs[ind][[1]]
      #grid::removeGrob(gridobj, rect$name)
      grad_levels <- 100
      fill_to <- round(100*bar_width_percent[row_col[tocolor, 1], row_col[tocolor, 2]])
      gp <- grid::gpar(col = NA,
                       fill = c(
                         grDevices::colorRampPalette(c(col_low, col_high), space = "Lab")(fill_to),
                         rep(col_bg, grad_levels - fill_to)))
      gridobj <- gtable::gtable_add_grob(gridobj,
                                         grobs = grid::rectGrob(
                                           x = grid::unit(seq(from = 0, by = 1/grad_levels, length.out = grad_levels), "npc"),
                                           y = grid::unit(0.5, "npc"),
                                           width = rep(grid::unit(1/grad_levels, "npc") -
                                                         grid::unit(2/grad_levels, "scaledpts"), grad_levels),
                                           height = grid::unit(1, "npc") - grid::unit(2, "scaledpts"),
                                           hjust = 0,
                                           vjust = 0.5,
                                           gp = gp),
                                         t = row_col[tocolor, 1] + has_colnames,
                                         b = row_col[tocolor, 1] + has_colnames,
                                         l = row_col[tocolor, 2] + has_rownames,
                                         r = row_col[tocolor, 2] + has_rownames,
                                         z = 0)
    }
  }

  if (identical(cf_field[["lock_cells"]], TRUE)) {
    unlocked[mask] <- FALSE
  }
  return(list(gridobj = gridobj, unlocked = unlocked))
}
