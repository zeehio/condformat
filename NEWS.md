# condformat 0.10.1.9000

## Breaking changes

* Drop the `magrittr` dependency and the `%>%` pipe operator it re-exported
  (part of #43). condformat now requires R >= 4.1 and its own examples,
  tests and vignette use the native base R pipe (`|>`) instead. If your code
  relied on `library(condformat)` making `%>%` available, either
  `library(magrittr)` yourself or switch to `|>`.

## Dependencies

* Bump the minimum required version of all versioned dependencies to the
  release that was current about a year ago, rather than today's exact
  latest release, so users aren't forced onto a brand new patch just to
  install condformat (part of #43): `dplyr` (>= 1.1.4), `gtable` (>= 0.3.6),
  `htmlTable` (>= 2.4.3), `htmltools` (>= 0.5.8.1), `knitr` (>= 1.50),
  `openxlsx` (>= 4.2.8), `rmarkdown` (>= 2.29), `rlang` (>= 1.1.6),
  `scales` (>= 1.4.0), `tibble` (>= 3.3.0), `tidyselect` (>= 1.2.1),
  `shiny` (>= 1.11.1), `testthat` (>= 3.2.3) and `vdiffr` (>= 1.0.8).
  `gridExtra`'s floor (>= 2.3) is unchanged, as that release is still the
  latest on CRAN as of a year ago.

## New features

* Export `condformat2excelsheet()`, which writes a condformat table into a
  worksheet of an `openxlsx` workbook you're already building, without
  creating or saving the workbook itself (closes #25). Its docs show how to
  combine condformat's own cell styling with your own `openxlsx::addStyle()`
  calls (e.g. a number format) via `stack = TRUE`, since `addStyle()`
  replaces existing styles by default.
* Add a `.col` pronoun usable in the `expression` argument of `rule_fill_discrete()`,
  `rule_fill_gradient()`, `rule_fill_gradient2()`, `rule_fill_bar()`, `rule_text_bold()`,
  `rule_text_color()` and `rule_css()`. When `columns` selects more than one column,
  `.col` is bound to each column's own values in turn, so a single rule call can
  format several columns based on each one's own condition (closes #19):
  `rule_fill_discrete(c(Sepal.Length, Sepal.Width), .col > 3)` is now equivalent to
  chaining the rule once per column. `expression` now also defaults to `.col` when
  omitted, replacing the previous behaviour of silently using only the first
  selected column (with a warning) when no expression was given for a multi-column
  selection.

## Fixes

* Fix `condformat2excel()`/`condformat2excelsheet()` silently discarding any
  pre-existing cell formatting on every export — including a Date column's
  automatic format from `openxlsx::writeData()` — even for cells with no
  condformat rule applied to them. `condformat2excelsheet()`'s own styling
  now uses `stack = TRUE` internally, so it merges with, rather than
  replaces, formatting that was already there (related to #25).
* Fix `lockcells = TRUE` being ignored (or worse, unlocking cells) for LaTeX
  and grob/gtable output in `rule_fill_discrete()`, `rule_fill_gradient()`,
  `rule_fill_gradient2()`, `rule_text_bold()` and `rule_text_color()`. HTML
  and Excel output were not affected.
* Fix `theme_htmlTable()` discarding arguments accumulated from earlier
  `theme_htmlTable()` calls in the same pipeline, due to a `%in%` check
  against list values instead of list names.
* Fix `rule_fill_bar()`'s `na.value` never being applied to `background-color`
  in HTML output: the CSS mask used to gate that assignment already excluded
  NA cells, so the assignment was dead code.
* Fix `rule_fill_bar()` erroring in grob/gtable output when a value rescaled
  to exactly 0% of the bar width (`colorRampPalette()` can't return zero
  colours); such cells now render with just the background colour.
* Fix `rule_fill_bar()`'s `lockcells = TRUE` not protecting NA cells from
  later CSS/HTML rules, and not protecting any cell (NA or not) from later
  grob/gtable rules: the grob/gtable renderer painted every targeted cell
  unconditionally and never checked incoming lock state at all.
* Fix `rule_text_bold()`, `rule_text_color()` and `rule_css()` erroring when
  `expression` evaluates to a single (scalar) value on a table with more than
  one row, unlike `rule_fill_discrete()`, `rule_fill_gradient()`,
  `rule_fill_gradient2()` and `rule_fill_bar()`, which already recycled such
  values to every row.

## Other

* Fix the bundled shiny example (`inst/shinyexample/`): `requireNamespace()`
  now passes `quietly = TRUE`, so it no longer prints a package-startup
  message every time the optional `colourpicker` dependency isn't installed.
  Also modernised the legacy `shinyUI()`/`shinyServer()` wrappers to the
  current `ui <- ...` / `server <- function(input, output) {...}` style.

# condformat 0.10.1

## Fixes

* Fix `theme_grob()` documentation
* Updated URLs
* Register internal S3 methods
* Minor documentation formatting

# condformat 0.10.0

## New features

* `condformat2grob()` function takes an optional `draw=TRUE` parameter which
  controls whether the grob is immediately drawn during the function call.
  `draw=FALSE` allows easier use in composite figures with
  `gridExtra::grid.arrange()` or `ggpubr::ggarrange()` (@interzoneboy, #31)

## Other

* Replace `tidyselect::vars_select()` (questioning) with `tidyselect::eval_select()`
* Avoid drawing plots on tests, use vdiffr instead.

# condformat 0.9.0

## Breaking changes

* The lazyeval API deprecated on condformat 0.6 has been dropped, only the tidy evaluation
  API remains. This fixes dplyr 1.0.0 test failures.

## New features

* Excel output supports rule_text_bold and rule_text_color

## Deprecation

* space is ignored and deprecated in rule_fill_gradient and rule_fill_gradient2,
  following recent changes in the scales package. Please remove it if you are
  using it, as it will be removed in a future version

## Other

* Replace xlsx with openxlsx. This avoids pulling RJava as a suggested dependency
* Warn if unsupported rule is used with Excel output
* Fix warning due to rlang update
* Change examples and tests so they don't open browser windows

# condformat 0.8.0

## New features:

* `rule_fill_bar` (HTML, for now)
* `rule_text_bold` (HTML, PDF)
* `rule_text_color` (HTML, PDF)
* `theme_htmlWidget` (HTML) Customize the number of entries and the widget size.
* `theme_kable` (PDF) pass options to kable
* `theme_caption` (HTML, PDF) Set a caption.
* `condformat2grob`: Engine to render tables as graphics
* Shiny: `renderCondformat` works with "async" promises (if htmlwidgets 1.3 is available)

## Deprecations:

* Deprecate: `theme_htmlTable` now only accepts arguments to be passed to
  `htmlTable::htmlTable`. Before, it could also be used to accept 
  `number_of_entries`, `width`, `height`... to customize the widget appearance.
  For that use case, please use `theme_htmlWidget` instead.
* Deprecate: `condformat2widget` will not accept arguments to customize the
  appearance of the widget. Use `theme_htmlWidget` instead.

## Other

* Replace dependency: Use grDevices instead of gplots
* Depend on knitr 1.18: slidy and other rmarkdown formats are properly detected (#13)
* Drop rJava from suggests (xlsx fix released)
* Bump rlang dependency (parse_quosure -> parse_expr) (#18, Thanks to @lionel-
  for the advice and code review!)
* Internal: More modular code. CSS tags to LaTeX conversion is based on
  S3 methods so new rules don't need to touch the code for LaTeX export.

# condformat 0.7.0

* Add a new condformat API, that is based on tidy evaluation principles and
  leverages on rlang and tidyselect. The old API is still fully functional but
  deprecated.
* Add a `rule_css` to let the user specify arbitrary CSS fields. For this rule
  there is no support for PDF or Excel output.
* Improve `condformat2excel` to export to a specific Excel sheet (#11)
* Fix `condformat2latex` and add regression test (#9)
* Fix `rescale` and `rescale_mid` calls (recent scales use a S3 generic, so we
  need to import it)

# condformat 0.6.0

* Fix tibble formatting issues, causing package malfunction.
* Fix: `theme_htmlTable()` now accepts the `css.cell` argument (thanks
  to 鱼飞灰 for reporting it)
* Require latest `htmlTable`. Improve/fix printing of condformat



# condformat 0.5.0

* Shiny support: Add `condformatOutput` and `renderCondformat`. Closes #3.
  - Use pagination in shiny if recent enough `htmlTable` package is installed.
* PDF support: Fix extra `NULL` when printing. Closes #4.
* Demote xlsx (and rJava) to Suggests.
* Fix standard evaluation functions, they now use formulas. See the examples (Reported in #5).
* Fix Excel export (fixed in `80440ecfd16fb74e3e0aa4c6ebc623ad2ae74b15`, reported in #7)

# condformat 0.4.0

* Excel export support with `condformat2excel`

# condformat 0.3.0

* PDF support: Export table to LaTeX using knitr.
* Fix examples for full compatibility with dplyr 0.5.0

# condformat 0.2.0

* First CRAN submission
* Add `theme_htmlTable` to pass custom formatting options to htmlTable.
* Improve knitr compatibility

# condformat 0.1.1

* Change to a ggplot-like syntax

