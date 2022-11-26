# condformat 0.9.0.9000

## New features

* `condformat2grob()` function takes an optional `draw=TRUE` parameter which
  controls whether the grob is immediately drawn during the function call.
  `draw=FALSE` allows easier use in composite figures with
  `gridExtra::grid.arrange()` or `ggpubr::ggarrange()` (@interzoneboy, #31)

## Other

* Replace tidyselect::vars_select (questioning) with tidyselect::eval_select()
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
* Shiny: renderCondformat works with async promises (if htmlwidgets 1.3 is available)

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
* Bugfix: `theme_htmlTable()` now accepts the `css.cell` argument (thanks
  to 鱼飞灰 for reporting it)
* Require latest `htmlTable`. Improve/fix printing of condformat



# condformat 0.5.0

* Shiny support: Add `condformatOutput` and `renderCondformat`. Closes #3.
  - Use pagination in shiny if recent enough `htmlTable` package is installed.
* PDF support: Fix extra `NULL` when printing. Closes #4.
* Demote xlsx (and rJava) to Suggests.
* Fix standard evaluation functions, they now use formulas. See the examples (Reported in #5).
* Fix Excel export (fixed in 80440ecfd16fb74e3e0aa4c6ebc623ad2ae74b15, reported in #7)

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

