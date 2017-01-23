## Test environments
* local Ubuntu 16.04 install, R 3.3.2, with latest htmlTable version in CRAN
* local Ubuntu 16.04 install, R 3.3.2, development version of htmlTable package
* ubuntu 12.04.5 (on travis-ci), R 3.3.2
* win-builder (devel and release)

## R CMD check results
There is one NOTE:
Missing or unexported objects:
  ‘htmlTable::htmlTableWidget’ ‘htmlTable::htmlTableWidgetOutput’
  ‘htmlTable::renderHtmlTableWidget’

These objects are found in functions that are only called after checking that 
the htmlTable version is new enough (using "utils::packageVersion"). The symbols
exist in the development version of the htmlTable package that will be available
in the next CRAN release. To make sure the package is functional even in
previous htmlTable package versions a fallback is implemented and used if needed.


## Downstream dependencies
There are no known downstream dependencies.
