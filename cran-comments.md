## Test environments
* local Ubuntu 16.04 install, R 3.3.2, with latest htmlTable version in CRAN
* local Ubuntu 16.04 install, R 3.3.2, development version of htmlTable package
* ubuntu 12.04.5 (on travis-ci), R 3.3.2
* win-builder (devel and release)

## R CMD check results
There are two NOTEs:

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Sergio Oller Moreno <sergioller@gmail.com>’

License components with restrictions and base license permitting such:
  BSD_3_clause + file LICENSE
File 'LICENSE':
  YEAR: 2015
  COPYRIGHT HOLDER: Sergio Oller Moreno
  ORGANIZATION: copyright holders

This NOTE is expected.

* checking dependencies in R code ... NOTE
Missing or unexported objects:
  ‘htmlTable::htmlTableWidget’ ‘htmlTable::htmlTableWidgetOutput’
  ‘htmlTable::renderHtmlTableWidget’

These missing objects are used in functions that are only called after checking that 
the htmlTable version is new enough (using "utils::packageVersion"). The symbols
currently only exist in the development version of the htmlTable package that
will be available in the next CRAN release. To make sure the package is functional even in
previous htmlTable package versions a fallback is implemented and used if needed.


## Downstream dependencies
There are no known downstream dependencies.
