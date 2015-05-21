# condformat

condformat provides an HTML representation of a data frame in which
cells in columns are formatted according to several rules or criteria

To install:

* the latest development version: 

    ```
    devtools::install_github("gforge/htmlTable", ref="develop")
    devtools::install_github("zeehio/condformat")
    ```

To use:

```
data(iris)
library(condformat)
condformat(iris) +
 rule_highlight(column="Species",
                condition="Sepal.Width > Sepal.Length - 2.25",
                css="background: #7D00FF") +
 rule_fill_gradient2(column="Sepal.Length")
```
