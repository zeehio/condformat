Introduction to condformat
================
Sergio Oller
2017-11-16

condformat renders a data frame in which cells in columns are formatted according to several rules or criteria.

[![Downloads](https://cranlogs.r-pkg.org/badges/condformat)](https://cran.r-project.org/package=condformat) [![Build Status](https://travis-ci.org/zeehio/condformat.svg?branch=master)](https://travis-ci.org/zeehio/condformat) [![codecov.io](https://codecov.io/github/zeehio/condformat/coverage.svg?branch=master)](https://codecov.io/github/zeehio/condformat)

Browse source code
------------------

Checkout the code and browse it at <http://github.com/zeehio/condformat>.

How to install condformat:
--------------------------

### Dependencies

If you want to use the PDF output you will need the `xcolor` LaTeX package. Either use the full texlive distribution, or install `latex-xcolor` on Debian and derivatives.

### Package installation

-   From CRAN:

        install.packages("condformat")

-   To install the latest development version:

        devtools::install_github("zeehio/condformat")

Example
-------

The example is properly formatted at <http://zeehio.github.io/condformat>.

``` r
data(iris)
library(condformat)
condformat(iris[c(1:5,70:75, 120:125),]) %>%
  rule_fill_discrete(Species) %>%
  rule_fill_discrete(c(Sepal.Width, Sepal.Length),
                     expression = Sepal.Width > Sepal.Length - 2.25,
                     colours = c("TRUE" = "#7D00FF")) %>%
  rule_fill_gradient2(Petal.Length) %>%
  rule_text_bold(c(Sepal.Length, Species), Species == "setosa") %>%
  rule_text_color(Sepal.Length,
                  expression = ifelse(Species == "setosa", "yellow", "")) %>%
  rule_fill_bar(Petal.Width, limits = c(0, NA))
```

<table class="gmisc_table" style="border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;">
<thead>
<tr>
<th style="border-bottom: 1px solid grey; border-top: 2px solid grey;">
</th>
<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
Sepal.Length
</th>
<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
Sepal.Width
</th>
<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
Petal.Length
</th>
<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
Petal.Width
</th>
<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
Species
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">
1
</td>
<td style="background-color: #7D00FF; font-weight: bold; color: yellow; text-align: center;">
5.1
</td>
<td style="background-color: #7D00FF; text-align: center;">
3.5
</td>
<td style="background-color: #872B29; text-align: center;">
1.4
</td>
<td style="background-color: white; background-size: 9% 100%; background-image: linear-gradient(to right, rgba(0, 100, 0, 1) 0%, rgba(255, 255, 255, 1) 100%); border: 1px solid black; background-repeat: no-repeat; text-align: center;">
0.2
</td>
<td style="background-color: #F8766D; font-weight: bold; text-align: center;">
setosa
</td>
</tr>
<tr>
<td style="text-align: left;">
2
</td>
<td style="background-color: #7D00FF; font-weight: bold; color: yellow; text-align: center;">
4.9
</td>
<td style="background-color: #7D00FF; text-align: center;">
3.0
</td>
<td style="background-color: #872B29; text-align: center;">
1.4
</td>
<td style="background-color: white; background-size: 9% 100%; background-image: linear-gradient(to right, rgba(0, 100, 0, 1) 0%, rgba(255, 255, 255, 1) 100%); border: 1px solid black; background-repeat: no-repeat; text-align: center;">
0.2
</td>
<td style="background-color: #F8766D; font-weight: bold; text-align: center;">
setosa
</td>
</tr>
<tr>
<td style="text-align: left;">
3
</td>
<td style="background-color: #7D00FF; font-weight: bold; color: yellow; text-align: center;">
4.7
</td>
<td style="background-color: #7D00FF; text-align: center;">
3.2
</td>
<td style="background-color: #832424; text-align: center;">
1.3
</td>
<td style="background-color: white; background-size: 9% 100%; background-image: linear-gradient(to right, rgba(0, 100, 0, 1) 0%, rgba(255, 255, 255, 1) 100%); border: 1px solid black; background-repeat: no-repeat; text-align: center;">
0.2
</td>
<td style="background-color: #F8766D; font-weight: bold; text-align: center;">
setosa
</td>
</tr>
<tr>
<td style="text-align: left;">
4
</td>
<td style="background-color: #7D00FF; font-weight: bold; color: yellow; text-align: center;">
4.6
</td>
<td style="background-color: #7D00FF; text-align: center;">
3.1
</td>
<td style="background-color: #8C322F; text-align: center;">
1.5
</td>
<td style="background-color: white; background-size: 9% 100%; background-image: linear-gradient(to right, rgba(0, 100, 0, 1) 0%, rgba(255, 255, 255, 1) 100%); border: 1px solid black; background-repeat: no-repeat; text-align: center;">
0.2
</td>
<td style="background-color: #F8766D; font-weight: bold; text-align: center;">
setosa
</td>
</tr>
<tr>
<td style="text-align: left;">
5
</td>
<td style="background-color: #7D00FF; font-weight: bold; color: yellow; text-align: center;">
5.0
</td>
<td style="background-color: #7D00FF; text-align: center;">
3.6
</td>
<td style="background-color: #872B29; text-align: center;">
1.4
</td>
<td style="background-color: white; background-size: 9% 100%; background-image: linear-gradient(to right, rgba(0, 100, 0, 1) 0%, rgba(255, 255, 255, 1) 100%); border: 1px solid black; background-repeat: no-repeat; text-align: center;">
0.2
</td>
<td style="background-color: #F8766D; font-weight: bold; text-align: center;">
setosa
</td>
</tr>
<tr>
<td style="text-align: left;">
6
</td>
<td style="background-color: #FFFFFF; font-weight: normal; text-align: center;">
5.6
</td>
<td style="background-color: #FFFFFF; text-align: center;">
2.5
</td>
<td style="background-color: #E7CAC6; text-align: center;">
3.9
</td>
<td style="background-color: white; background-size: 48% 100%; background-image: linear-gradient(to right, rgba(0, 100, 0, 1) 0%, rgba(255, 255, 255, 1) 100%); border: 1px solid black; background-repeat: no-repeat; text-align: center;">
1.1
</td>
<td style="background-color: #00BA38; font-weight: normal; text-align: center;">
versicolor
</td>
</tr>
<tr>
<td style="text-align: left;">
7
</td>
<td style="background-color: #FFFFFF; font-weight: normal; text-align: center;">
5.9
</td>
<td style="background-color: #FFFFFF; text-align: center;">
3.2
</td>
<td style="background-color: #F9F9FC; text-align: center;">
4.8
</td>
<td style="background-color: white; background-size: 78% 100%; background-image: linear-gradient(to right, rgba(0, 100, 0, 1) 0%, rgba(255, 255, 255, 1) 100%); border: 1px solid black; background-repeat: no-repeat; text-align: center;">
1.8
</td>
<td style="background-color: #00BA38; font-weight: normal; text-align: center;">
versicolor
</td>
</tr>
<tr>
<td style="text-align: left;">
8
</td>
<td style="background-color: #FFFFFF; font-weight: normal; text-align: center;">
6.1
</td>
<td style="background-color: #FFFFFF; text-align: center;">
2.8
</td>
<td style="background-color: #EAD0CD; text-align: center;">
4.0
</td>
<td style="background-color: white; background-size: 57% 100%; background-image: linear-gradient(to right, rgba(0, 100, 0, 1) 0%, rgba(255, 255, 255, 1) 100%); border: 1px solid black; background-repeat: no-repeat; text-align: center;">
1.3
</td>
<td style="background-color: #00BA38; font-weight: normal; text-align: center;">
versicolor
</td>
</tr>
<tr>
<td style="text-align: left;">
9
</td>
<td style="background-color: #FFFFFF; font-weight: normal; text-align: center;">
6.3
</td>
<td style="background-color: #FFFFFF; text-align: center;">
2.5
</td>
<td style="background-color: #F4F2F9; text-align: center;">
4.9
</td>
<td style="background-color: white; background-size: 65% 100%; background-image: linear-gradient(to right, rgba(0, 100, 0, 1) 0%, rgba(255, 255, 255, 1) 100%); border: 1px solid black; background-repeat: no-repeat; text-align: center;">
1.5
</td>
<td style="background-color: #00BA38; font-weight: normal; text-align: center;">
versicolor
</td>
</tr>
<tr>
<td style="text-align: left;">
10
</td>
<td style="background-color: #FFFFFF; font-weight: normal; text-align: center;">
6.1
</td>
<td style="background-color: #FFFFFF; text-align: center;">
2.8
</td>
<td style="background-color: #FFFFFF; text-align: center;">
4.7
</td>
<td style="background-color: white; background-size: 52% 100%; background-image: linear-gradient(to right, rgba(0, 100, 0, 1) 0%, rgba(255, 255, 255, 1) 100%); border: 1px solid black; background-repeat: no-repeat; text-align: center;">
1.2
</td>
<td style="background-color: #00BA38; font-weight: normal; text-align: center;">
versicolor
</td>
</tr>
<tr>
<td style="text-align: left;">
11
</td>
<td style="background-color: #FFFFFF; font-weight: normal; text-align: center;">
6.4
</td>
<td style="background-color: #FFFFFF; text-align: center;">
2.9
</td>
<td style="background-color: #F3E4E2; text-align: center;">
4.3
</td>
<td style="background-color: white; background-size: 57% 100%; background-image: linear-gradient(to right, rgba(0, 100, 0, 1) 0%, rgba(255, 255, 255, 1) 100%); border: 1px solid black; background-repeat: no-repeat; text-align: center;">
1.3
</td>
<td style="background-color: #00BA38; font-weight: normal; text-align: center;">
versicolor
</td>
</tr>
<tr>
<td style="text-align: left;">
12
</td>
<td style="background-color: #FFFFFF; font-weight: normal; text-align: center;">
6.0
</td>
<td style="background-color: #FFFFFF; text-align: center;">
2.2
</td>
<td style="background-color: #EEECF6; text-align: center;">
5.0
</td>
<td style="background-color: white; background-size: 65% 100%; background-image: linear-gradient(to right, rgba(0, 100, 0, 1) 0%, rgba(255, 255, 255, 1) 100%); border: 1px solid black; background-repeat: no-repeat; text-align: center;">
1.5
</td>
<td style="background-color: #619CFF; font-weight: normal; text-align: center;">
virginica
</td>
</tr>
<tr>
<td style="text-align: left;">
13
</td>
<td style="background-color: #FFFFFF; font-weight: normal; text-align: center;">
6.9
</td>
<td style="background-color: #FFFFFF; text-align: center;">
3.2
</td>
<td style="background-color: #C8C1E1; text-align: center;">
5.7
</td>
<td style="background-color: white; background-size: 100% 100%; background-image: linear-gradient(to right, rgba(0, 100, 0, 1) 0%, rgba(255, 255, 255, 1) 100%); border: 1px solid black; background-repeat: no-repeat; text-align: center;">
2.3
</td>
<td style="background-color: #619CFF; font-weight: normal; text-align: center;">
virginica
</td>
</tr>
<tr>
<td style="text-align: left;">
14
</td>
<td style="background-color: #FFFFFF; font-weight: normal; text-align: center;">
5.6
</td>
<td style="background-color: #FFFFFF; text-align: center;">
2.8
</td>
<td style="background-color: #F4F2F9; text-align: center;">
4.9
</td>
<td style="background-color: white; background-size: 87% 100%; background-image: linear-gradient(to right, rgba(0, 100, 0, 1) 0%, rgba(255, 255, 255, 1) 100%); border: 1px solid black; background-repeat: no-repeat; text-align: center;">
2.0
</td>
<td style="background-color: #619CFF; font-weight: normal; text-align: center;">
virginica
</td>
</tr>
<tr>
<td style="text-align: left;">
15
</td>
<td style="background-color: #FFFFFF; font-weight: normal; text-align: center;">
7.7
</td>
<td style="background-color: #FFFFFF; text-align: center;">
2.8
</td>
<td style="background-color: #9085C3; text-align: center;">
6.7
</td>
<td style="background-color: white; background-size: 87% 100%; background-image: linear-gradient(to right, rgba(0, 100, 0, 1) 0%, rgba(255, 255, 255, 1) 100%); border: 1px solid black; background-repeat: no-repeat; text-align: center;">
2.0
</td>
<td style="background-color: #619CFF; font-weight: normal; text-align: center;">
virginica
</td>
</tr>
<tr>
<td style="text-align: left;">
16
</td>
<td style="background-color: #FFFFFF; font-weight: normal; text-align: center;">
6.3
</td>
<td style="background-color: #FFFFFF; text-align: center;">
2.7
</td>
<td style="background-color: #F4F2F9; text-align: center;">
4.9
</td>
<td style="background-color: white; background-size: 78% 100%; background-image: linear-gradient(to right, rgba(0, 100, 0, 1) 0%, rgba(255, 255, 255, 1) 100%); border: 1px solid black; background-repeat: no-repeat; text-align: center;">
1.8
</td>
<td style="background-color: #619CFF; font-weight: normal; text-align: center;">
virginica
</td>
</tr>
<tr>
<td style="border-bottom: 2px solid grey; text-align: left;">
17
</td>
<td style="background-color: #FFFFFF; font-weight: normal; border-bottom: 2px solid grey; text-align: center;">
6.7
</td>
<td style="background-color: #FFFFFF; border-bottom: 2px solid grey; text-align: center;">
3.3
</td>
<td style="background-color: #C8C1E1; border-bottom: 2px solid grey; text-align: center;">
5.7
</td>
<td style="background-color: white; background-size: 91% 100%; background-image: linear-gradient(to right, rgba(0, 100, 0, 1) 0%, rgba(255, 255, 255, 1) 100%); border: 1px solid black; background-repeat: no-repeat; border-bottom: 2px solid grey; text-align: center;">
2.1
</td>
<td style="background-color: #619CFF; font-weight: normal; border-bottom: 2px solid grey; text-align: center;">
virginica
</td>
</tr>
</tbody>
</table>
Rules and engine support
------------------------

All rules should be supported in all engines.

| Rule                  | HTML | LaTeX |
|-----------------------|:----:|:-----:|
| `rule_fill_discrete`  |   X  |   X   |
| `rule_fill_gradient`  |   X  |   X   |
| `rule_fill_gradient2` |   X  |   X   |
| `rule_text_color`     |   X  |   X   |
| `rule_text_bold`      |   X  |   X   |
| `rule_css`            |   X  |       |
| `rule_fill_bar`       |   X  |       |
