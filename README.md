condformat provides an HTML representation of a data frame in which
cells in columns are formatted according to several rules or criteria.

[![Build
Status](https://travis-ci.org/zeehio/condformat.svg?branch=master)](https://travis-ci.org/zeehio/condformat)
[![codecov.io](https://codecov.io/github/zeehio/condformat/coverage.svg?branch=master)](https://codecov.io/github/zeehio/condformat)

Browse source code
------------------

Checkout the code and browse it at
[<http://github.com/zeehio/condformat>](http://github.com/zeehio/condformat).

How to install condformat:
--------------------------

condformat is not yet on CRAN.

-   To install the latest development version:

        devtools::install_github("gforge/htmlTable", ref="develop")
        devtools::install_github("zeehio/condformat")

Example
-------

The example is properly formatted at
[<http://zeehio.github.io/condformat>](http://zeehio.github.io/condformat).

    data(iris)
    library(condformat)
    condformat(iris[c(1:5,70:75, 120:125),]) +
      rule_fill_discrete(Species) + 
      rule_fill_discrete(Sepal.Width, Sepal.Length,
                         expression = Sepal.Width > Sepal.Length - 2.25,
                         colours = c("TRUE" = "#7D00FF")) + 
      rule_fill_gradient2(Petal.Length)

<table class="gmisc_table" style="border-collapse: collapse;">
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
<td style="; background-color: #7D00FF; text-align: center;">
5.1
</td>
<td style="; background-color: #7D00FF; text-align: center;">
3.5
</td>
<td style="; background-color: #862A2A; text-align: center;">
1.4
</td>
<td style="; background-color:; text-align: center;">
0.2
</td>
<td style="; background-color: #F8766D; text-align: center;">
setosa
</td>
</tr>
<tr>
<td style="text-align: left;">
2
</td>
<td style="; background-color: #7D00FF; text-align: center;">
4.9
</td>
<td style="; background-color: #7D00FF; text-align: center;">
3.0
</td>
<td style="; background-color: #862A2A; text-align: center;">
1.4
</td>
<td style="; background-color:; text-align: center;">
0.2
</td>
<td style="; background-color: #F8766D; text-align: center;">
setosa
</td>
</tr>
<tr>
<td style="text-align: left;">
3
</td>
<td style="; background-color: #7D00FF; text-align: center;">
4.7
</td>
<td style="; background-color: #7D00FF; text-align: center;">
3.2
</td>
<td style="; background-color: #832424; text-align: center;">
1.3
</td>
<td style="; background-color:; text-align: center;">
0.2
</td>
<td style="; background-color: #F8766D; text-align: center;">
setosa
</td>
</tr>
<tr>
<td style="text-align: left;">
4
</td>
<td style="; background-color: #7D00FF; text-align: center;">
4.6
</td>
<td style="; background-color: #7D00FF; text-align: center;">
3.1
</td>
<td style="; background-color: #8A3030; text-align: center;">
1.5
</td>
<td style="; background-color:; text-align: center;">
0.2
</td>
<td style="; background-color: #F8766D; text-align: center;">
setosa
</td>
</tr>
<tr>
<td style="text-align: left;">
5
</td>
<td style="; background-color: #7D00FF; text-align: center;">
5.0
</td>
<td style="; background-color: #7D00FF; text-align: center;">
3.6
</td>
<td style="; background-color: #862A2A; text-align: center;">
1.4
</td>
<td style="; background-color:; text-align: center;">
0.2
</td>
<td style="; background-color: #F8766D; text-align: center;">
setosa
</td>
</tr>
<tr>
<td style="text-align: left;">
6
</td>
<td style="; background-color: blank; text-align: center;">
5.6
</td>
<td style="; background-color: blank; text-align: center;">
2.5
</td>
<td style="; background-color: #E1CBCB; text-align: center;">
3.9
</td>
<td style="; background-color:; text-align: center;">
1.1
</td>
<td style="; background-color: #00BA38; text-align: center;">
versicolor
</td>
</tr>
<tr>
<td style="text-align: left;">
7
</td>
<td style="; background-color: blank; text-align: center;">
5.9
</td>
<td style="; background-color: blank; text-align: center;">
3.2
</td>
<td style="; background-color: #F9F9FB; text-align: center;">
4.8
</td>
<td style="; background-color:; text-align: center;">
1.8
</td>
<td style="; background-color: #00BA38; text-align: center;">
versicolor
</td>
</tr>
<tr>
<td style="text-align: left;">
8
</td>
<td style="; background-color: blank; text-align: center;">
6.1
</td>
<td style="; background-color: blank; text-align: center;">
2.8
</td>
<td style="; background-color: #E5D1D1; text-align: center;">
4.0
</td>
<td style="; background-color:; text-align: center;">
1.3
</td>
<td style="; background-color: #00BA38; text-align: center;">
versicolor
</td>
</tr>
<tr>
<td style="text-align: left;">
9
</td>
<td style="; background-color: blank; text-align: center;">
6.3
</td>
<td style="; background-color: blank; text-align: center;">
2.5
</td>
<td style="; background-color: #F3F3F8; text-align: center;">
4.9
</td>
<td style="; background-color:; text-align: center;">
1.5
</td>
<td style="; background-color: #00BA38; text-align: center;">
versicolor
</td>
</tr>
<tr>
<td style="text-align: left;">
10
</td>
<td style="; background-color: blank; text-align: center;">
6.1
</td>
<td style="; background-color: blank; text-align: center;">
2.8
</td>
<td style="; background-color: #FFFFFF; text-align: center;">
4.7
</td>
<td style="; background-color:; text-align: center;">
1.2
</td>
<td style="; background-color: #00BA38; text-align: center;">
versicolor
</td>
</tr>
<tr>
<td style="text-align: left;">
11
</td>
<td style="; background-color: blank; text-align: center;">
6.4
</td>
<td style="; background-color: blank; text-align: center;">
2.9
</td>
<td style="; background-color: #F0E5E5; text-align: center;">
4.3
</td>
<td style="; background-color:; text-align: center;">
1.3
</td>
<td style="; background-color: #00BA38; text-align: center;">
versicolor
</td>
</tr>
<tr>
<td style="text-align: left;">
12
</td>
<td style="; background-color: blank; text-align: center;">
6.0
</td>
<td style="; background-color: blank; text-align: center;">
2.2
</td>
<td style="; background-color: #EDEDF5; text-align: center;">
5.0
</td>
<td style="; background-color:; text-align: center;">
1.5
</td>
<td style="; background-color: #619CFF; text-align: center;">
virginica
</td>
</tr>
<tr>
<td style="text-align: left;">
13
</td>
<td style="; background-color: blank; text-align: center;">
6.9
</td>
<td style="; background-color: blank; text-align: center;">
3.2
</td>
<td style="; background-color: #C5C5E0; text-align: center;">
5.7
</td>
<td style="; background-color:; text-align: center;">
2.3
</td>
<td style="; background-color: #619CFF; text-align: center;">
virginica
</td>
</tr>
<tr>
<td style="text-align: left;">
14
</td>
<td style="; background-color: blank; text-align: center;">
5.6
</td>
<td style="; background-color: blank; text-align: center;">
2.8
</td>
<td style="; background-color: #F3F3F8; text-align: center;">
4.9
</td>
<td style="; background-color:; text-align: center;">
2.0
</td>
<td style="; background-color: #619CFF; text-align: center;">
virginica
</td>
</tr>
<tr>
<td style="text-align: left;">
15
</td>
<td style="; background-color: blank; text-align: center;">
7.7
</td>
<td style="; background-color: blank; text-align: center;">
2.8
</td>
<td style="; background-color: #8B8BC2; text-align: center;">
6.7
</td>
<td style="; background-color:; text-align: center;">
2.0
</td>
<td style="; background-color: #619CFF; text-align: center;">
virginica
</td>
</tr>
<tr>
<td style="text-align: left;">
16
</td>
<td style="; background-color: blank; text-align: center;">
6.3
</td>
<td style="; background-color: blank; text-align: center;">
2.7
</td>
<td style="; background-color: #F3F3F8; text-align: center;">
4.9
</td>
<td style="; background-color:; text-align: center;">
1.8
</td>
<td style="; background-color: #619CFF; text-align: center;">
virginica
</td>
</tr>
<tr>
<td style="border-bottom: 2px solid grey; text-align: left;">
17
</td>
<td style="; background-color: blank; border-bottom: 2px solid grey; text-align: center;">
6.7
</td>
<td style="; background-color: blank; border-bottom: 2px solid grey; text-align: center;">
3.3
</td>
<td style="; background-color: #C5C5E0; border-bottom: 2px solid grey; text-align: center;">
5.7
</td>
<td style="; background-color:; border-bottom: 2px solid grey; text-align: center;">
2.1
</td>
<td style="; background-color: #619CFF; border-bottom: 2px solid grey; text-align: center;">
virginica
</td>
</tr>
</tbody>
</table>
