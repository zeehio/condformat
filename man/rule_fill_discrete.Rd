% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rule_fill_discrete.R
\name{rule_fill_discrete}
\alias{rule_fill_discrete}
\title{Fill column with discrete colors}
\usage{
rule_fill_discrete(
  x,
  columns,
  expression,
  colours = NA,
  na.value = "#FFFFFF",
  h = c(0, 360) + 15,
  c = 100,
  l = 65,
  h.start = 0,
  direction = 1,
  lockcells = FALSE
)
}
\arguments{
\item{x}{A condformat object, typically created with \code{\link[=condformat]{condformat()}}}

\item{columns}{A character vector with column names to be coloured. Optionally
\code{\link[tidyselect:language]{tidyselect::language()}} can be used.}

\item{expression}{an expression to be evaluated with the data.
It should evaluate to a logical or an integer vector,
that will be used to determine which cells are to be coloured.}

\item{colours}{a character vector with colours as values and the expression
possible results as names.}

\item{na.value}{a character string with the CSS color to be used in missing values}

\item{h}{range of hues to use, in [0, 360]}

\item{c}{chroma (intensity of colour), maximum value varies depending on
combination of hue and luminance.}

\item{l}{luminance (lightness), in [0, 100]}

\item{h.start}{hue to start at}

\item{direction}{direction to travel around the colour wheel,
1 = clockwise, -1 = counter-clockwise}

\item{lockcells}{logical value determining if no further rules should be applied to the affected cells.}
}
\value{
The condformat_tbl object, with the added formatting information
}
\description{
Fills a column or columns of a data frame using a discrete
colour palette, based on an expression.
}
\examples{
data(iris)
cf <- condformat(iris[c(1:5, 70:75, 120:125), ]) \%>\%
 rule_fill_discrete("Species", colours = c("setosa" = "red",
                                         "versicolor" = "blue",
                                         "virginica" = "green")) \%>\%
 rule_fill_discrete("Sepal.Length", expression = Sepal.Length > 4.6,
                    colours=c("TRUE"="red"))
\dontrun{
print(cf)
}

cf <- condformat(iris[c(1:5, 70:75, 120:125), ]) \%>\%
 rule_fill_discrete(c(starts_with("Sepal"), starts_with("Petal")),
                    expression = Sepal.Length > 4.6,
                    colours=c("TRUE"="red"))
\dontrun{
print(cf)
}
}
\seealso{
Other rule: 
\code{\link{rule_css}()},
\code{\link{rule_fill_bar}()},
\code{\link{rule_fill_gradient2}()},
\code{\link{rule_fill_gradient}()},
\code{\link{rule_text_bold}()},
\code{\link{rule_text_color}()}
}
\concept{rule}
