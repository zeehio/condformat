# Tests:
library(condformat)
library(dplyr)
context("rule_tests")

test_that("rule_column passes CSS attribute",{
  htmlcode <- print(condformat(iris %>% head) + rule_column(column="Species", css="background: red"))
  expect_match(htmlcode[1],"<td style='background: red; text-align: center;'>setosa</td>", fixed = TRUE)
})

condformat(iris %>% head,
           select=c("Sepal.Length", "Species"),
           col.names = c("Sepal Length", "Species")) +
  rule_column(column="Species", css="background: green")

condformat(iris %>% head,
           select=c("Sepal.Length", "Species"),
           col.names = c("Sepal Length", "Species")) +
  rule_column(column="Species", css="background: green") +
  rule_highlight(column="Sepal.Length", condition="Sepal.Length > 4.7",
                 css="background: orange")

condformat(iris %>% head,
           select=c("Sepal.Length", "Species"),
           col.names = c("Sepal Length", "Species")) +
  rule_fill_gradient(column="Sepal.Length")


condformat(iris %>% mutate(Sepal.Length=Sepal.Length - mean(Sepal.Length)),
           select=c("Sepal.Length", "Species"),
           col.names = c("Sepal Length", "Species")) +
  rule_fill_gradient2(column="Sepal.Length")
