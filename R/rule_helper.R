add_rule_to_condformat <- function(x, rule) {
  condformatopts <- attr(x, "condformat")
  condformatopts[["rules"]] <- c(condformatopts[["rules"]], list(rule))
  attr(x, "condformat") <- condformatopts
  x
}
