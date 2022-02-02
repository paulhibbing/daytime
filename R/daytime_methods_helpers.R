attr_apply <- function(x, f) {
  f(x) %>%
  structure(
    .,
    rational = attr(x, "rational"),
    class = unique(c(
      "daytime",
      setdiff(class(.), "circular")
    ))
  )
}

hr_to_min <- function(hr) {
  {hr * 60} %>%
  {. + 1440} %>%
  {. %% 1440}
}
