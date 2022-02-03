attr_apply <- function(x, f, ...) {
  f(x) %>%
  structure_daytime(., x, attr(x, "rational"), ...)
}

hr_to_min <- function(hr) {
  {hr * 60} %>%
  {. + 1440} %>%
  {. %% 1440}
}
