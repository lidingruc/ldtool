% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ldruc.R
\name{tabf}
\alias{tabf}
\title{make  count table using ctable function in summarytools library.}
\usage{
tabf(
  x,
  y,
  prop = st_options("ctable.prop"),
  useNA = "ifany",
  totals = st_options("ctable.totals"),
  style = st_options("style"),
  round.digits = st_options("ctable.round.digits"),
  justify = "right",
  plain.ascii = st_options("plain.ascii"),
  headings = st_options("headings"),
  display.labels = st_options("display.labels"),
  split.tables = Inf,
  dnn = c(substitute(x), substitute(y)),
  chisq = FALSE,
  OR = FALSE,
  RR = FALSE,
  weights = NA,
  rescale.weights = FALSE,
  ...
)
}
\description{
使用summarytools::ctable制作简单的频数表
}
