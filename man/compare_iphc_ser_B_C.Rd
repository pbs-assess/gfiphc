% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc-iphc-ser.R
\name{compare_iphc_ser_B_C}
\alias{compare_iphc_ser_B_C}
\title{Compare Series B and C to see if A can be considered a relative index for the whole coast}
\usage{
compare_iphc_ser_B_C(series_all)
}
\arguments{
\item{series_all}{List of tibbles, one for each of Series A, B, C and D,
resulting from \code{\link[=calc_iphc_ser_all]{calc_iphc_ser_all()}}}
}
\value{
List of t_BC (results of the paired t-test), and geometric means G_B
and G_C.
}
\description{
Compare Series B and C to see if A can be considered a relative index for
the whole coast
}
