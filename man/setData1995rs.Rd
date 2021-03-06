% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{setData1995rs}
\alias{setData1995rs}
\title{Station data for 1995 IPHC survey for RS stations}
\format{
A tibble:
\describe{
\item{lat}{latitude of station}
\item{lon}{longitude of station}
\item{station}{station name}
\item{effSkate}{effective skate number of the set, as calculated by IPHC}
\item{usable}{whether or not the set is usable, as determined by IPHC plus
four extra that had no hooks enumerated except for Pacific
Halibut, and so are not usable by us}
}
}
\source{
Originally from file 1995IPHCSetlineData.xlsx that Lynne Yamanaka
received from Aaron Ranta at the IPHC.
}
\usage{
setData1995rs
}
\description{
A dataset containing details of the RS (Random Sample?) stations for the 1995
IPHC survey; one set was deployed at each station. We are not using these data
but including here for completeness.
}
\keyword{datasets}
