% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{setData2013}
\alias{setData2013}
\title{Station data for 2013 IPHC survey}
\format{
A tibble:
\describe{
\item{year}{2013 for this data set}
\item{station}{station name}
\item{lat}{latitude of station}
\item{lon}{longitude of station}
\item{avgDepth}{average depth of set - NOTE: have not double checked if fathoms
(like later years) or metres}
\item{effSkateIPHC}{effective skate number of the set, as calculated by IPHC}
\item{E_it20}{effective skate number of the set based on the first 20 hooks
only, calculated as the (number of hooks observed)/
(number of hooks retrieved) * effSkateIPHC}
\item{usable}{whether or not the set is usable, as determined by IPHC}
}
}
\source{
Originally from file 2013.20-SetInfo.csv from a spreadsheet from the
IPHC, with further calculations done in private yeye15Reproduce repo of
Andrew Edwards (see data-raw there, could go into here if needed).
}
\usage{
setData2013
}
\description{
A dataset containing details of the stations for the 2013
IPHC survey; one set was deployed at each station.
}
\keyword{datasets}
