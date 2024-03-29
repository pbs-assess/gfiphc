% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{data1996to2002}
\alias{data1996to2002}
\title{Set-by-set level data (with species counts) from 1996 to 2002.}
\format{
A tibble:
\describe{
\item{year}{year}
\item{station}{station name}
\item{set}{set number within a trip (double check that)}
\item{lat}{latitude of station}
\item{lon}{longitude of station}
\item{depthAvge}{average depth of set - NOTE: have not double checked if fathoms
(like later years) or metres}
\item{spCodeIPHC}{species code as used by the IPHC}
\item{spNameIPHC}{species name as used by the IPHC}
\item{E_it}{effective skate number of the set, as calculated by IPHC (based on
observed hook numbers)}
\item{catchCount}{count of that species for that set}
\item{skates}{number of skates on that set}
\item{hooksObserved}{number of hooks observed on that set}
\item{usable}{whether or not the set is usable, as determined by IPHC}
}
}
\source{
Originally from the file 2B AllSpecies 96-02 roundlll.xls that came
from the IPHC, which Rowan Haigh extracted into 'IPHC 2B Catch.rda', Andrew
Edwards preprocessed in iphc9602.Snw for Yelloweye 2014 assessment, and then
tidied further in private yeye15reproduce repository.
}
\usage{
data1996to2002
}
\description{
A dataset containing details of the catches at each station from 1996 to 2002
for the IPHC survey. Note that 1996 is based on all hooks being enumerated
while 1997-2002 is for first 20 only. This is taken into account in
\code{\link[=get_iphc_1996to2002]{get_iphc_1996to2002()}} which should be used to extract these data.
The values of effSkateIPHC here have already been corrected to
give an effective skate number based on the true number of hooks observed for
1997-2002.
}
\keyword{datasets}
