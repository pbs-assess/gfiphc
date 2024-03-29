% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{skates_other_years}
\alias{skates_other_years}
\title{Details of skates for data extracted from GFBio}
\format{
A \code{tibble} with one row for each skate, with columns:
\describe{
\item{year}{year}
\item{tripID}{trip ID number from GFBio}
\item{set}{set number within the trip}
\item{setID}{set ID number from GFBio}
\item{skate}{skate number within the set}
\item{skateID}{skate ID number from GFBio}
\item{bait}{numeric code for the bait used on skate}
\item{deplHooksPerSkate}{deployed number of hooks for that skate}
\item{obsHooksPerSkate}{observed number of hooks for that skate}
\item{firstHook}{hook number of first hook on that skate}
\item{lastHook}{hook number of final hook on that skate}
\item{hook20}{hook number of 20th hook on that skate}
\item{chum....}{just chum-bait values for some columns}
\item{obsHooksPerSkate20}{observed hooks per skate for just the first 20 hooks}
}
}
\source{
Extracted from GFBio using \code{data-raw/sets-skates-hooks-yelloweye.R}.
}
\usage{
skates_other_years
}
\description{
Extracted data plus some calculations done in \code{get_iphc_skates_info()}, that
accounts for the fact that hook numbering starts at 1 for each set until
2006 (and so goes 1-800ish), then resets at 1 for each skate for each skate
for 2007 onwards (and so goes 1-100ish).
}
\keyword{datasets}
