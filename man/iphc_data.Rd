% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iphc-data.R
\name{iphc_data}
\alias{iphc_data}
\alias{get_iphc_sets}
\alias{get_iphc_sets_info}
\alias{get_iphc_skates_info}
\alias{get_iphc_hooks}
\alias{cache_pbs_data_iphc}
\title{Get IPHC data}
\usage{
get_iphc_sets(species, usability = NULL)

get_iphc_sets_info()

get_iphc_skates_info()

get_iphc_hooks(species, usability = NULL)

cache_pbs_data_iphc(species, file_name = NULL, path = ".", compress = FALSE)
}
\arguments{
\item{species}{One or more species common names (e.g. \code{"pacific ocean perch"}) or one or more species codes (e.g. \code{396}). Species codes can be
specified as numeric vectors \verb{c(396, 442}) or characters \code{c("396", "442")}.
Numeric values shorter than 3 digits will be expanded to 3 digits and
converted to character objects (\code{1} turns into \code{"001"}). Species common
names and species codes should not be mixed. If any element is missing a
species code, then all elements will be assumed to be species common
names.}

\item{usability}{A vector of usability codes to include. Defaults to all.
IPHC codes may be different to other surveys.}

\item{file_name}{Optional filename(s) for the cached file. Defaults to the
same as the \code{species} argument.}

\item{path}{The folder where the cached data will be saved.}

\item{compress}{Compress the \code{.rds} file? Defaults to \code{FALSE} for faster
reading and writing at the expense of disk space.}
}
\value{
The \code{\link[=get_all_iphc_set_counts]{get_all_iphc_set_counts()}} function returns a data frame.
The \code{\link[=cache_pbs_data_iphc]{cache_pbs_data_iphc()}}
function writes an \code{.rds} file to \code{path} for each specified species. A data
object for a single species is a named list object with one element
containing the data frame from \code{\link[=get_all_iphc_set_counts]{get_all_iphc_set_counts()}}.
The element name of the list is \code{set_counts}.
}
\description{
Get IPHC data
}
\details{
\itemize{
\item \code{get_iphc_sets()} extracts IPHC survey data at the set level for given
species, from 2003 to present (excluding 2013 and any later years that are
not in database)
\item \code{get_iphc_sets_info()} extracts IPHC survey data regarding each set, with no
species information, to give one unique row (with lat, lon etc.) for each
set, from 2003 to present (excluding 2013 and others not in database)
\item \code{get_iphc_skates_info()} extracts IPHC survey data regarding each skate,
with no species information, to give one unique row (with lat, lon etc.)
for each set, from 2003 to present (excluding 2013 and otheres not in database);
needed for the hooks per skate
\item \code{get_iphc_hooks()} extracts IPHC survey data at the hook level for given
species, from 2003 to present (excluding 2013 and others not in database).
If species is 'hook with bait' then it returns the hooks that were returned
with bait.
\item \code{cache_pbs_data_iphc()} runs \code{get_all_iphc_set_counts()} for a given species
and caches extracted data to a given folder
}

This \code{\link[=cache_pbs_data_iphc]{cache_pbs_data_iphc()}} function caches the data for the given
species from \code{\link[=get_all_iphc_set_counts]{get_all_iphc_set_counts()}}
}
\examples{
\dontrun{
cache_pbs_data_iphc("redbanded rockfish")
cache_pbs_data_iphc(c("redbanded rockfish",
                      "pacific ocean perch"),
                    path = "two-rockfish")
}
}
