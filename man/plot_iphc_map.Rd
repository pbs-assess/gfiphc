% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iphc-maps.R
\name{plot_iphc_map}
\alias{plot_iphc_map}
\title{Plot the locations of the IPHC stations for a given year, with various}
\usage{
plot_iphc_map(
  set_counts_of_sp,
  sp_short_name = NULL,
  years,
  main_title = NULL,
  mar_val = c(1.8, 2, 1, 0.5),
  mgp_val = c(1.6, 0.5, 0),
  lat_cut_off = 50.6,
  lon_cut_off_1 = -130,
  lon_cut_off_2 = -128.25,
  pch_pos_count = 19,
  pch_zero_count = 1,
  pch_non_standard = 4,
  cex_val = 1,
  add_to_existing = FALSE,
  indicate_in_area = FALSE,
  indicate_standard = TRUE,
  include_legend = TRUE,
  ...
)
}
\arguments{
\item{set_counts_of_sp}{input tibble of the species of interest, likely generated from
\code{cache_pbs_data_iphc} (which seems to save a list with the first element
the desired tibble), with column names \code{year}, \code{station}, \code{lat}, \code{lon},
\code{E_it}, \code{N_it}, \code{C_it}, \code{E_it20}, \code{N_it20}, \code{C_it20}, \code{usable}, and
\code{in_area} if \code{indicate_in_area} is TRUE}

\item{sp_short_name}{short name species of interest to have in legend,
if NULL then plots all stations labelled as
usable and unusable, but with no catch rates. Still needs
\code{set_counts_of_sp} input tibble (can be any species, as they all have all
stations documented).}

\item{years}{year of interest. See vignettes for movie code.}

\item{main_title}{title for map, if NULL then is All \code{years} stations}

\item{mar_val}{mar values to reduce whitespace around maps}

\item{mgp_val}{mgp values to reduce whitespace around maps}

\item{lat_cut_off}{latitudinal cut off near top of Vancouver Island, below
which stations are excluded when constructing Series A and B; this is just
below all stations from 1995-1998, so that Series A and B include all
stations for 1995-1998. If NULL then nothing plotted}

\item{lon_cut_off_1}{left-hand end of line (longitude) to show cut off}

\item{lon_cut_off_2}{right-hand end of line (longitude) to show cut off}

\item{pch_pos_count}{pch for positive counts (or sets inside area if
\code{indicate_in_area} is TRUE)}

\item{pch_zero_count}{pch for zero counts (or sets outside area if
\code{indicate_in_area} is FALSE)}

\item{pch_non_standard}{pch for showing non-standard stations for that year}

\item{cex_val}{cex size of plotted circles (or whatever is chosen using
above \code{pch} options}

\item{add_to_existing}{if TRUE then add to an existing plot, if FALSE then
create new map using \code{plot_BC()}}

\item{indicate_in_area}{indicate whether or not station is within a
specficied area, as indicated by TRUE/FALSE in \code{set_counts_of_sp$in_area}}

\item{indicate_standard}{indicate whether or not station is part of the
standard stations or an expansion (in 2018, 2020, and later), as
indicated by Y/N in \code{set_counts_of_sp$standard}}

\item{include_legend}{whether to include legend or not (TRUE/FALSE)}

\item{...}{extra arguments to \code{par()}}
}
\value{
A map of the IPHC survey stations for that year and species, with a
legend describing the points.
}
\description{
Options include whether or not a given species was caught in that year, or
indicating showing stations within a given area, or not referencing a
particular species (just showing stations), or showing non-standard stations.
}
\details{
If possible, also show whether the species was caught in the first 20 hooks
and only caught after the first 20 hooks. Uses \code{PBSmapping} style of plot,
as used in the Redbanded and Yelloweye Rockfish stock assessments.
}
\examples{
\dontrun{
# See vignette `data_for_one_species`
sp <- "yelloweye rockfish"
sp_set_counts <- readRDS(sp_hyphenate(sp))    # Need .rds
  saved already
plot_iphc_map(sp_set_counts$set_counts, sp_short_name = "Yelloweye", years =
  2003)  # one year
# many years (matches maps in Yamanka et al. (2018) Yelloweye assessment,
#  except stations 14A, 19IC, and 3D that are now marked as unusable):
for(i in unique(sp_set_counts$set_counts$year)){
  plot_iphc_map(sp_set_counts$set_counts,
                sp_short_name = "Yelloweye",
                years = i)
  invisible(readline(prompt="Press [enter] to continue"))
}

# Hooks with no bait, showing non-standard as crosses :
plot_iphc_map(hooks_with_bait$set_counts,
              sp = "Hooks with bait",
              years = 2018,
              indicate_standard = TRUE)

# Just the stations, with no species information:
plot_iphc_map(sp_set_counts$set_counts,
              sp_short_name = NULL,
              years = 2008)
}

}
\author{
Andrew Edwards
}
