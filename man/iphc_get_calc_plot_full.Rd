% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc-iphc-ser.R
\name{iphc_get_calc_plot_full}
\alias{iphc_get_calc_plot_full}
\title{Get data, do calculations and plot Series A, B and longest series possible
(usually Series AB) for the IPHC survey, and maybe save results}
\usage{
iphc_get_calc_plot_full(
  sp,
  cached_data = TRUE,
  cached_results = FALSE,
  verbose = FALSE,
  print_sp_name = TRUE,
  path_data = ".",
  path_results = NULL
)
}
\arguments{
\item{sp}{Species names (as used in gfdata and gfplot). Or something like
"skates combined" -- see vignette.}

\item{cached_data}{if TRUE then use cached data (path_data/sp-name.rds)}

\item{cached_results}{if TRUE then use cached results
(path_results/sp-name-results.rds), else do calculations here and save results}

\item{verbose}{if TRUE then print out some of the data (useful in vignette loops)}

\item{print_sp_name}{if TRUE then print out species name (useful in vignette
loops)}

\item{path_data}{path to save or load the cached data}

\item{path_results}{path to save or load the cached data}
}
\value{
Saves results in \code{path_results/species-name-results.RDS}. If \code{path_results}
is NULL then do not save. For the given species, return list containing

sp_set_counts: list with one element (for consistency), a tibble
\code{set_counts} (the .RDS file saved when doing \code{cache_pbs_data_iphc(sp)}).

ser_ABCD_full: list of output from \code{calc_iphc_full_res()} from doing all
calculations.
}
\description{
Get data, do calculations and plot longest series for the IPHC survey for
a given species. For species that have several years of zero catches,
Series B may be better to use (but shorter) than Series AB because all
hooks were counted. For example Walleye Pollock in 2016 -- see example.
}
\details{
Will take a while since queries GFbio (and need to be on DFO
network). Basically a wrapper for the calculations in the vignette
\code{data_for_one_species}. Based on \code{iphc_get_calc_plot_area()}. See
\code{gfsynopsis::iphc_get_calc_plot()} for function for gfsynopsis
reports. Calling this one \code{iphc_get_calc_plot_full()} to distinguish it.
}
\examples{
\dontrun{
iphc_get_calc_plot_full("redbanded rockfish", cached_data = FALSE) # only at PBS
# Example where longest series may not be the most useful since only looking
  at first 20 hooks gives all zeros in 2016, but looking at all hooks gives
  some non-zero sets. In practice, catches may be too sparse to be useful anyway.
x <- iphc_get_calc_plot_full("walleye pollock", cached = FALSE) # only PBS
filter(x$series_ABCD_full$ser_longest, year == 2016)
# A tibble: 1 x 8
  year  Sets num_pos20 I_t20SampleMean I_t20BootMean I_t20BootLow I_t20BootHigh
 <dbl> <int>     <int>           <dbl>         <dbl>        <dbl>         <dbl>
  2016   132         0               0             0            0             0
... with 1 more variable: I_t20BootCV <dbl>
filter(x$series_ABCD_full$ser_all$ser_B, year == 2016)
# A tibble: 1 x 8
   year  Sets num_pos I_tSampleMean I_tBootMean I_tBootLow I_tBootHigh I_tBootCV
  <dbl> <int>   <int>         <dbl>       <dbl>      <dbl>       <dbl>     <dbl>
1  2016   132       2       0.00252     0.00249          0     0.00629     0.719
}
}
\author{
Andrew Edwards
}
