% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-iphc.R
\name{plot_IPHC_ser_E_and_F}
\alias{plot_IPHC_ser_E_and_F}
\title{Plotting functions for gfiphc. The E and F ones were actually done first
(had no time to do Series A-D ones for first gfsynopsis, which is why
Figures G.1 and G.2 were copied from an earlier assessment).
Could have made one global function for all options, but would have lots of
if statements, so just use the E_and_F one to create new one.
Though for A-D there are multiple comparisons we might wish to make, so
making that function a bit more general .
Single plotting function for Series E, F or EF}
\usage{
plot_IPHC_ser_E_and_F(
  ser_E_and_F,
  series_longest = NULL,
  plot_type = "E",
  x_lim = c(1995, 2022),
  y_lim = NULL,
  shift = 0.15,
  tck_length = -0.02,
  ser_E_col = "blue",
  ser_F_col = "red",
  legend_text = NULL,
  x_lab = "Year",
  y_lab = "Catch rate index (numbers per effective skate)",
  gap_ci = 0.5,
  ...
)
}
\arguments{
\item{ser_E_and_F}{list containing tibbles for \code{ser_E} and \code{ser_F} of class
\code{IPHC_ser_E_and_F}, an output from \code{calc_iphc_ser_E_and_F}}

\item{series_longest}{list of outputs from \code{calc_iphc_ser_EF(ser_E_and_F)},
including tibble \code{ser_longest} plus t-test results and geometric means of
each series.}

\item{plot_type}{one of
\itemize{
\item \code{E} to plot just Series E
\item \code{F} to plot just Series F
\item \code{E_F_scaled} to plot Series E and F each scaled by their geometric mean
of the overlapping years (no scaling if no overlapping years)
\item \code{EF} to plot Series EF (with E rescaled in red), empty plot if longest
series is just E or F
}}

\item{x_lim}{x range}

\item{y_lim}{y range}

\item{shift}{if plotting two then shift Series E to left and F to right by
\code{shift} amount}

\item{tck_length}{small tickmark lengths}

\item{ser_E_col}{colour for Series E}

\item{ser_F_col}{colour for Series F}

\item{legend_text}{text to override default if desired}

\item{x_lab}{x label to override default if desired}

\item{y_lab}{y label to override default if desired}

\item{gap_ci}{gap (units of 'height of letter O') between cirlce and
whiskers for confidence intervals}

\item{...}{further arguments to \code{gplots::plotCI}}
}
\value{
Single plot for Series E, F, or EF
}
\description{
Plotting function for objects of class \verb{IPHC_ser_E_and F} and also
\code{IPHC_ser_EF}, which are, respectively,
\itemize{
\item list containing tibbles for \code{ser_E} and \code{ser_F}
\item list containing tibble for \code{ser_longest} (and more)
}
}
\details{
Since an object with class \code{IPHC_ser_EF} can only be calculated from an
object of class \code{IPHC_ser_E_and_F}, to plot the former requires the latter
to be input also, and this function plots your choice of plot. Inputting
both means that the y-axes will be standardised across the types of plot
(where possible).
}
\examples{
\dontrun{
# See vignettes.

}
}
\author{
Andrew Edwards
}
