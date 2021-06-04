##' Plotting functions for gfiphc. The E and F ones were actually done first
##' (had no time to do Series A-D ones for first gfsynopsis, which is why
##' Figures G.1 and G.2 were copied from an earlier assessment).
##' Could have made one global function for all options, but would have lots of
##'  if statements, so just use the E_and_F one to create new one.
##'  Though for A-D there are multiple comparisons we might wish to make, so
##'  making that function a bit more general .

##' Single plotting function for Series E, F or EF
##'
##' Plotting function for objects of class `IPHC_ser_E_and F` and also
##' `IPHC_ser_EF`, which are, respectively,
##'  - list containing tibbles for `ser_E` and `ser_F`
##'  - list containing tibble for `ser_longest` (and more)
##'
##' Since an object with class `IPHC_ser_EF` can only be calculated from an
##' object of class `IPHC_ser_E_and_F`, to plot the former requires the latter
##' to be input also, and this function plots your choice of plot. Inputting
##' both means that the y-axes will be standardised across the types of plot
##' (where possible).
##'
##' @param ser_E_and_F list containing tibbles for `ser_E` and `ser_F` of class
##'   `IPHC_ser_E_and_F`, an output from `calc_iphc_ser_E_and_F`
##' @param series_longest list of outputs from `calc_iphc_ser_EF(ser_E_and_F)`,
##'   including tibble `ser_longest` plus t-test results and geometric means of
##'   each series.
##' @param plot_type one of
##'  * `E` to plot just Series E
##'  * `F` to plot just Series F
##'  * `E_F_scaled` to plot Series E and F each scaled by their geometric mean
##'   of the overlapping years (no scaling if no overlapping years)
##'  * `EF` to plot Series EF [with E rescaled in red], empty plot if longest
##'   series is just E or F
##' @param ser_E_col
##' @param ser_F_col
##' @param legend_text
##' @param x_lab
##' @param y_lab
##' @param gap_ci gap (units of 'height of letter O') between cirlce and
##'   whiskers for confidence intervals
##' @param ...
##' @param shift if plotting two then shift Series E to left and F to right by
##'   `shift` amount
##' @param tck_length small tickmark lengths
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' @donttest{
##' # See vignettes.
##' @}
plot_IPHC_ser_E_and_F <- function(ser_E_and_F,
                                  series_longest = NULL,
                                 plot_type = "E",    # change back to default as EF
                                 x_lim = c(1995, as.integer(format(Sys.Date(),
                                                                   "%Y")) - 1),
                                     # not automatic since not loading in full data
                                 y_lim = NULL,
                                 shift = 0.15,
                                 tck_length = -0.02,
                                 ser_E_col = "blue",
                                 ser_F_col = "red",
                                 legend_text = NULL,
                                 x_lab = "Year",
                                 y_lab = "Catch rate index (numbers per effective skate)",
                                 gap_ci = 0.5,
                                 ...){

  if(!is.null(series_longest)){
    G_E <- series_longest$test_EF$G_E
    G_F <- series_longest$test_EF$G_F
  }

  if(is.null(y_lim)){
    if(series_longest$test_EF$type == "F"){
      y_max_series_longest <- series_longest$ser_longest$I_tBootHigh} else {
      y_max_series_longest <- series_longest$ser_longest$I_t20BootHigh}

    y_max <- max(c(ser_E_and_F$ser_E$I_t20BootHigh,
                   ser_E_and_F$ser_F$I_tBootHigh,
                   y_max_series_longest))
    y_lim <- c(0, y_max)
  }

  if(is.null(x_lim)){
    x_lim <- range(c(ser_E_and_F$serE$year,
                     ser_E_and_F$serF$year,
                     series_longest$ser_longest$year))
  }

  x_ticks <- x_lim[1]:x_lim[2]

  if(plot_type == "E"){
    gplots::plotCI(ser_E_and_F$ser_E$year,
                   ser_E_and_F$ser_E$I_t20BootMean,
                   li = ser_E_and_F$ser_E$I_t20BootLow,
                   ui = ser_E_and_F$ser_E$I_t20BootHigh,
                   col = ser_E_col,
                   barcol = ser_E_col,
                   xlim = x_lim,
                   ylim = y_lim,
                   xlab = x_lab,
                   ylab = y_lab,
                   gap = gap_ci,
                   ...)
    axis(1, at = x_ticks, labels = FALSE, tck = tck_length)
    if(is.null(legend_text)) {
      legend_text = "Series E"
    }

    legend("topright",
           legend = legend_text,
           bty = "n")
  }

 if(plot_type == "F"){
    gplots::plotCI(ser_E_and_F$ser_F$year,
                   ser_E_and_F$ser_F$I_tBootMean,
                   li = ser_E_and_F$ser_F$I_tBootLow,
                   ui = ser_E_and_F$ser_F$I_tBootHigh,
                   col = ser_F_col,
                   barcol = ser_F_col,
                   xlim = x_lim,
                   ylim = y_lim,
                   xlab = x_lab,
                   ylab = y_lab,
                   gap = gap_ci,
                   ...)

    axis(1, at = x_ticks, labels = FALSE, tck = tck_length)
    if(is.null(legend_text)) {
      legend_text = "Series F"
    }

    legend("topright",
           legend = legend_text,
           bty = "n")
 }

  if(plot_type == "E_F_scaled"){

    overlap_years <- intersect(ser_E_and_F$ser_E$year,
                               ser_E_and_F$ser_F$year)  # overlapping years for
                                                        # shifting horizontally
    if(length(overlap_years) == 0){
      G_E = 1
      G_F = 1
    }   # just don't rescale, since can't.

    y_lim_E_F_scaled <- c(0, max(c(ser_E_and_F$ser_E$I_t20BootHigh / G_E,
                                   ser_E_and_F$ser_F$I_tBootHigh / G_F)))

    gplots::plotCI(ser_E_and_F$ser_E$year -
                     (ser_E_and_F$ser_E$year %in% overlap_years) * shift,
                   ser_E_and_F$ser_E$I_t20BootMean / G_E,
                   li = ser_E_and_F$ser_E$I_t20BootLow / G_E,
                   ui = ser_E_and_F$ser_E$I_t20BootHigh / G_E,
                   col = ser_E_col,
                   barcol = ser_E_col,
                   xlim = x_lim,
                   ylim = y_lim_E_F_scaled,  # include 0, and diff to y_lim
                   xlab = x_lab,
                   ylab = "Relative catch rate index", # Relative since scaled
                   gap = gap_ci,
                   ...)

    gplots::plotCI(ser_E_and_F$ser_F$year +
                     (ser_E_and_F$ser_F$year %in% overlap_years) * shift,
                   ser_E_and_F$ser_F$I_tBootMean / G_F,
                   li = ser_E_and_F$ser_F$I_tBootLow / G_F,
                   ui = ser_E_and_F$ser_F$I_tBootHigh / G_F,
                   col = ser_F_col,
                   barcol = ser_F_col,
                   add = TRUE,
                   gap = gap_ci,
                   ...)

    axis(1, at = x_ticks, labels = FALSE, tck = tck_length)
    legend("topright",
           legend = c("Series E scaled", "Series F scaled"),
           pch = c(1,1),
           col = c(ser_E_col,
                   ser_F_col),
           bty = "n")
  }

    if(plot_type == "EF"){

      if(series_longest$test_EF$type != "EF"){
        # is.null(series_longest$test_EF$t_EF$p.value) | series_longest$test_EF$t_EF$p.value < 0.05){
        # stop("Plot needs tweaking if not Series EF due to species counts -- adapt based on calc_iphc_ser_EF() and maybe commit 26028b9.")
        # stop("Plot needs tweaking if not Series EF due to p-value -- adapt
        # based on calc_iphc_ser_EF() and maybe commit 26028b9.")
        # plot nothing
      } else {
        years_only_F <- setdiff(ser_E_and_F$ser_F$year,
                                ser_E_and_F$ser_E$year) # years only in F


        years_col <- rep(ser_E_col,
                         length(series_longest$ser_longest$year))

        years_col[which(years_only_F %in% series_longest$ser_longest$year)] <- ser_F_col

        gplots::plotCI(series_longest$ser_longest$year,
                       series_longest$ser_longest$I_t20BootMean,
                       li = series_longest$ser_longest$I_t20BootLow,
                       ui = series_longest$ser_longest$I_t20BootHigh,
                       col = years_col,
                       barcol = years_col,
                       xlim = x_lim,
                       ylim = y_lim,
                       xlab = x_lab,
                       ylab = y_lab,
                       gap = gap_ci,
                       ...)

        axis(1, at = x_ticks, labels = FALSE, tck = tck_length)
        legend("topright",
               legend = c("Series EF",
                          "",
                          "Original Series E",
                          "Rescaled Series F"),
               pch = c(NA, NA, 1,1),
               col = c(NA,
                       NA,
                       ser_E_col,
                       ser_F_col),
               cex = c(1.2, 1, 1, 1),
               bty = "n")
      }
    }
}

##' Wrapper to plot all four versions of Series E and F plots in one figure,
##' with species name as main title
##'
##' @param ser_E_and_F
##' @param series_longest
##' @param sp species common name
##' @param line_title
##' @param ... further arguments to `plot_IPHC_ser_E_and_F()`
##' @return simple panel plot of four figures
##' @export
##' @author Andrew Edwards
##' @examples
##' @donttest{
##' @ # see vignette
##' @}
plot_IPHC_ser_four_panels <- function(ser_E_and_F,
                                      series_longest,
                                      sp = NULL,
                                      line_title = -2,
                                      ...){
  par(mfcol = c(2,2))

  plot_IPHC_ser_E_and_F(ser_E_and_F,
                        series_longest,
                        plot_type = "E",
                        ...)

  plot_IPHC_ser_E_and_F(ser_E_and_F,
                        series_longest,
                        plot_type = "F",
                        ...)

  plot_IPHC_ser_E_and_F(ser_E_and_F,
                        series_longest,
                        plot_type = "E_F_scaled",
                        ...)

  plot_IPHC_ser_E_and_F(ser_E_and_F,
                        series_longest,
                        plot_type = "EF",
                        ...)
  title(simple_cap(sp),
        line = line_title,
        outer = TRUE)
}


##' Single plotting function for Series A, B, C, D, or combination of two
##'
##' @param series_ABCD_full list containing tibbles for `ser_longest` etc.,
##'   an output from `calc_iphc_full_res`
##' @param plot_type one of
##'  * `X`, where `X` is `A`, `B`, `C`, or `D`, to plot just Series X
##'  * `X_Y_scaled` to plot Series X and Y each scaled by their geometric mean
##'   of the overlapping years (no scaling if no overlapping years), where X and
##'   Y are A, B, C, or D.
##'  * `XY` to plot Series XY [with X rescaled in red], empty plot if longest
##'   series is just X or Y - TODO - maybe. where X and
##'   Y are A, B, C, or D.
##' @param ser_X_col
##' @param ser_Y_col
##' @param legend_text
##' @param x_lab
##' @param y_lab
##' @param gap_ci gap (units of 'height of letter O') between cirlce and
##'   whiskers for confidence intervals
##' @param ...
##' @param shift if plotting two then shift Series X to left and Y to right by
##'   `shift` amount
##' @param tck_length small tickmark lengths
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' @donttest{
##' # See vignettes.
##' @}
plot_IPHC_ser_ABCD <- function(series_ABCD_full,
                               plot_type = "AB",
                               x_lim = c(1995, as.integer(format(Sys.Date(),
                                                                 "%Y")) - 1),
                                     # standard across species
                                 y_lim = NULL,
                                 shift = 0.15,
                                 tck_length = -0.02,
                                 ser_X_col = "blue",
                                 ser_Y_col = "red",
                                 legend_text = NULL,
                                 x_lab = "Year",
                                 y_lab = "Catch rate index (numbers per effective skate)",
                                 gap_ci = 0.5,
                                 ...){
HERE
  if(!is.null(series_longest)){
    G_E <- series_longest$test_EF$G_E
    G_F <- series_longest$test_EF$G_F
  }

  if(is.null(y_lim)){
    if(series_longest$test_EF$type == "F"){
      y_max_series_longest <- series_longest$ser_longest$I_tBootHigh} else {
      y_max_series_longest <- series_longest$ser_longest$I_t20BootHigh}

    y_max <- max(c(ser_E_and_F$ser_E$I_t20BootHigh,
                   ser_E_and_F$ser_F$I_tBootHigh,
                   y_max_series_longest))
    y_lim <- c(0, y_max)
  }

  if(is.null(x_lim)){
    x_lim <- range(c(ser_E_and_F$serE$year,
                     ser_E_and_F$serF$year,
                     series_longest$ser_longest$year))
  }

  x_ticks <- x_lim[1]:x_lim[2]

  if(plot_type == "E"){
    gplots::plotCI(ser_E_and_F$ser_E$year,
                   ser_E_and_F$ser_E$I_t20BootMean,
                   li = ser_E_and_F$ser_E$I_t20BootLow,
                   ui = ser_E_and_F$ser_E$I_t20BootHigh,
                   col = ser_E_col,
                   barcol = ser_E_col,
                   xlim = x_lim,
                   ylim = y_lim,
                   xlab = x_lab,
                   ylab = y_lab,
                   gap = gap_ci,
                   ...)
    axis(1, at = x_ticks, labels = FALSE, tck = tck_length)
    if(is.null(legend_text)) {
      legend_text = "Series E"
    }

    legend("topright",
           legend = legend_text,
           bty = "n")
  }

 if(plot_type == "F"){
    gplots::plotCI(ser_E_and_F$ser_F$year,
                   ser_E_and_F$ser_F$I_tBootMean,
                   li = ser_E_and_F$ser_F$I_tBootLow,
                   ui = ser_E_and_F$ser_F$I_tBootHigh,
                   col = ser_F_col,
                   barcol = ser_F_col,
                   xlim = x_lim,
                   ylim = y_lim,
                   xlab = x_lab,
                   ylab = y_lab,
                   gap = gap_ci,
                   ...)

    axis(1, at = x_ticks, labels = FALSE, tck = tck_length)
    if(is.null(legend_text)) {
      legend_text = "Series F"
    }

    legend("topright",
           legend = legend_text,
           bty = "n")
 }

  if(plot_type == "E_F_scaled"){

    overlap_years <- intersect(ser_E_and_F$ser_E$year,
                               ser_E_and_F$ser_F$year)  # overlapping years for
                                                        # shifting horizontally
    if(length(overlap_years) == 0){
      G_E = 1
      G_F = 1
    }   # just don't rescale, since can't.

    y_lim_E_F_scaled <- c(0, max(c(ser_E_and_F$ser_E$I_t20BootHigh / G_E,
                                   ser_E_and_F$ser_F$I_tBootHigh / G_F)))

    gplots::plotCI(ser_E_and_F$ser_E$year -
                     (ser_E_and_F$ser_E$year %in% overlap_years) * shift,
                   ser_E_and_F$ser_E$I_t20BootMean / G_E,
                   li = ser_E_and_F$ser_E$I_t20BootLow / G_E,
                   ui = ser_E_and_F$ser_E$I_t20BootHigh / G_E,
                   col = ser_E_col,
                   barcol = ser_E_col,
                   xlim = x_lim,
                   ylim = y_lim_E_F_scaled,  # include 0, and diff to y_lim
                   xlab = x_lab,
                   ylab = "Relative catch rate index", # Relative since scaled
                   gap = gap_ci,
                   ...)

    gplots::plotCI(ser_E_and_F$ser_F$year +
                     (ser_E_and_F$ser_F$year %in% overlap_years) * shift,
                   ser_E_and_F$ser_F$I_tBootMean / G_F,
                   li = ser_E_and_F$ser_F$I_tBootLow / G_F,
                   ui = ser_E_and_F$ser_F$I_tBootHigh / G_F,
                   col = ser_F_col,
                   barcol = ser_F_col,
                   add = TRUE,
                   gap = gap_ci,
                   ...)

    axis(1, at = x_ticks, labels = FALSE, tck = tck_length)
    legend("topright",
           legend = c("Series E scaled", "Series F scaled"),
           pch = c(1,1),
           col = c(ser_E_col,
                   ser_F_col),
           bty = "n")
  }

    if(plot_type == "EF"){

      if(series_longest$test_EF$type != "EF"){
        # is.null(series_longest$test_EF$t_EF$p.value) | series_longest$test_EF$t_EF$p.value < 0.05){
        # stop("Plot needs tweaking if not Series EF due to species counts -- adapt based on calc_iphc_ser_EF() and maybe commit 26028b9.")
        # stop("Plot needs tweaking if not Series EF due to p-value -- adapt
        # based on calc_iphc_ser_EF() and maybe commit 26028b9.")
        # plot nothing
      } else {
        years_only_F <- setdiff(ser_E_and_F$ser_F$year,
                                ser_E_and_F$ser_E$year) # years only in F


        years_col <- rep(ser_E_col,
                         length(series_longest$ser_longest$year))

        years_col[which(years_only_F %in% series_longest$ser_longest$year)] <- ser_F_col

        gplots::plotCI(series_longest$ser_longest$year,
                       series_longest$ser_longest$I_t20BootMean,
                       li = series_longest$ser_longest$I_t20BootLow,
                       ui = series_longest$ser_longest$I_t20BootHigh,
                       col = years_col,
                       barcol = years_col,
                       xlim = x_lim,
                       ylim = y_lim,
                       xlab = x_lab,
                       ylab = y_lab,
                       gap = gap_ci,
                       ...)

        axis(1, at = x_ticks, labels = FALSE, tck = tck_length)
        legend("topright",
               legend = c("Series EF",
                          "",
                          "Original Series E",
                          "Rescaled Series F"),
               pch = c(NA, NA, 1,1),
               col = c(NA,
                       NA,
                       ser_E_col,
                       ser_F_col),
               cex = c(1.2, 1, 1, 1),
               bty = "n")
      }
    }
}
