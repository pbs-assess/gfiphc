##' Plotting functions for gfiphc



# ser_E_and_F <- calc_iphc_ser_E_and_F(sp_set_counts_with_area)
# plot.IPHC_ser_E_and_F(ser_E_and_F)  # this works
# plot(ser_E_and_F)  # this doesn't work, even though
# class(ser_E_and_F)
# [1] "IPHC_ser_E_and_F" "list"
##' Single plotting function for Series E, F or EF
##'
##' If you have EF then you have ser_E_and_F....
##' REDO: Plotting function for objects of class `IPHC_ser_E_and F` and also
##' `IPHC_ser_EF`, which are, respectively,
##'  - list containing tibbles for `ser_E` and `ser_F`
##'  - list containing
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
##' @
##' @}
plot.IPHC_ser_E_and_F <- function(ser_E_and_F,
                                  series_longest = NULL,
                                 plot_type = "E",    # change back to default as EF
                                 x_lim = NULL,
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
browser()
  if(is.null(y_lim)){
    y_max_series_longest <- ifelse(series_longest$test_EF$type == "F",
                                   series_longest$ser_longest$I_tBootHigh,
                                   series_longest$ser_longest$I_t20BootHigh)

    y_max <- max(c(ser_E_and_F$serE$I_t20BootHigh,
                   ser_E_and_F$serF$I_tBootHigh,
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

##' Wrapper to plot all four versions of Series E and F plots in one figure
##' ##'
##' @param ser_E_and_F
##' @param series_longest
##' @param ... further arguments to `plot.IPHC_ser_E_and_F()`
##' @return simple panel plot of four figures
##' @export
##' @author Andrew Edwards
##' @examples
##' @donttest{
##' @ # see vignette
##' @}
plot_IPHC_ser_four_panels <- function(ser_E_and_F,
                                      series_longest,
                                      ...){
  par(mfcol = c(2,2))

  plot.IPHC_ser_E_and_F(ser_E_and_F,
                        series_longest,
                        plot_type = "E",
                        ...)

  plot.IPHC_ser_E_and_F(ser_E_and_F,
                        series_longest,
                        plot_type = "F",
                        ...)

  plot.IPHC_ser_E_and_F(ser_E_and_F,
                        series_longest,
                        plot_type = "E_F_scaled",
                        ...)

  plot.IPHC_ser_E_and_F(ser_E_and_F,
                        series_longest,
                        plot_type = "EF",
                        ...)
}
