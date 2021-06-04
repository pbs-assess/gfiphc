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
                               ser_A_col = "blue",
                               ser_B_col = "red",
                               ser_C_col = "orange",
                               ser_D_col = "darkgreen",
                               legend_text = NULL,
                               x_lab = "Year",
                               y_lab = "Catch rate index (numbers per effective skate)",
                               gap_ci = 0.5,
                               ...){
  # First just get working for A, B, A_B_scaled and AB
  # maybe do ylim calcs in the wrapper to do all four, since have many more
  # options than for E and F. TODO

  stopifnot(plot_type %in% c("A", "B", "C", "D", "A_B_scaled", "AB")) # not
                                        # implemented yet for others, but
                                        # somewhat general with X and Y used

  X <- substr(plot_type, 1, 1)

  ifelse(nchar(plot_type) == 2,
         Y <- substr(plot_type, 2, 2),
         Y <- FALSE)

  if(substr(plot_type, 4, 10) == "_scaled"){
    Y <- substr(plot_type, 3, 3)
  }

  x_ticks <- x_lim[1]:x_lim[2]

  # Can't use get() for elements of lists, so extract manually:
  ser_A <- series_ABCD_full$ser_all$ser_A
  ser_B <- series_ABCD_full$ser_all$ser_B
  ser_C <- series_ABCD_full$ser_all$ser_C
  ser_D <- series_ABCD_full$ser_all$ser_D

  G_A <- series_ABCD_full$test_AD$G_A
  G_B <- series_ABCD_full$test_BC$G_B
  G_C <- series_ABCD_full$test_BC$G_C
  G_D <- series_ABCD_full$test_AD$G_D

  assign("ser_X", get(paste0("ser_", X)))
  assign("ser_X_col", get(paste0("ser_", X, "_col")))
  # Then need later for Y and scaled

  # Plot simple unscaled of X
  if(plot_type %in% c("A", "D")){   # First 20 hooks only
    gplots::plotCI(ser_X$year,
                   ser_X$I_t20BootMean,
                   li = ser_X$I_t20BootLow,
                   ui = ser_X$I_t20BootHigh,
                   col = ser_X_col,
                   barcol = ser_X_col,
                   xlim = x_lim,
                   ylim = y_lim,
                   xlab = x_lab,
                   ylab = y_lab,
                   gap = gap_ci,
                   ...)
    axis(1, at = x_ticks, labels = FALSE, tck = tck_length)
    if(is.null(legend_text)) {
      legend_text = paste0("Series ", X)
    }

    legend("topright",
           legend = legend_text,
           bty = "n")
  }

  if(plot_type %in% c("B", "C")){   # All hooks
    gplots::plotCI(ser_X$year,
                   ser_X$I_tBootMean,
                   li = ser_X$I_tBootLow,
                   ui = ser_X$I_tBootHigh,
                   col = ser_X_col,
                   barcol = ser_X_col,
                   xlim = x_lim,
                   ylim = y_lim,
                   xlab = x_lab,
                   ylab = y_lab,
                   gap = gap_ci,
                   ...)
    axis(1, at = x_ticks, labels = FALSE, tck = tck_length)
    if(is.null(legend_text)) {
      legend_text = paste0("Series ", X)
    }

    legend("topright",
           legend = legend_text,
           bty = "n")
  }


  if(plot_type %in% c("A_B_scaled", "AB")){
    assign("ser_Y", get(paste0("ser_", Y)))
    assign("ser_Y_col", get(paste0("ser_", Y, "_col")))

    assign("G_X", get(paste0("G_", X)))
    assign("G_Y", get(paste0("G_", Y)))


    if(plot_type == "A_B_scaled"){

      overlap_years <- intersect(ser_X$year,
                                 ser_Y$year)  # overlapping years for
                                              # shifting horizontally
      if(length(overlap_years) == 0){
        G_X = 1
        G_Y = 1
      }   # just don't rescale, since can't.

      y_lim_X_Y_scaled <- c(0, max(c(ser_X$I_t20BootHigh / G_X,
                                     ser_Y$I_tBootHigh / G_Y)))

      gplots::plotCI(ser_X$year -
                     (ser_X$year %in% overlap_years) * shift,
                     ser_X$I_t20BootMean / G_X,
                     li = ser_X$I_t20BootLow / G_X,
                     ui = ser_X$I_t20BootHigh / G_X,
                     col = ser_X_col,
                     barcol = ser_X_col,
                     xlim = x_lim,
                     ylim = y_lim_X_Y_scaled,
                     xlab = x_lab,
                     ylab = "Relative catch rate index", # Relative since scaled
                     gap = gap_ci,
                     ...)

      gplots::plotCI(ser_Y$year +
                     (ser_Y$year %in% overlap_years) * shift,
                     ser_Y$I_tBootMean / G_Y,
                     li = ser_Y$I_tBootLow / G_Y,
                     ui = ser_Y$I_tBootHigh / G_Y,
                     col = ser_Y_col,
                     barcol = ser_Y_col,
                     add = TRUE,
                     gap = gap_ci,
                     ...)

      axis(1, at = x_ticks, labels = FALSE, tck = tck_length)
      legend("topright",
             legend = c(paste0("Series ", X, " scaled"),
                        paste0("Series ", Y, " scaled")),
             pch = c(1,1),
             col = c(ser_X_col,
                     ser_Y_col),
             bty = "n")
    }

    if(plot_type == "AB"){

      if(series_ABCD_full$type != "AB"){
        # plot nothing
      } else {
        years_only_Y <- setdiff(ser_Y$year,
                                ser_X$year) # years only in Y

        years_col <- rep(ser_X_col,
                         length(series_ABCD_full$ser_longest$year))

        years_col[which(years_only_Y %in% series_ABCD_full$ser_longest$year)] <-
          ser_Y_col

        gplots::plotCI(series_ABCD_full$ser_longest$year,
                       series_ABCD_full$ser_longest$I_t20BootMean,
                       li = series_ABCD_full$ser_longest$I_t20BootLow,
                       ui = series_ABCD_full$ser_longest$I_t20BootHigh,
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
               legend = c(paste0("Series ", X, Y),
                          "",
                          paste0("Original Series ", X),
                          paste0("Rescaled Series ", Y)),
               pch = c(NA, NA, 1,1),
               col = c(NA,
                       NA,
                       ser_X_col,
                       ser_Y_col),
               cex = c(1.2, 1, 1, 1),
               bty = "n")
      }
    }
  }
}



##' Wrapper to plot four versions of Series A-D plots in one figure,
##' with species name as main title.
##'
##' Defaults to Series A, B, A and B scaled, and AB. Haven't generalised
##'   `plot_IPHC_ser_ABCD()` to combinations with C and D yet (though does them
##'   individually).
##' @param series_ABCD_full list containing tibbles for `ser_longest` etc.,
##'   an output from `calc_iphc_full_res`
##' @param sp species common name
##' @param line_title for tweaking species title
##' @param y_lim y limits, if NULL (default) then automatically calculated
##' @param ... further arguments to `plot_IPHC_ser_ABCD()`
##' @return simple panel plot of four figures
##' @export
##' @author Andrew Edwards
##' @examples
##' @donttest{
##' series_ABCD_full <- calc_iphc_full_res(yelloweye_rockfish$set_counts)
##' plot_IPHC_ser_four_panels_ABCD(series_ABCD_full, "yelloweye rockfish")
##' # and see vignette
##' @}
plot_IPHC_ser_four_panels_ABCD <- function(series_ABCD_full,
                                           sp = NULL,
                                           line_title = -2,
                                           y_lim = NULL,
                                           ...){
  par(mfcol = c(2,2))

  #   Do axes here
  if(is.null(y_lim)){
    if(series_ABCD_full$type == "B"){
      y_max_series_longest <- series_ABCD_full$ser_longest$I_tBootHigh} else {
      y_max_series_longest <- series_ABCD_full$ser_longest$I_t20BootHigh} # may not cover
                                        # all types yet
    # Just base on A, B for now; may need adapting for C and D.
    y_max <- max(c(series_ABCD_full$ser_all$ser_A$I_t20BootHigh,
                   series_ABCD_full$ser_all$ser_B$I_tBootHigh,
                   y_max_series_longest))
    y_lim <- c(0, y_max)
  }

  plot_IPHC_ser_ABCD(series_ABCD_full,
                     plot_type = "A",
                     y_lim = y_lim,
                     ...)

  plot_IPHC_ser_ABCD(series_ABCD_full,
                     plot_type = "B",
                     y_lim = y_lim,
                     ...)

  plot_IPHC_ser_ABCD(series_ABCD_full,
                     plot_type = "A_B_scaled",
                     ...)

  plot_IPHC_ser_ABCD(series_ABCD_full,
                     plot_type = "AB",
                     y_lim = y_lim,
                     ...)

  title(simple_cap(sp),
        line = line_title,
        outer = TRUE)
}
