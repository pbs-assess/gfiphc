##' Plotting functions for gfiphc
##'
##' Currently just for Series EF
##'
##' @param ser_list
##' @param ...
plot.ser_long_EF <- function(ser_list,
                             ...){

# Think using one below instead

}



# ser_E_and_F <- calc_iphc_ser_E_and_F(sp_set_counts_with_area)
# plot.IPHC_ser_E_and_F(ser_E_and_F)  # this works
# plot(ser_E_and_F)  # this doesn't work, even though
# class(ser_E_and_F)
# [1] "IPHC_ser_E_and_F" "list"
#single plot function with arguments
#ser_E, ser_F, ser_EF
#
#call them all class IPHC_ser_E_F_EF
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
##'  * `EF` to plot Series EF [with E rescaled in red]
##' @param year_lim
##' @param y_lim
##' @param shift if plotting two then shift Series E to left and F to right by
##'   `shift` amount
##' @param tck_length small tickmark lengths
##' @param ser_E_col
##' @param ser_F_col
##' @param ...
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
                                 x_lim = c(1995, 2020),
                                 y_lim = NULL,
                                 shift = 0.15,
                                 tck_length = -0.02,
                                 ser_E_col = "blue",
                                 ser_F_col = "red",
                                 legend_text = NULL,
                                 x_lab = "Year",
                                 y_lab = "Catch rate index (numbers per effective skate)",
                                 ...){

  if(is.null(y_lim)){
  # TODO
  }

  if(!is.null(series_longest)){
    G_E <- series_longest$test_EF$G_E
    G_F <- series_longest$test_EF$G_F
  }

  if(plot_type == "E"){
    gplots::plotCI(ser_E_and_F$ser_E$year,
                   ser_E_and_F$ser_E$I_t20BootMean,
                   li = ser_E_and_F$ser_E$I_t20BootLow,
                   ui = ser_E_and_F$ser_E$I_t20BootHigh,
                   col = ser_E_col,
                   barcol = ser_E_col,
                   xlim = x_lim,
                   xlab = x_lab,
                   ylab = y_lab,
                   ...)

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
                   xlab = x_lab,
                   ylab = y_lab,
                   ...)

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
    gplots::plotCI(ser_E_and_F$ser_E$year -
                     (ser_E_and_F$ser_E$year %in% overlap_years) * shift,
                   ser_E_and_F$ser_E$I_t20BootMean / G_E,
                   li = ser_E_and_F$ser_E$I_t20BootLow / G_E,
                   ui = ser_E_and_F$ser_E$I_t20BootHigh / G_E,
                   col = ser_E_col,
                   barcol = ser_E_col,
                   xlim = x_lim,
                   xlab = x_lab,
                   ylab = "Relative catch rate index", # Relative since scaled
                   ...)

    gplots::plotCI(ser_E_and_F$ser_F$year +
                     (ser_E_and_F$ser_F$year %in% overlap_years) * shift,
                   ser_E_and_F$ser_F$I_tBootMean / G_F,
                   li = ser_E_and_F$ser_F$I_tBootLow / G_F,
                   ui = ser_E_and_F$ser_F$I_tBootHigh / G_F,
                   col = ser_F_col,
                   barcol = ser_F_col,
                   add = TRUE,
                   ...)
    legend("topright",
           legend = c("Series E scaled", "Series F scaled"),
           pch = c(1,1),
           col = c(ser_E_col,
                   ser_F_col),
           bty = "n")
  }

    if(plot_type == "EF"){

      if(is.null(series_longest$test_EF$t_EF$p.value)){
        stop("Plot needs tweaking if not Series EF due to species counts -- adapt based on calc_iphc_ser_EF() and maybe commit 26028b9.")
      }
      if(series_longest$test_EF$t_EF$p.value < 0.05){
        stop("Plot needs tweaking if not Series EF due to p-value -- adapt based on calc_iphc_ser_EF() and maybe commit 26028b9.")
      }

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
                     xlab = x_lab,
                     ylab = y_lab,
                     ...)

      legend("topright",
             legend = c("Original Series E", "Rescaled Series F"),
             pch = c(1,1),
             col = c(ser_E_col,
                     ser_F_col),
             bty = "n")
  }

}



##TODO:   axis(1, at=allYears, labels=FALSE, tck=tckL)   # not automated
