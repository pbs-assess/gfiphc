##' Plot the locations of the IPHC stations for a given year, with various
##options.
##'
##' Options include whether or not a given species was caught in that year, or
##'  indicating showing stations within a given area, or not referencing a
##'  particular species (just showing stations), or showing non-standard stations.
##'
##' If possible, also show whether the species was caught in the first 20 hooks
##' and only caught after the first 20 hooks. Uses `PBSmapping` style of plot,
##' as used in the Redbanded and Yelloweye Rockfish stock assessments.
##'
##' @param set_counts_of_sp input tibble of the species of interest, likely generated from
##'   `cache_pbs_data_iphc` (which seems to save a list with the first element
##'   the desired tibble), with column names `year`, `station`, `lat`, `lon`,
##'   `E_it`, `N_it`, `C_it`, `E_it20`, `N_it20`, `C_it20`, `usable`, and
##'   `in_area` if `indicate_in_area` is TRUE
##' @param sp_short_name short name species of interest to have in legend,
##'   if NULL then plots all stations labelled as
##'   usable and unusable, but with no catch rates. Still needs
##'   `set_counts_of_sp` input tibble (can be any species, as they all have all
##'   stations documented).
##' @param years year of interest. See vignettes for movie code.
##' @param main_title title for map, if NULL then is All `years` stations
##' @param mar_val mar values to reduce whitespace around maps
##' @param mgp_val mgp values to reduce whitespace around maps
##' @param lat_cut_off latitudinal cut off near top of Vancouver Island, below
##'   which stations are excluded when constructing Series A and B; this is just
##'   below all stations from 1995-1998, so that Series A and B include all
##'   stations for 1995-1998. If NULL then nothing plotted
##' @param lon_cut_off_1 left-hand end of line (longitude) to show cut off
##' @param lon_cut_off_2 right-hand end of line (longitude) to show cut off
##' @param pch_pos_count pch for positive counts (or sets inside area if
##'   `indicate_in_area` is TRUE)
##' @param pch_zero_count pch for zero counts (or sets outside area if
##'   `indicate_in_area` is FALSE)
##' @param pch_non_standard pch for showing non-standard stations for that year
##' @param cex_val cex size of plotted circles (or whatever is chosen using
##'   above `pch` options
##' @param add_to_existing if TRUE then add to an existing plot, if FALSE then
##'   create new map using `plot_BC()`
##' @param indicate_in_area indicate whether or not station is within a
##'   specficied area, as indicated by TRUE/FALSE in `set_counts_of_sp$in_area`
##' @param indicate_standard indicate whether or not station is part of the
##'   standard stations or an expansion (in 2018, 2020, and later), as
##'   indicated by Y/N in `set_counts_of_sp$standard`
##' @param ... extra arguments to `par()`
##' @return A map of the IPHC survey stations for that year and species, with a
##'   legend describing the points.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' # See vignette `data_for_one_species`
##' sp <- "yelloweye rockfish"
##' sp_set_counts <- readRDS(sp_hyphenate(sp))    # Need .rds
##'   saved already
##' plot_iphc_map(sp_set_counts$set_counts, sp_short_name = "Yelloweye", years =
##'   2003)  # one year
##' # many years (matches maps in Yamanka et al. (2018) Yelloweye assessment,
##' #  except stations 14A, 19IC, and 3D that are now marked as unusable):
##' for(i in unique(sp_set_counts$set_counts$year)){
##'   plot_iphc_map(sp_set_counts$set_counts,
##'                 sp_short_name = "Yelloweye",
##'                 years = i)
##'   invisible(readline(prompt="Press [enter] to continue"))
##' }
##'
##' # Hooks with no bait, showing non-standard as crosses :
##' plot_iphc_map(hooks_with_bait$set_counts,
##'               sp = "Hooks with bait",
##'               years = 2018,
##'               indicate_standard = TRUE)
##'
##' # Just the stations, with no species information:
##' plot_iphc_map(sp_set_counts$set_counts,
##'               sp_short_name = NULL,
##'               years = 2008)
##' }
##'
plot_iphc_map <- function(set_counts_of_sp,
                          sp_short_name = NULL,
                          years,
                          main_title = NULL,
                          mar_val = c(1.8,2,1.0,0.5),
                          mgp_val = c(1.6,0.5,0),
                          lat_cut_off = 50.6,
                          lon_cut_off_1 =-130,
                          lon_cut_off_2 = -128.25,
                          pch_pos_count = 19,
                          pch_zero_count = 1,
                          pch_non_standard = 4,
                          cex_val = 1,
                          add_to_existing = FALSE,
                          indicate_in_area = FALSE,
                          indicate_standard = TRUE,
                          ...
                          ){
  par(mar = mar_val,
      mgp = mgp_val,
      ...)

  if(is.null(main_title))  main_title = paste0("All ", years, " stations")

  set_counts_of_sp_one_year <- filter(set_counts_of_sp,
                                      year == years)

  if(!add_to_existing){
    plot_BC(main = main_title)
  }

  if(!is.null(sp_short_name)){
    # Species-specific
    add_stations(set_counts_of_sp_one_year,
                 species = TRUE,
                 pch_zero_count = pch_zero_count,
                 pch_pos_count = pch_pos_count,
                 cex_val = cex_val)
    legend_text <- c(paste("Did not catch", sp_short_name),
                     paste("Did catch", sp_short_name),
                     "Unusable station")
    legend_pch <- c(pch_zero_count,
                    pch_pos_count,
                    pch_pos_count)
    legend_cex <- rep(cex_val, 3)
    legend_col <- c("red",
                    "red",
                    "grey")
  } else {
    # Just show stations for whole coast
    if(!indicate_in_area){

      points(lat~lon,
             data = filter(set_counts_of_sp_one_year,
                           year == years,
                           usable == "Y"),
             col = "blue",
             pch = pch_pos_count,
             cex = cex_val)

      legend_text <- c("Usable station",
                      "Unusable station")
      legend_pch <- rep(pch_pos_count, 2)
      legend_cex <- rep(cex_val, 2)
      legend_col <- c("blue",
                      "grey")

    } else {
      # Indicate whether stations are in or outside of a region (region to be
      #  plotted outside of this function, see vignette)

      points(lat~lon,
             data = filter(set_counts_of_sp_one_year,
                           year == years,
                           usable == "Y",
                           in_area == TRUE),
             col = "blue",
             pch = pch_pos_count,
             cex = cex_val)

      points(lat~lon,
             data = filter(set_counts_of_sp_one_year,
                           year == years,
                           usable == "Y",
                           in_area == FALSE),
             col = "blue",
             pch = pch_zero_count,
             cex = cex_val)

      legend_text <- c("In area of interest",
                       "Outside area",
                       "Unusable station")
      legend_pch <- c(pch_pos_count,
                      pch_zero_count,
                      pch_pos_count)
      legend_cex <- rep(cex_val, 3)
      legend_col = c("blue",
                     "blue",
                     "grey")
    }
  }


  # Plot unusable stations (which have NA's for catch rates and E_it, E_it20)
  points(lat~lon,
         data = filter(set_counts_of_sp_one_year,
                       year == years,
                       usable == "N"),
         col = "grey",
         pch = pch_pos_count,
         cex = cex_val)

  # Add crosses for those not considered 'standard' stations in 2018, 2020 and later,
  #  originally to help figure out the differing definitions of standard in those two years.
  if(indicate_standard){
    points(lat~lon,
           data = filter(set_counts_of_sp_one_year,
                         year == years,
                         standard == "N"),
           pch = pch_non_standard,
           cex = cex_val*1.5)

    legend_text <- c(legend_text, "Non-standard station")
    legend_pch <- c(legend_pch, pch_non_standard)
    legend_cex <- c(legend_cex, cex_val*1.5)
    legend_col <- c(legend_col, "black")
  }

  legend("bottomleft",
         legend = legend_text,
         pch = legend_pch,
         pt.cex = legend_cex,
         col = legend_col)

  # This is keeping in Series A or not - could just draw the horizontal line every time
  #points(lat~lon,
  #      data=filter(set_counts_of_sp,
  #                    year == 1995,
  #                    keep == 0),
  #      col="black", pch=4, cex=1.2)

  lines(c(lon_cut_off_1, lon_cut_off_2),
        rep(lat_cut_off, 2))

}



##' Map for BC coast, adapted from an example in Rowan Haigh's TESA course
##'
##' @param xlim range of x (longitude)
##' @param ylim range of y (latitude)
##' @param zlev depth contours to show
##' @param showBathymetry if TRUE then show bathymetry contours
##' @param ... other arguments to `PBSmapping::plotMap()` or `PBSmapping::addLines()`
##' @return Map of BC land and sea
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' #
##' }
plot_BC <- function(xlim = c(-134,-124),
                   ylim = c(48,54.6),
                   zlev = seq(200,1200,200),
                   showBathymetry = FALSE,
                   ...)
{
  utils::data(nepacLL,
              nepacLLhigh,
              bcBathymetry,
              package = "PBSmapping")
  coast <- if (diff(xlim)<5) nepacLLhigh else nepacLL
  clin <- grDevices::contourLines(bcBathymetry,
                                  levels=zlev)
  poly <- PBSmapping::convCP(clin)
  isob <- PBSmapping::clipLines(poly$PolySet,
                                xlim=xlim,
                                ylim=ylim)
  pdat <- poly$PolyData
  attr(isob,"projection") <- "LL"
  clrFN <- grDevices::colorRampPalette(c("cyan4","blue","navy"))
  clrs <- clrFN(length(zlev))
  PBSmapping::plotMap(coast,
                      xlim = xlim,
                      ylim = ylim,
                      col = "lemonchiffon",
                      border = "grey50",
                      plt=NULL,
                      ...)
  if(showBathymetry){
    PBSmapping::addLines(isob,
                         col=clrs,
                         ...)
  }
  invisible(isob)
}

##' Add individual stations to map
##'
##' Add individual stations to map of BC, with options for all stations (no
##' species catch information), or positive and zero catches for a given
##' species, and more. Called from within `plot_iphc_map()`.
##'
##' @param set_counts_of_sp_one_year tibble for just one year, from
##'   `set_counts_of_sp`.
##' @param species TRUE to plot the stations and catch rates for the given
##'   species (already specified in the data), or FALSE to just low locations of
##'   stations and whether they are usable or not.
##' @param pch_zero_count pch for zero counts
##' @param pch_pos_count pch for positive counts
##' @param cex_val cex size of plotted circles (or whatever is chosen using
##'   above `pch` options
##' @return adds points to existing plot
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' #
##' }
add_stations <- function(set_counts_of_sp_one_year,
                         species = TRUE,
                         pch_zero_count,
                         pch_pos_count,
                         cex_val){
  if(all(!is.na(set_counts_of_sp_one_year$E_it))){
    # So E_it based on all hooks, so use N_it

    # Plot usable stations with catch rates = 0 for sp
    points(lat~lon,
           data = filter(set_counts_of_sp_one_year,
                         usable == "Y",
                         N_it == 0),
           col = "red",
           pch = pch_zero_count,
           cex = cex_val)

    # Plot usable stations with catch rates > 0
    points(lat~lon,
           data = filter(set_counts_of_sp_one_year,
                         usable == "Y",
                         N_it > 0),
           col="red",
           pch = pch_pos_count,
           cex = cex_val)} else {
    # So E_it is NA for all sets, meaning only first 20 hooks enumerated so use N_it20

    # Plot usable stations with catch rates = 0 for sp
    points(lat~lon,
           data = filter(set_counts_of_sp_one_year,
                         usable == "Y",
                         N_it20 == 0),
           pch = pch_zero_count,
           col = "red",
           cex = cex_val)

    # Plot usable stations with catch rates > 0
    points(lat~lon,
           data = filter(set_counts_of_sp_one_year,
                         usable == "Y",
                         N_it20 > 0),
           col = "red",
           pch = pch_pos_count,
           cex = cex_val)
    }
  invisible()
}

##' Plot a panel of maps
##'
##'
##' @param set_counts_of_sp see `plot_iphc_map()`
##' @param sp_short_name see `plot_iphc_map()`
##' @param years_to_show see vector of years to show one map for each year
##' @param par_mfrow two-value vector (rows by columns) for `par(mfrow = par_mfrow)`
##' @param ... further arguments to `plot_iphc_map()`
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'
##' }
plot_iphc_map_panel <- function(set_counts_of_sp,
                                sp_short_name,
                                years_to_show,
                                par_mfrow = c(2,2),
                                ...){

  par(mfrow = par_mfrow)
  for(years in years_to_show){
    plot_iphc_map(set_counts_of_sp,
                  sp_short_name,
                  years = years,
                  ...)
  }
}
