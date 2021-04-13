##' Plot the locations of the IPHC stations for a given year, and whether or not
##'  a given species was caught in that year.
##'
##' If possible, also show whether the species was caught in the first 20 hooks
##' and only caught after the first 20 hooks. MAY ADD other options. Uses
##' `PBSmapping` style of plot, as used in the Redbanded and Yelloweye Rockfish
##' stock assessments.
##'
##' @param set_counts_of_sp input tibble of the species of interest, likely generated from
##'   `cache_pbs_data_iphc` (which seems to save a list with the first element
##'   the desired tibble), with column names `year`, `station`, `lat`, `lon`,
##'   `E_it`, `N_it`, `C_it`, `E_it20`, `N_it20`, `C_it20`, `usable`
##' @param sp_short_name short name species of interest to have in legend,
##'   if NULL then plots all stations labelled as
##'   usable and unusable, but with no catch rates. Still needs
##'   `set_counts_of_sp` input tibble (can be any species, as they all have all
##'   stations documented).
##' @param years years of interest ****currently just for one year, multiple will
##'   get tricky
##' @param main_title title for map, if NULL then is "All <year> stations"
##' @param mar_val mar values to reduce whitespace around maps
##' @param mgp_val mgp values to reduce whitespace around maps
##' @param lat_cut_off latitudinal cut off near top of Vancouver Island, below
##'   which stations are excluded when constructing Series A and B; this is just
##'   below all stations from 1995-1998, so that Series A and B include all
##'   stations for 1995-1998
##' @param lon_cut_off_1 left-hand end of line (longitude) to show cut off
##' @param lon_cut_off_2 right-hand end of line (longitude) to show cut off
##'
##' @param pch_pos_count
##' @param pch_zero_count
##' @param cex_val
##' @param ...
##' @return A map of the IPHC survey stations for that year and species, with a
##'   legend describing the points.
##' @export
##' @author Andrew Edwards
##' @examples
##' @donttest{
##' @ See vignette ******
##' sp <- "yelloweye rockfish"
##' sp_set_counts <- readRDS(paste0(gsub(" ", "-", sp), ".rds"))    # Need .rds
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
##' # Just the stations with no species information:
##' plot_iphc_map(sp_set_counts$set_counts, sp = NULL, years = 1995)
##' }
##' @}
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
                          cex_val = 1,
                          ...
                          ){
  par(mar = mar_val,
      mgp = mgp_val,
      ...)

  if(is.null(main_title))  main_title = paste0("All ", years, " stations")

  set_counts_of_sp_one_year <- filter(set_counts_of_sp,
                                      year == years)

  plot_BC(main = main_title)

  if(!is.null(sp)){
    # Species-specific
    add_stations(set_counts_of_sp_one_year,
                 species = TRUE,
                 pch_zero_count = pch_zero_count,
                 pch_pos_count = pch_pos_count,
                 cex_val = cex_val)
    legend("bottomleft", legend=c(paste("Did not catch", sp_short_name),
                                  paste("Did catch", sp_short_name),
                                  "Unusable station"),
           pch = c(pch_zero_count, pch_pos_count, pch_pos_count),
           pt.cex = rep(cex_val, 3),
           col=c("red", "red", "grey"))
  } else {

    # Just show stations
    points(lat~lon,
           data = filter(set_counts_of_sp_one_year,
                         year == years,
                         usable == "Y"),
           col = "blue",
           pch = pch_pos_count,
           cex = cex_val)

    legend("bottomleft", legend=c(paste("Usable station"),
                                  "Unusable station"),
           pch = rep(pch_pos_count, 2),
           pt.cex = rep(cex_val, 2),
           col=c("blue", "grey"))
    }

    # Plot unusable stations (which have NA's for catch rates and E_it, E_it20)
    points(lat~lon,
           data = filter(set_counts_of_sp_one_year,
                         year == years,
                         usable == "N"),
           col = "grey",
           pch = pch_pos_count,
           cex = cex_val)

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
##' @param ...
##' @return Map of BC land and sea
##' @export
##' @author Andrew Edwards
##' @examples
##' @donttest{
##' @
##' @}
plot_BC <- function(xlim = c(-134,-124),
                   ylim=c(48,54.6),
                   zlev=seq(200,1200,200),
                   ...)
{
  data(nepacLL,
       nepacLLhigh,
       bcBathymetry)
  coast <- if (diff(xlim)<5) nepacLLhigh else nepacLL
  clin <- contourLines(bcBathymetry,
                       levels=zlev)
  poly <- convCP(clin)
  isob <- clipLines(poly$PolySet,
                    xlim=xlim,
                    ylim=ylim)
  pdat <- poly$PolyData
  attr(isob,"projection") <- "LL"
  clrFN <- colorRampPalette(c("cyan4","blue","navy"))
  clrs <- clrFN(length(zlev))
  plotMap(coast,
          xlim = xlim,
          ylim = ylim,
          col = "lemonchiffon",
          border = "grey50",
          plt=NULL,
          ...)
  #if(showBathymetry){
  #  addLines(isob,
  #           col=clrs,
  #           ...)
  #}
  invisible(isob)
}

##' Add individual stations to map
##'
##' Add individual stations to map of BC, with options for all stations (no
##' species catch information), or positive and zero catches for a given
##' species. Called from within `plot_iphc_map()`.
##'
##' @param set_counts_of_sp_one_year tibble for just one year, from
##'   `set_counts_of_sp`.
##' @param #usable
##' @param cex_val
##' @param usable TRUE if plotting the usable stations, FALSE for unusable.
##' @param pos_catch_rates TRUE if plotting the positive (>0) catch rates for
##'   the given species, FALSE if plotting the 0 catch rates (showing as open
##'   circles).
##' @param species TRUE to plot the stations and catch rates for the given
##'   species (already specified in the data), or FALSE to just low locations of
##'   stations and whether they are usable or not.
##' @return adds points to existing plot
##' @export
##' @author Andrew Edwards
##' @examples
##' @donttest{
##' @
##' @}
add_stations <- function(set_counts_of_sp_one_year,
#                         usable = "Y",
#                         pos_catch_rates = TRUE,
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
