##' Plot the locations of the IPHC stations for a given year, and whether or not
##'  a given species was caught in that year.
##'
##' If possible, also show whether the species was caught in the first 20 hooks
##' and only caught after the first 20 hooks. MAY ADD other options. Uses
##' `PBSmapping` style of plot, as used in the Redbanded and Yelloweye Rockfish
##' stock assessments.
##'
##' @param set_counts_of_sp input tibble, likely generated from
##'   `cache_pbs_data_iphc` (which seems to save a list with the first element
##'   the desired tibble), with column names `year`, `station`, `lat`, `lon`,
##'   `E_it`, `N_it`, `C_it`, `E_it20`, `N_it20`, `C_it20`, `usable`
##' @param sp species of interest
##' @param years years of interest ****currently just for one year
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
##' @return A map of the IPHC survey stations for that year and species, with a
##'   legend describing the points.
##' @export
##' @author Andrew Edwards
##' @examples
##' @donttest{
##' @ See vignette ******
##' plot_iphc_map(
##' @}
plot_iphc_map <- function(set_counts_of_sp,
                          sp,
                          years,
                          main_title = NULL,
                          mar_val = c(1.8,2,1.0,0.5),
                          mgp_val = c(1.6,0.5,0),
                          lat_cut_off = 50.6,
                          lon_cut_off_1 =-130,
                          lon_cut_off_2 = -128.25,
                          ...
                          ){
  par(mar = mar_val,
      mgp = mgp_val,
      ...)

  if(is.null(main_title))  main_title = paste0("All ", years, " stations")

  plot_BC(main = main_title)

  # TODO: generalise to work for 1997 and other years with only first 20 years
  # of data - so filter the data first then plot; need to use N_it20

  # Plot usable stations with catch rates = 0 for sp
  points(lat~lon,
         data = filter(set_counts_of_sp,
                       year == years,
                       usable=="Y",
                       N_it == 0),
         col = "red",
         cex = 1)

  # Plot usable stations with catch rates > 0
  points(lat~lon,
         data = filter(set_counts_of_sp,
                       year == years,
                       usable == "Y",
                       N_it > 0),
         col="red",
         pch=20,
         cex=1.2)

  # Plot unusable stations
  points(lat~lon,
         data = filter(set_counts_of_sp,
                       year == years,
                       usable == "N"),
         col="grey",
         pch=20,
         cex=1.2)

  # This is keeping in Series A or not - could just draw the horizontal line every time
#points(lat~lon,
#      data=filter(set_counts_of_sp,
#                    year == 1995,
#                    keep == 0),
  #      col="black", pch=4, cex=1.2)

  lines(c(lon1, lon2), rep(latCutOff, 2))

  legend("bottomleft", legend=c(paste("Did not catch", shortName),
                                paste("Did catch", shortName),
                                "Unusable station"),
         pch=c(1, 20, 20),
         pt.cex=c(1, 1.2, 1.2),
         col=c("red", "red", "grey"))
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
