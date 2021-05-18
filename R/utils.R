##' Hyphenate or underscore species name.
##'
##'
##' @param sp Species common name (as used in gfdata and gfplot) as a string,
##'   e.g. "yelloweye rockfish".
##' @param hyphen_rds If TRUE then return "yelloweye-rockfish.rds" (for saving/loading
##'   .rds files of cached data).
##' @param underscore If TRUE then return "yelloweye_rockfish" (for storing R
##'   objects), and overrides `hyphen` being TRUE.
##' @param area If TRUE then return "yelloweye-rockfish-area" (or
##'   "yelloweye_rockfish_area") to use for area-restricted analyses.
##' @return string of adapted species name
##' @export
##' @author Andrew Edwards
##' @examples
##' @donttest{
##' sp_hyphenate("yelloweye rockfish", underscore = TRUE)
##' @}
sp_hyphenate <- function(sp,
                         hyphen_rds = TRUE,
                         underscore = FALSE,
                         area = FALSE){
  if(!hyphen_rds & !underscore) stop("Need one argument TRUE.")

  if(area){
    sp <- paste(sp, "area")
  }

  if(underscore){
    return(paste0(gsub(" ", "_", sp)))
  } else {
    return(paste0(gsub(" ", "-", sp), ".rds"))
  }
}
