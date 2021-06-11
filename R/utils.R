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
##' @param results If TRUE then append "results" to name also, for saving .RDS
##'   files in `data_for_all_species` vignette. E.g. return "yelloweye-rockfish-results.rds"
##' @return string of adapted species name
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' sp_hyphenate("yelloweye rockfish", underscore = TRUE)
##' }
sp_hyphenate <- function(sp,
                         hyphen_rds = TRUE,
                         underscore = FALSE,
                         area = FALSE,
                         results = FALSE){
  if(!hyphen_rds & !underscore) stop("Need one argument TRUE.")

  if(area){
    sp <- paste(sp, "area")
  }

  if(results){
    sp <- paste(sp, "results")
  }

  sp <- gsub("/",
             " ",
             sp)  # For rougheye/blackspotted rockfish complex

  if(underscore){
    return(paste0(gsub(" ",
                       "_",
                       sp)))
  } else {
    return(paste0(gsub(" ",
                       "-",
                       sp),
                  ".rds"))
  }
}

##' Capitalise first letters of words (from ?toupper)
##'
##' @param x words to be capitalised
simple_cap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}
