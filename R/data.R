#' Station data for 1995 IPHC survey
#'
#' A dataset containing details of the Setline Grid stations for the 1995 IPHC
#'  survey; one set was deployed at each station.
#'
#' @format A tibble:
#' \describe{
#'   \item{lat}{latitude of station}
#'   \item{lon}{longitude of station}
#'   \item{station}{station name}
#'   \item{effSkate}{effective skate number of the set, as calculated by IPHC}
#'   \item{usable}{whether or not the set is usable, as determined by IPHC plus
#'                 four extra that had no hooks enumerated except for Pacific
#'                 Halibut, and so are not usable by us}
#' }
#' @source Originally from file 1995IPHCSetlineData.xlsx that Lynne Yamanaka
#'         received from Aaron Ranta at the IPHC.
"setData1995"

#' Count data for 1995 IPHC survey
#'
#' A dataset containing counts of each species for each Setline Grid station for
#' the 1995 IPHC survey.
#'
#' @format A tibble:
#' \describe{
#'   \item{year}{1995 for this data set}
#'   \item{station}{station name, to link with setData1995}
#'   \item{specName}{species name for the counts for that set}
#'   \item{specCount}{count of that species for that set}
#' }
#' @source Originally from file 1995IPHCSetlineData.xlsx that Lynne Yamanaka
#'         received from Aaron Ranta at the IPHC.
"countData1995"

#' Station data for 1995 IPHC survey for RS stations
#'
#' A dataset containing details of the RS (Random Sample?) stations for the 1995
#' IPHC survey; one set was deployed at each station. We are not using these data
#' but including here for completeness.
#'
#' @format A tibble:
#' \describe{
#'   \item{lat}{latitude of station}
#'   \item{lon}{longitude of station}
#'   \item{station}{station name}
#'   \item{effSkate}{effective skate number of the set, as calculated by IPHC}
#'   \item{usable}{whether or not the set is usable, as determined by IPHC plus
#'                 four extra that had no hooks enumerated except for Pacific
#'                 Halibut, and so are not usable by us}
#' }
#' @source Originally from file 1995IPHCSetlineData.xlsx that Lynne Yamanaka
#'         received from Aaron Ranta at the IPHC.
"setData1995rs"

#' Count data for 1995 IPHC survey for RS stations
#'
#' A dataset containing counts of the RS (Random Sample?) stations for
#' the 1995 IPHC survey.
#'
#' @format A tibble:
#' \describe{
#'   \item{year}{1995 for this data set}
#'   \item{station}{station name, to link with setData1995}
#'   \item{specName}{species name for the counts for that set}
#'   \item{specCount}{count of that species for that set}
#' }
#' @source Originally from file 1995IPHCSetlineData.xlsx that Lynne Yamanaka
#'         received from Aaron Ranta at the IPHC.
"countData1995rs"


#' Count data for 2013 IPHC survey
#'
#' A dataset containing counts of each species for each
#' station for the 2013 IPHC survey.
#'
#' @format A tibble:
#' \describe{
#'
#'   \item{year}{2013 for this data set}
#'   \item{station}{station name}
#'   \item{spNameIPHC}{species name for the counts for that set}
#'   \item{specCount}{count of that species for that set}
#' }
#' @source Originally from file 2013.20-SampleInfo.csv from a spreadsheet
#'          from the IPHC.
#'          See private yeye15Reproduce repo by Andrew Edwards for data-raw.
"countData2013"

#' Station data for 2013 IPHC survey
#'
#' A dataset containing details of the stations for the 2013
#' IPHC survey; one set was deployed at each station.
#'
#' @format A tibble:
#' \describe{
#'   \item{year}{2013 for this data set}
#'   \item{station}{station name}
#'   \item{lat}{latitude of station}
#'   \item{lon}{longitude of station}
#'   \item{avgDepth}{average depth of set}
#'   \item{effSkateIPHC}{effective skate number of the set, as calculated by IPHC}
#'   \item{E_it20}{effective skate number of the set based on the first 20 hooks
#'     only, calculated as the (number of hooks observed)/
#'     (number of hooks retrieved) * effSkateIPHC}
#'   \item{usable}{whether or not the set is usable, as determined by IPHC}
#' }
#' @source Originally from file 2013.20-SetInfo.csv from a spreadsheet from the
#'   IPHC, with further calculations done in private yeye15Reproduce repo of
#'   Andrew Edwards (see data-raw there, could go into here if needed).
"setData2013"


#' Set-by-set level data (with species counts) from 1996 to 2002.
#'
#' A dataset containing details of the catches at each station from 1996 to 2002
#'  for the IPHC survey. Note that 1996 is based on all hooks being enumerated
#'  while 1997-2002 is for first 20 only. This is taken into account in
#'  [get_iphc_1996to2002()] which should be used to extract these data.
#'  The values of effSkateIPHC here have already been corrected to
#'  give an effective skate number based on the true number of hooks observed for
#'  1997-2002.
#' @format A tibble:
#' \describe{
#'   \item{year}{year}
#'   \item{station}{station name}
#'   \item{set}{set number within a trip (double check that)}
#'   \item{lat}{latitude of station}
#'   \item{lon}{longitude of station}
#'   \item{depthAvge}{average depth of set}
#'   \item{spCodeIPHC}{species code as used by the IPHC}
#'   \item{spNameIPHC}{species name as used by the IPHC}
#'   \item{E_it}{effective skate number of the set, as calculated by IPHC (based on
#'     observed hook numbers)}
#'   \item{catchCount}{count of that species for that set}
#'   \item{skates}{number of skates on that set}
#'   \item{hooksObserved}{number of hooks observed on that set}
#'   \item{usable}{whether or not the set is usable, as determined by IPHC}
#' }
#' @source Originally from the file 2B AllSpecies 96-02 roundlll.xls that came
#'   from the IPHC, which Rowan Haigh extracted into 'IPHC 2B Catch.rda', Andrew
#'   Edwards preprocessed in iphc9602.Snw for Yelloweye 2014 assessment, and then
#'   tidied further in private yeye15reproduce repository.
"data1996to2002"


#' Data indicating which stations are the 2018 (and maybe later) expansion stations
#'
#' A dataset containing details of the stations for the 2018
#' IPHC survey; one set was deployed at each usual station, but an expanded grid (more
#' stations) was used this year, and the expansion stations need to be kept
#' track of when comparing spatially-averaged catch rates to previous
#' years. 2019 onwards may also use the expanded grids.
#' Effective skate number, avgDepth and usability will come from the GFbio query (unlike, say,
#' setData2013, for which the set details could not go into GFbio).
#'
#' @format A tibble:
#' \describe{
#'   \item{station}{station name (character because early years used
#'     alphanumeric and need to be able to combine years later)}
#'   \item{lat}{latitude of station}
#'   \item{lon}{longitude of station}
#'   \item{standard}{whether or not (Y/N) the station is a standard one or an
#'   expansions station}
#'   \item{rca}{whether or not (Y/N) the station is within a Rockfish
#'   Conservation Area}
#'   \item{mpa}{whether or not (Y/N) the station is within a Marine Protected
#'   Area}
#'   \item{mpa_name}{name of the Marine Protected Area that the station is
#'   within, if applicable (else is character <NA>)}
#' }
#' @source Originally from file IPHC_Stations_All.csv from Dana Haggarty (later
#'   updated as IPHC_FISS_All_Stn_GFBio.csv to include the seven stations
#'   originally missing but that were in GFbio), with
#'   details and further tidying done here in `data-raw/IPHC-stations-expanded.R`.
"setDataExpansion"



#' Conversion from GFbio names to IPHC species names (for data not in GFbio)
#'
#' A data frame with the GFbio name and the corresponding name used by IPHC in
#'  the datasets that are at the set level and so not extracted from GFbio.
#' @format A csv file:
#' \describe{
#'   \item{species_common_nameyear}{GFbio species common name}
#'   \item{iphc_common_name}{IPHC data sets species common name}
#' }
#' @source Created manually by Andrew Edwards. Most are obvious. If get a new
#'   set-level dataset from IPHC (that doesn't get put into GFbio) then see
#'   [check_iphc_spp_name()].
#'   Currently this only has Type A species ****Andy to update with Type B.
#'   **And may also want "Hook with Bait" if different in GFbio******
#' @name iphc-spp-names.csv
NULL


#' Area considered for predators of Haida Gwaii Pacific Herring
#'
#' Primarily to determine which stations are within the relevant area to then
#' generate indices of abundance for groundfish predators of Haida Gwaii Pacific
#' Herring. See vignette.
#'
#' @format A `data.frame`, also of class `PolySet` (from PBSmapping) with
#'   columns, of which X and Y (longitude and latitude) are then used to
#'   determine which IPHC stations are inside the relevant area:
#' \describe{
#'   \item{PID}{primary identifier}
#'   \item{SID}{secondary identify}
#'   \item{POS}{not sure what this is}
#'   \item{X}{longitude of station}
#'   \item{Y}{latitude of station}
#' }
#' @source Jennifer Boldt sent Andrew Edwards the shape files for the area
#'   (corresponding to those used for Pacific Hake predators of Pacific
#'   Herring), which Rowan Haigh helped use with `PBSmapping` (primarily helping
#'   overdrawing of islands) There are still some minor issues and extra lines
#'   shown, possibly to do with islands on lakes on islands in the sea. Details
#'   in `data-raw/herring-predators-HG/herring-predators-HG-area.R`.
"HG_herring_pred_area"


#' Details of sets for data extracted from GFBio
#'
#' @format A `tibble` with one row for each set, with columns:
#'
#'
#'
#' also of class `PolySet` (from PBSmapping) with
#'   columns:
#' \describe{
#'   \item{year}{year}
#'   \item{tripID}{trip ID number from GFBio}
#'   \item{setID}{set ID number from GFBio}
#'   \item{station}{IPHC station number}
#'   \item{setInTrip}{set number during the trip}
#'   \item{{long}{longitude}
#'   \item{lat}{latitude}
#'   \item{obsHooksPerSet}{observed number of hooks for that set}
#'   \item{deplHooksPerSet}{deployed number of hooks for that set}
#'   \item{skatesCount}{number of skates on the set}
#'   \item{effSkateIPHC}{effective skate number from the IPHC}
#'   \item{iphcUsabilityCode}{usability code from the IPHC}
#'   \item{iphcUsabilityDesc}{usability description from the IPHC}
#'   \item{usable}{whether or not (Y/N) the set is usable, as determined by the
#'     IPHC}
#'   \item{standard}{whether or not (Y/N) the set is in the standard area or the
#'   expansion area (in 2018, and maybe later)}
#' }
#' @source Extracted from GFBio using `data-raw/sets-skates-hooks-yelloweye.R`.
"sets_other_years"

skates_other_years # Skate-level details from GFBio (such data are not available
                   # for years for which data are only available at the
                   # set-by-set level).

hooks_with_bait    # Counts of hooks returned with bait on them, for each set,
                   # for all years (note it is a list containing one tibble)

yelloweye_rockfish # Counts of Yelloweye Rockfish on each set for all years
                   #  (note it is a list containing one tibble)
