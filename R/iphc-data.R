#' Get IPHC data
#'
#' @details
#' * `get_iphc_sets()` extracts IPHC survey data at the set level for given
#'    species, from 2003 to present (excluding 2013 and any later years that are
#'    not in database)
#' * `get_iphc_sets_info()` extracts IPHC survey data regarding each set, with no
#'    species information, to give one unique row (with lat, lon etc.) for each
#'    set, from 2003 to present (excluding 2013 and others not in database)
#' * `get_iphc_skates_info()` extracts IPHC survey data regarding each skate,
#'    with no species information, to give one unique row (with lat, lon etc.)
#'    for each set, from 2003 to present (excluding 2013 and otheres not in database);
#'    needed for the hooks per skate
#' * `get_iphc_hooks()` extracts IPHC survey data at the hook level for given
#'    species, from 2003 to present (excluding 2013 and others not in database).
#'    If species is 'hook with bait' then it returns the hooks that were returned
#'    with bait.
#' * `cache_pbs_data_iphc()` runs `get_all_iphc_set_counts()` for a given species
#'    and caches extracted data to a given folder
#' @name iphc_data
#' @importFrom gfdata run_sql inject_filter
#' @importFrom stats sd
#' @importFrom rlang .data
NULL

#' @rdname iphc_data
#' @param usability A vector of usability codes to include. Defaults to all.
#'   IPHC codes may be different to other surveys.
#' @param species One or more species common names (e.g. `"pacific ocean
#'   perch"`) or one or more species codes (e.g. `396`). Species codes can be
#'   specified as numeric vectors `c(396, 442`) or characters `c("396", "442")`.
#'   Numeric values shorter than 3 digits will be expanded to 3 digits and
#'   converted to character objects (`1` turns into `"001"`). Species common
#'   names and species codes should not be mixed. If any element is missing a
#'   species code, then all elements will be assumed to be species common
#'   names.
get_iphc_sets <- function(species, usability = NULL) {
  .q <- read_sql("get-iphc-set-level.sql")
  .q <- gfdata::inject_filter("AND C.SPECIES_CODE IN", species, sql_code = .q)
  .d <- gfdata::run_sql("GFBioSQL", .q)
  .d$species <- tolower(.d$species)
  as_tibble(.d)
}

##' @rdname iphc_data
##' @export
get_iphc_sets_info <- function() {
  .q <- read_sql("get-iphc-set-info.sql")
  .d <- gfdata::run_sql("GFBioSQL", .q)
  .d <- mutate(.d,
               usable = ifelse(iphcUsabilityCode %in% c(1, 52),
                               "Y",
                               "N")) %>%
    left_join(select(setDataExpansion,
                     station,
                     standard),
              by = "station")

  as_tibble(.d)
}

##' @rdname iphc_data
##' @export
get_iphc_skates_info <- function() {
  .q <- read_sql("get-iphc-skate-info.sql")
  .d <- gfdata::run_sql("GFBioSQL", .q)
  # Calculating here:
  # lastHook - hook number of final hook on that skate
  # firstHook - hook number of 1st hook on that skate
  # hook20 - hook number of 20th hook on that skate
  # lastHookTemp needed since need before firstHook, but want to re-order
  # chum... are just the chum-bait ones
  # Hook numbering starts at 1 for each set until 2006 (and so goes 1-800ish),
  #  then resets at 1 for each skate for each skate for 2007 onwards (and so
  #  goes 1-100ish). Hence the <2006.5 in lastHookTemp.
  .d <- mutate(group_by(.d, setID),
    lastHookTemp = cumsum(deplHooksPerSkate) * (year < 2006.5) +
      deplHooksPerSkate * (year > 2006.5),
    firstHook = lastHookTemp - deplHooksPerSkate + 1,
    lastHook = lastHookTemp,
    hook20 = firstHook +
      (deplHooksPerSkate < 19.5) * deplHooksPerSkate +
      (deplHooksPerSkate > 19.5) * 20 - 1,
    chumDeplHooksPerSkate = deplHooksPerSkate * (bait == 110),
    chumObsHooksPerSkate = obsHooksPerSkate * (bait == 110),
    obsHooksPerSkate20 = 20, # **Remove this line when Elise updates
    #  sql query, Issue #12 in gfiphc - needs finishing
    chumObsHooksPerSkate20 = obsHooksPerSkate20 * (bait == 110)
  ) %>%
    select(-lastHookTemp)
  as_tibble(.d)
}

#' @rdname iphc_data
#' @export
get_iphc_hooks <- function(species, usability = NULL) {
  if (species != "hook with bait") {
    .q <- read_sql("get-iphc-hook-level.sql")
    .q <- gfdata::inject_filter("AND C.SPECIES_CODE IN", species, sql_code = .q)
    .d <- gfdata::run_sql("GFBioSQL", .q)
    .d$species <- tolower(.d$species)
    if (dim(.d)[1] == 0) {
      # No data, give NA's, did have 2003 for year but now removing year = NA in get_all_iphc_set_counts()
      .d[1, ] <- c(NA, rep(NA, dim(.d)[2] - 1))
    }

    return(as_tibble(.d))
  } else {
    .q <- read_sql("get-iphc-hook-level-bait-on-hook.sql")
    .d <- gfdata::run_sql("GFBioSQL", .q)
    # .d$species <- tolower(.d$species)
    .d <- mutate(.d,
      speciesCode = NA,
      species = "hook with bait",
      numOnHook = 1,
      hookCondCode = NA
    ) %>% # that code unlikely to be useful
      select(
        year:hook,
        .data$speciesCode,
        .data$species,
        .data$numOnHook,
        .data$hookYieldCode,
        .data$hookCondCode
      ) # return same order as for species
    if (dim(.d)[1] == 0) {
      # No data, give NA's, did have 2003 for year but now removing year = NA in get_all_iphc_set_counts()
      .d[1, ] <- c(NA, rep(NA, dim(.d)[2] - 1))
    }
    as_tibble(.d)
  }
}

#' @param file_name Optional filename(s) for the cached file. Defaults to the
#'   same as the `species` argument.
#' @param path The folder where the cached data will be saved.
#' @param compress Compress the `.rds` file? Defaults to `FALSE` for faster
#'   reading and writing at the expense of disk space.
#'
#' @return The [get_all_iphc_set_counts()] function returns a data frame.
#'  The [cache_pbs_data_iphc()]
#' function writes an `.rds` file to `path` for each specified species. A data
#' object for a single species is a named list object with one element
#' containing the data frame from [get_all_iphc_set_counts()].
#' The element name of the list is `set_counts`.
#' @details
#' This [cache_pbs_data_iphc()] function caches the data for the given
#'   species from [get_all_iphc_set_counts()]
#' @rdname iphc_data
#' @export
#' @examples
#' \dontrun{
#' cache_pbs_data_iphc("redbanded rockfish")
#' cache_pbs_data_iphc(c("redbanded rockfish",
#'                       "pacific ocean perch"),
#'                     path = "two-rockfish")
#' }
cache_pbs_data_iphc <- function(species, file_name = NULL,
                                path = ".",
                                compress = FALSE) {
  dir.create(path, showWarnings = FALSE)
  for (sp_i in seq_along(species)) {
    this_sp <- species[[sp_i]]

    if (is.null(file_name)) {
      this_sp_clean <- gsub("/", "-", gsub(" ", "-", this_sp))
    } else {
      this_sp_clean <- gsub("/", "-", gsub(" ", "-", file_name[[sp_i]]))
    } # Not sure why Sean's works in cache_pbs_data without the { }. Also have
      #  sp_hyphenate() now that could maybe replace this. The "/" are
      #  thanks to blackspotted/rougheye

    message("Extracting IPHC data for ", this_sp)
    out <- list()
    out$set_counts <- get_all_iphc_set_counts(this_sp)
    saveRDS(out,
      file = paste0(file.path(path, this_sp_clean), ".rds"),
      compress = compress
    )
  }
  message("All IPHC data extracted and saved in the folder `", path, "`.")
}

#' Get early IPHC data from already-saved .rds files (can likely integrate into get-dat.R)
#'
#' Gets the early IPHC data from .rds files for the species in question.
#' Those data are not all in GFBio.
#'
#' The output datasets feed into IPHC-specific functions to create a time-series.
#' **.....other functions (`tidy_`,
#' `plot_`, or `fit_` functions) for data visualization, which can be used as
#' products themselves or can be fed into automated DFO Pacific groundfish data
#' synopsis report production.
#'
#' @details
#'
##' @param species Species common name (as used by gfdata). If NULL then
##'  get_iphc_spp_name returns the full data frame of names (iphc-spp-names-csv).
##' @name get_early_iphc
NULL

##' Get IPHC species common name given a single gfplot species common name
##'
##' Use the gfplot species common name to extract IPHC data.
##' @return IPHC species common name, or NA if species is not in IPHC data
##' (**need to add more years of species names if they start showing up), or
##' if species is NULL
##' then a data.frame of all species names (the file iphc-spp-names.csv).
##' @rdname get_early_iphc
##' @export
##' @examples
##' \dontrun{
##' get_iphc_spp_name("redbanded rockfish")
##' get_iphc_spp_name("made up name")
##' testthat::expect_error(get_iphc_spp_name(c("redbanded rockfish", "yellowmouth rockfish")))
##' }
get_iphc_spp_name <- function(species = NULL) {
  iphc_names <- read.csv(system.file("extdata/iphc-spp-names.csv",
                                     package = "gfiphc"),
                         comment.char = "#",
                         as.is = TRUE
                         )
  if (is.null(species)) {
    return(iphc_names)
  } else {
    if (length(species) > 1) {
      stop("get_iphc_spp_name() only works for one species")
    } else {
      name <- iphc_names[
        which(iphc_names$species_common_name == species),
        "iphc_common_name"
      ]
      if (length(name) == 0) {
        name <- NA
      } # species is not in .csv
      return(name)
    }
  }
}

##' Check whether the names in a new IPHC data set are already included
##'
##' For set-level data the counts are not in GFbio but in data/. When
##'  getting a new dataset for which we do not have hook level data
##'  then run this to check that all IPHC names have a GFbio name.
##' @param countData A new IPHC dataset to check all species names against, if
##'  NULL then returns all IPHC names in the saved set-level data that are not
##'  in our list of Type A or Type B species given in gfsynopsis.
##' @param ignore_obvious Whether or not to ignore some obvious non-groundfish
##'  species (listed in function) when countData is NULL.
##' @return IPHC species common names in countData (or if NULL IPHC species names
##'  in saved set-level IPHC data that are not in iphc-spp-names.csv).
##' @export
##' @examples
##' \dontrun{
##' check_iphc_spp_name()              # All the IPHC names not in
##'                                    #  iphc-spp-names.csv
##' check_iphc_spp_name(countData2013) # All the 2013 IPHC names not in
##'                                    #  iphc-spp-names.csv
##' }
##' @rdname get_early_iphc
check_iphc_spp_name <- function(countData = NULL, ignore_obvious = TRUE) {
  iphc_names <- read.csv(system.file("extdata/iphc-spp-names.csv",
                                     package = "gfiphc"
                                     ),
                         comment.char = "#",
                         as.is = TRUE)
  ignore <- c(
    "Missing Hook/Gangion",
    "Bent Hook",
    "Hook with Skin",
    "Empty Hook",
    "Bent/Broken/Missing"
  )
  if (ignore_obvious) {
    ignore <- c(
      ignore,
      "unident. Starfish",
      "Scallop",
      "Sea Anemone",
      "Unknown/Unspecified",
      "Sea Pen",
      "Oregon Rock Crab",
      "Shells",
      "Box Crab",
      "unident. Sponge",
      "Octopus",
      "unident. Invertebrate",
      "Sea Cucumber",
      "Inanimate Object",
      "Gastropod",
      "Sea Urchin",
      "unident. Coral",
      "Skate Egg Case",
      "unident. Crab",
      "Tunicates",
      "(none)",
      "Red King Crab",
      "Black-footed Albatross",
      "Nudibranch",
      "Bivalve",
      "Steller Sea Lion",
      "Red Tree Coral",
      "Solaster sp (starfish)",
      "Sunflower Sea Star",
      "Gorgonian coral",
      "Giant Pacific Octopus",
      "Fish-eating Star",
      "Sea Whip",
      "Stylaster campylecus (coral)",
      "Sun Sea Star",
      "Jellyfish",
      "Unident. Salmon",
      "unident. organic matter",
      "Dungeness Crab"
    )
  }
  if (!is.null(countData)) {
    data_names <- unique(countData$spNameIPHC)
    new_missing_names <- data_names[!(data_names %in%
      unique(iphc_names$iphc_common_name))]
    new_missing_names <- new_missing_names[!(new_missing_names %in% ignore)]
    return(new_missing_names)
  } else {
    data_names_1995 <- unique(as.character(countData1995$spNameIPHC))
    data_names_1996to2002 <- unique(as.character(data1996to2002$spNameIPHC))
    data_names_2013 <- unique(as.character(countData2013$spNameIPHC))
    data_names_2020 <- unique(as.character(countData2020$spNameIPHC))
    data_names_2021 <- unique(as.character(countData2021$spNameIPHC))
    #data_names_2022 <- unique(as.character(countData2022$spNameIPHC))
    data_names_all <- c(
      data_names_1995,
      data_names_1996to2002,
      data_names_2013,
      data_names_2020,
      data_names_2021#,
      #data_names_2022
    ) %>%
      unique()
    old_missing_names <- data_names_all[!(data_names_all %in%
      unique(iphc_names$iphc_common_name))]
    old_missing_names <- old_missing_names[!(old_missing_names %in% ignore)]
    return(old_missing_names)
  }
}


##' Get the data for IPHC 1995 survey for a given species
##'
##' Details The IPHC report for 1996 mentions 120 stations and does not use the
##'        ones off WCVI that are 'RS' (random sample?) and not in the
##'        same pattern as the 'SG' (Setline Grid) stations. Here using the SG
##'        ones only (the RS ones are saved as setData1995rs and countData1995rs).
##'
##' @return Tibble contains year (1995), station name, lat, lon,
##'           E_it (effective skate number for that station, based on all
##'                   hooks),
##'           N_it (number of 'species' caught on all hooks),
##'           C_it (catch rate of 'species' as number per effective skate,
##'                   based on all hooks),
##'           E_it20 (effective skate number for that station, based on first
##'                   20 hooks, so NA for 1995),
##'           N_it20 (number of 'species' caught in first 20 hooks, so NA for
##'                   1995),
##'           C_it20 (catch rate of 'species' as number per effective skate,
##'                   based on the first 20 hooks, so NA for 1995),
##'           usable (whether or not that station is usable, as deemed by IPHC),
##'           standard (whether or not station is a standard one or in the
##'                     expansion set after 2018; manually setting to Y here as
##'                     stations are not exactly the same as the standard ones
##'                     but are not in the expanded area)
##'
##' If no data at all on that species then C_it and N_it are NA's.
##' @examples
##' \dontrun{
##' yyr1995 <- get_iphc_1995("yelloweye rockfish")
##' summary(yyr1995)
##' }
##'
##' @rdname get_early_iphc
get_iphc_1995 <- function(species) {
  iphc_spp_name <- get_iphc_spp_name(species)

  setVals1995prelim <- summarise(group_by(countData1995, year, station),
    N_it = sum((spNameIPHC == iphc_spp_name) *
      specCount)
  ) %>%
    arrange(station)
  # Need the station-specific information (not all stations will appear in the
  #  setVals1995prelim).
  setVals1995 <- left_join(setData1995, setVals1995prelim, by = "station") %>%
    mutate(C_it = N_it / effSkate) %>%
    mutate(
      E_it20 = as.double(NA),
      N_it20 = as.double(NA),
      C_it20 = as.double(NA),
      standard = as.factor("Y")
    )

  setVals1995$year <- 1995 # some are NA's from the left_join
  # Re-order:
  setVals1995 <- select(setVals1995,
    year,
    station,
    lat,
    lon,
    E_it = effSkate,
    N_it,
    C_it,
    E_it20,
    N_it20,
    C_it20,
    usable,
    standard
  )
  setVals1995
}

##' Get the data for IPHC surveys from 1996-2002 for a given species
##'
##' Details
##'       Note that 1996 is based on all hooks being enumerated while
##'            1997-2002 is for first 20 only.
##' @return Tibble contains year, station name, lat, lon,
##'           E_it (effective skate number for that station, based on all
##'                   hooks, so NA for 1997-2002),
##'           N_it (number of 'species' caught on all hooks, so NA for 1997-2002),
##'           C_it (catch rate of 'species' as number per effective skate,
##'                   based on all hooks, so NA for 1997-2002),
##'           E_it20 (effective skate number for that station, based on first
##'                   20 hooks, so NA for 1996),
##'           N_it20 (number of 'species' caught in first 20 hooks, so NA for
##'                  1996),
##'           C_it20 (catch rate of 'species' as number per effective skate,
##'                   based on the first 20 hooks, so NA for 1996),
##'           usable (whether or not that station is usable, as deemed by IPHC),
##'           standard (whether or not station is a standard one or in the
##'                   expansion set after 2018; all Y here since pre-2018; for
##'                   1996 and 1997 the station layout is not the standard grid
##'                   and the naming of stations is different,
##'                   `intersect(filter(sp_set_counts$set_counts, year == 1996)$station, setDataExpansion$station`
##'                   is empty, but the area fished is similar to later standard
##'                   years so we put Y here)
##'
##' If no data on that species then C_it and N_it are NA's.
##' @examples
##' \dontrun{
##' yyr1996to2002 <- get_iphc_1996to2002("yelloweye rockfish")
##' summary(yyr1996to2002)
##' dplyr::summarise(group_by(yyr1996to2002, year, num.stations = n()))
##' # number of stations in each year.
##' }
##'
##' @rdname get_early_iphc
get_iphc_1996to2002 <- function(species) {
  iphc_spp_name <- get_iphc_spp_name(species)

  setVals1996prelim <- filter(data1996to2002, year < 1996.5)
  setVals1996 <- summarise(group_by(setVals1996prelim, year, station),
    lat = unique(lat),
    lon = unique(lon),
    E_it = unique(E_it),
    N_it = sum((spNameIPHC == iphc_spp_name) * catchCount),
    usable = unique(usable)
  ) %>%
    mutate(
      C_it = N_it / E_it,
      E_it20 = NA,
      N_it20 = NA,
      C_it20 = NA,
      station = as.character(station),
      standard = as.factor("Y")
    ) %>%
    select(
      year,
      station,
      lat,
      lon,
      E_it,
      N_it,
      C_it,
      E_it20,
      N_it20,
      C_it20,
      usable,
      standard
    )

  setVals1997to2002prelim <- filter(data1996to2002, year > 1996.5)
  setVals1997to2002 <- summarise(group_by(setVals1997to2002prelim, year, station),
    lat = unique(lat),
    lon = unique(lon),
    E_it20 = unique(E_it),
    N_it20 = sum((spNameIPHC == iphc_spp_name) *
      catchCount),
    usable = unique(usable)
  ) %>%
    mutate(
      C_it20 = N_it20 / E_it20,
      E_it = NA,
      N_it = NA,
      C_it = NA,
      station = as.character(station),
      standard = as.factor("Y")
    ) %>%
    select(
      year,
      station,
      lat,
      lon,
      E_it,
      N_it,
      C_it,
      E_it20,
      N_it20,
      C_it20,
      usable,
      standard
    )
  rbind(setVals1996, setVals1997to2002)
}


##' Get the data for IPHC 2013 or 2020 to 2022 surveys for a given species, or later
##'  years for which only first 20 hooks were evaluated.
##'
##' When only first 20 hooks were evaluated, the data are in here (not
##' GFbio). See `data-raw/iphc-2020-data.Rmd` and updated
##' `data-raw/iphc-2020-data.Rmd`
##' for details to extract the data
##' from the IPHC website and include in this pacakge. This function should then
##' work for all such years, since the data will be saved in the correct format.
##' Was `get_iphc_2013()` but generalising for future years.
##'
##' Details
##' @param year year of interest, must be 2013, 2020, 2021, or 2022
##' @return Tibble contains year, station name, lat, lon,
##'           E_it (effective skate number for that station, based on all
##'                   hooks, so all NA),
##'           N_it (number of 'species' caught on all hooks, so all NA),
##'           C_it (catch rate of 'species' as number per effective skate,
##'                   based on all hooks, so all NA),
##'           E_it20 (effective skate number for that station, based on first
##'                   20 hooks),
##'           N_it20 (number of 'species' caught in first 20 hooks),
##'           C_it20 (catch rate of 'species' as number per effective skate,
##'                   based on the first 20 hooks),
##'           usable (whether or not that station is usable, as deemed by IPHC),
##'           standard (whether or not station is a standard one or in the
##'                   expansion set after 2018)
##'
##' If no data at all on that species then C_it and N_it are NA's.
##' @examples
##' \dontrun{
##' yyr2013 <- get_iphc_from_gfiphc("yelloweye rockfish", year = 2013)
##' summary(yyr2013)
##' expect_equal(get_iphc_2013("yelloweye rockfish"), yyr2013)  # until I retire get_iphc_2013()
##' }
##' @rdname get_early_iphc
get_iphc_from_gfiphc <- function(species,
                                 year){
  stopifnot(year %in% c(2013, 2020, 2021))  # update this each year
  iphc_spp_name <- get_iphc_spp_name(species)

  countData <- get(paste0("countData", year))
  setData <-  get(paste0("setData", year))

  setValsprelim <- summarise(group_by(countData, year, station),
    N_it20 = sum((spNameIPHC == iphc_spp_name) *
      specCount)
  ) %>%
    arrange(station)

  # Need the station-specific information (though all stations do appear in the
  #  setVals2013prelim, they didn't for 1995; 2020 and 2021 have a column so use that if
  #  it's there).
  setVals <- left_join(setData,
                       setValsprelim,
                       by = "station") %>%
    mutate(C_it20 = N_it20 / E_it20) %>%
    mutate(
      E_it = NA,
      N_it = NA,
      C_it = NA
    )

  # For 2013 there is no standard column.
  if(!("standard" %in% names(setData))){
    setVals <- dplyr::left_join(setVals,
                                select(setDataExpansion,
                                       station,
                                       standard),
                                by = "station")
  }

  setVals$year <- year   # some are NA's from the left_join
  # Re-order:
  setVals <- select(
    setVals,
    year,
    station,
    lat,
    lon,
    E_it,
    N_it,
    C_it,
    E_it20,
    N_it20,
    C_it20,
    usable,
    standard
  )
  setVals
}



##' Get and combine all the IPHC survey data for a given species
##'
##' Combine all the IPHC survey data for a given species, with catch rates
##'  calculated
##'  based on effective skates and numbers of hooks observed. Catch rates are
##'  given for the first 20 hooks and/or for all hooks, depending on the data
##'  collected.
##' @param species Species common name (as used by gfplot), or 'hook with bait'.
##' @return Tibble contains year, station name, lat, lon,
##'           E_it (effective skate number for that station, based on all
##'                   hooks),
##'           N_it (number of 'species' caught on all hooks)
##'           C_it (catch rate of 'species' as number per effective skate,
##'                   based on all hooks)
##'           E_it20 (effective skate number for that station, based on first
##'                   20 hooks),
##'           N_it20 (number of 'species' caught in first 20 hooks),
##'           C_it20 (catch rate of 'species' as number per effective skate,
##'                   based on the first 20 hooks),
##'           usable (whether or not that station is usable, as deemed by IPHC),
##'           standard (whether or not station is a standard one or in the
##'                   expansion set after 2018).
##'
##' @export
get_all_iphc_set_counts <- function(species) {
  bind_rows(
    get_iphc_1995(species),
    get_iphc_1996to2002(species),
    get_iphc_from_gfiphc(species,
                         year = 2013),
    get_iphc_from_gfiphc(species,
                         year = 2020),
    get_iphc_from_gfiphc(species,
                         year = 2021),
    # get_iphc_from_gfiphc(species,
    #                      year = 2022),
    tidy_iphc_survey(
      get_iphc_hooks(species),
      get_iphc_skates_info(),
      get_iphc_sets_info()
    )
  ) %>%
    dplyr::filter(!is.na(year)) %>%
    arrange(year)
}


read_sql <- function(x) {
  if (file.exists(system.file("sql", x, package = "gfiphc"))) {
    readLines(system.file("sql", x, package = "gfiphc"))
  } else {
    stop("The sql file does not exist.")
  }
}


##' Get combined catch counts for a set of species
##'
##' The .rds file for each species needs to already exist, from running
##' `cache_pbs_data_iphc(sp_vec)`.
##' This combines the values together, for example to give data for a number of
##' skates combined, rather than individual species.
##'
##' @param sp_vec vector of IPHC species names
##' @param save_RDS_name name (with .rds suffix) to save the .rds file of the
##'   output (there is no natural obvious default based on `sp_vec`), if NULL
##'   then do not save
##' @param path The folder where the cached data are saved, and the new one will
##'   be saved.
##' @param compress Compress the `.rds` file? Defaults to `FALSE` for faster
##'   reading and writing at the expense of disk space.
##' @return list with  tibble `set_counts` (same format as for individual
##'   species) that contains year, station name, lat, lon, and
##'  *  E_it - effective skate number for that station, based on all hooks (so
##'   NA for 1997-2002, 2013, 2020, 2021, 2022),
##'  * N_it...NUM - number of 'species' caught on all hooks, so NA for
##'   1997-2002, 2013, 2020, 2021, 2022, where NUM is the column number appended by dplyr within this
##'   function,
##'  * N_it_sum - sum of `N_it...NUM`, with NA's removed (treated as zeros)
##'   TODO: say what happens if some have NA
##'  * C_it_sum - catch rate of all 'species' as number per effective skate,
##'   based on all hooks, so NA for 1997-2002, 2013, 2020, 2021, 2022
##'  *  E_it20 - effective skate number for that station, based on first 20
##'   hooks, so NA for 1995 and 1996,
##'  * N_it20...NUM number of 'species' caught in first 20 hooks, so NA for
##'   1995 and 1996, where NUM is the column number appended by dplyr within this
##'  * N_it20_sum - sum of `N_it20...NUM`, with NA's removed (treated as zeros)
##'   TODO: say what happens if some have NA
##'  * C_it20_sum - catch rate all 'species' as number per effective skate,
##'   based on the first 20 hooks, so NA for 1995 and 1996,
##'  * usable  - whether or not that station is usable, as deemed by IPHC,
##'  * standard - whether or not station is a standard one or in the
##'               expansion set after 2018; all Y here since pre-2018; for
##'               1996 and 1997 the station layout is not the standard grid
##'               and the naming of stations is different,
##'               `intersect(filter(sp_set_counts$set_counts, year == 1996)$station, setDataExpansion$station`
##'               is empty, but the area fished is similar to later standard
##'               years so we put Y here
##'
##' If no data on any of the species in `sp_vec` then `C_it_all` and N_it are
##'   NA's. This happens for sure for years when the counts are not
##'   available. Examine each individual species count for NA's to check they
##'   have propagated through okay. Also saves the data in `path/save_RDS_name`.
##'
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'  skate_sp <- c("aleutian skate",
##'                "big skate",
##'                "roughtail skate",
##'                "sandpaper skate",
##'                "longnose skate",
##'                "alaska skate")
##' res <- get_combined_species(sp_vec = skate_sp)
##' # And see `analysis_for_HG_predators` vignette.
##' }
get_combined_species <- function(sp_vec,
                                 save_RDS_name = NULL,
                                 path = ".",
                                 compress = FALSE){
  for(i in 1:length(sp_vec)){
    this_sp <- readRDS(paste0(path, "/", sp_hyphenate(sp_vec[i])))
                             # this will become part of the above function (?)

    if(i == 1){
      # TODO: put error check here - if no 2003-2012 years then need another, more common, species first
      all_sp_set_counts <- this_sp$set_counts %>%
        select(-c(C_it,
                  C_it20))
    } else {
      # This gives error for unidentified, because there are none for GFbio (2003-2012 etc):
      # expect_equal(all_sp_set_counts$station,
      #             this_sp$set_counts$station)

      # This original attempt created columns N_it...<number>, but doesn't work for unidentified
      #  skates (for above reason):
      # all_sp_set_counts <- dplyr::bind_cols(all_sp_set_counts,
      #                                      select(this_sp$set_counts,
      #                                             N_it,
      #                                             N_it20))

      all_sp_set_counts <- dplyr::left_join(all_sp_set_counts,
                                            select(this_sp$set_counts,
                                                   year,
                                                   station,
                                                   N_it,
                                                   N_it20),
                                            by = c("year",
                                                   "station"),
                                            suffix = c("",
                                                       paste0(".", i)))
      # So original one is always N_it and N_it20, rest are N_it.i (i = 2, 3, ...).
      #  Originals become N_it.1 and N_it20.1 in next line
    }
  }

  # Need these for c_across below to work.
  all_sp_set_counts <- dplyr::rename(all_sp_set_counts,
                                     N_it.1 = N_it) %>%
    dplyr::rename(N_it20.1 = N_it20)

  combined_species <- list()
  combined_species$set_counts <- dplyr::rowwise(all_sp_set_counts) %>%
    mutate(N_it_sum = sum(dplyr::c_across(contains("N_it.")), na.rm = TRUE),
           N_it20_sum = sum(dplyr::c_across(contains("N_it20.")), na.rm = TRUE),
           C_it_sum = N_it_sum / E_it,
           C_it20_sum = N_it20_sum / E_it20) %>%
    ungroup()

  # Check that we have properly dealt with how NA's pass through. Want them
  #  to stay NA's if true, but check if we ever get N_it_sum = NA  when E_it != NA but
  #  usable = "Y"? This will break if we do.

  testthat::expect_equal(combined_species$set_counts %>% dplyr::filter(is.na(N_it_sum) &
                                                                       !is.na(E_it) &
                                                                       usable == "Y") %>%
                         nrow(),
                         0)

  # And check the same for the 20-hook counts:
  testthat::expect_equal(combined_species$set_counts %>% dplyr::filter(is.na(N_it20_sum) &
                                                                       !is.na(E_it20) &
                                                                       usable == "Y") %>%
                         nrow(),
                         0)
  if(!is.null(save_RDS_name)){
    dir.create(path, showWarnings = FALSE)
    saveRDS(combined_species,
            file = paste0(file.path(path,
                                    save_RDS_name)), #not quite as cache_pbs_data_iphc
            compress = compress)
  }

  return(combined_species)
}
