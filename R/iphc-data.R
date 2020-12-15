#' Get IPHC data
#'
#' @details
#' * `get_iphc_sets()` extracts IPHC survey data at the set level for given
#'    species, from 2003 to present (excluding 2013 which is not in database)
#' * `get_iphc_sets_info()` extracts IPHC survey data regarding each set, with no
#'    species information, to give one unique row (with lat, lon etc.) for each
#'    set, from 2003 to present (excluding 2013 which is not in database)
#' * `get_iphc_skates_info()` extracts IPHC survey data regarding each skate,
#'    with no species information, to give one unique row (with lat, lon etc.)
#'    for each set, from 2003 to present (excluding 2013 which is not in database);
#'    needed for the hooks per skate
#' * `get_iphc_hooks()` extracts IPHC survey data at the hook level for given
#'    species, from 2003 to present (excluding 2013 which is not in database).
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
  .d <- mutate(.d, usable = ifelse(iphcUsabilityCode %in% c(1, 52),
    "Y", "N"
  ))
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
      .d[1, ] <- c(2003, rep(NA, dim(.d)[2] - 1))
    }
    # No data, give NA's
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
      .d[1, ] <- c(2003, rep(NA, dim(.d)[2] - 1))
    }
    # No data, give NA's
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
#' cache_pbs_data_iphc(c(
#'   "redbanded rockfish",
#'   "pacific ocean perch"
#' ))
#' }
cache_pbs_data_iphc <- function(species, file_name = NULL, path = ".",
                                compress = FALSE) {
  dir.create(path, showWarnings = FALSE)
  for (sp_i in seq_along(species)) {
    this_sp <- species[[sp_i]]

    if (is.null(file_name)) {
      this_sp_clean <- gsub("/", "-", gsub(" ", "-", this_sp))
    } else {
      this_sp_clean <- gsub("/", "-", gsub(" ", "-", file_name[[sp_i]]))
    } # Not sure why Sean's works in cache_pbs_data without the { }

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
      "Fish-eating Star"
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
    data_names_all <- c(
      data_names_1995,
      data_names_1996to2002,
      data_names_2013
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

##' Get the data for IPHC 2013 survey for a given species
##'
##' Details
##' @return Tibble contains year (2013), station name, lat, lon,
##'           E_it (effective skate number for that station, based on all
##'                   hooks, so NA for 2013),
##'           N_it (number of 'species' caught on all hooks, so NA for 2013),
##'           C_it (catch rate of 'species' as number per effective skate,
##'                   based on all hooks, so NA for 2013),
##'           E_it20 (effective skate number for that station, based on first
##'                   20 hooks),
##'           N_it20 (number of 'species' caught in first 20 hooks),
##'           C_it20 (catch rate of 'species' as number per effective skate,
##'                   based on the first 20 hooks),
##'           usable (whether or not that station is usable, as deemed by IPHC),
##'           standard (whether or not station is a standard one or in the
##'                   expansion set after 2018; all Y here since 2013)
##'
##' If no data at all on that species then C_it and N_it are NA's.
##' @examples
##' \dontrun{
##' yyr2013 <- get_iphc_2013("yelloweye rockfish")
##' summary(yyr2013[["setVals2013"]])
##' }
##'
##' @rdname get_early_iphc
get_iphc_2013 <- function(species) {
  iphc_spp_name <- get_iphc_spp_name(species)

  setVals2013prelim <- summarise(group_by(countData2013, year, station),
    N_it20 = sum((spNameIPHC == iphc_spp_name) *
      specCount)
  ) %>%
    arrange(station)

  # Need the station-specific information (though all stations do appear in the
  #  setVals2013prelim, they didn't for 1995).
  setVals2013 <- left_join(setData2013, setVals2013prelim, by = "station") %>%
    mutate(C_it20 = N_it20 / E_it20) %>%
    mutate(
      E_it = NA,
      N_it = NA,
      C_it = NA
    ) %>%
    left_join(select(setDataExpansion,
                     station,
                     standard),
              by = "station")

  setVals2013$year <- 2013 # some are NA's from the left_join
  # Re-order:
  setVals2013 <- select(
    setVals2013,
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
  setVals2013
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
##'           usable (whether or not that station is usable, as deemed by IPHC).
##' @export
get_all_iphc_set_counts <- function(species) {
  bind_rows(
    get_iphc_1995(species),
    get_iphc_1996to2002(species),
    get_iphc_2013(species),
    tidy_iphc_survey(
      get_iphc_hooks(species),
      get_iphc_skates_info(),
      get_iphc_sets_info()
    )
  ) %>%
    arrange(year)
}

read_sql <- function(x) {
  if (file.exists(system.file("sql", x, package = "gfiphc"))) {
    readLines(system.file("sql", x, package = "gfiphc"))
  } else {
    stop("The sql file does not exist.")
  }
}
