#' Tidy IPHC survey data
#'
#' For a given species, calculate the catch rates for each skate based on all hooks
#'  and based on the first 20 hooks only, for skates baited with chum (not the
#'  other baits used in the 2012 bait experiment), for the years that we have
#'  hook-by-hook data and all hooks were enumerated (2003-2012, 2014-2017 and
#'  likely later).
#'
#' @param hook_level species-specific hook-level counts from [get_iphc_hooks()].
#' @param skate_info species-independent skate-level information from
#'   [get_iphc_skates_info()].
#' @param set_info species-independent set-level information from
#'   [get_iphc_sets_info()].
#'
#'
#' @return tibble with one row for each set in each year, with columns
#'   year, station, lat, lon, E_it (effective skate number), N_it (number of
#'   fish caught of the given species), C_it (catch rate of given species, as
#'   numbers per effective skate), E_it20, N_it20 and C_it20 are the same but
#'   considering the first 20 hooks of each skate only, usable (whether the set
#'   should be used, based on IPHC codes; some cannot be used for geospatial
#'   analysis but are included here). Returns single row with year 2003 and
#'   all entries NA, so that functions that call this still work.
#' @export
#'
#' @examples
#' \dontrun{
#' yelloweye <- tidy_iphc_survey(
#'   get_iphc_hooks("yelloweye rockfish"),
#'   get_iphc_skates_info(),
#'   get_iphc_sets_info()
#' )
#' }
tidy_iphc_survey <- function(hook_level,
                             skate_info,
                             set_info) {
  # If no data then still return a tibble
  if (nrow(hook_level) == 1) {
    if (is.na(hook_level$numOnHook)) {
      return(tibble(
        year = 2003,
        set = NA,
        station = NA,
        lat = NA,
        lon = NA,
        E_it = NA,
        N_it = NA,
        C_it = NA,
        E_it20 = NA,
        N_it20 = NA,
        C_it20 = NA,
        usable = NA,
        standard = NA
      ))
    }
  }

  # Append some skate info to the hook info, to then calculate counts on
  #  each skate for all hooks and counts for first 20 hooks
  hook_w_skate_info <- left_join(hook_level,
    select(
      skate_info, setID, skateID,
      firstHook, lastHook, hook20
    ),
    by = "skateID"
  ) %>%
    # Calc number on hook providing hook is in first 20
    mutate(numOnHook20 = numOnHook *
      (hook >= (firstHook - 0.1)) *
      (hook <= (hook20 + 0.1)))

  # Summarise to skate level then join with skate info (to include zero counts)
  skate_counts_nonzero <- summarise(group_by(
    hook_w_skate_info,
    skateID
  ),
  species = unique(species),
  countPerSkate = sum(numOnHook),
  countPerSkate20 = sum(numOnHook20)
  )
  skate_counts <- left_join(skate_info,
    skate_counts_nonzero,
    by = "skateID"
  ) %>%
    mutate(
      chumCountPerSkate = countPerSkate * (bait == 110),
      chumCountPerSkate20 = countPerSkate20 * (bait == 110)
    )
  # To get rid of NA's (needed later)
  skate_counts$species <- unique(skate_counts$species)[!is.na(unique(skate_counts$species))]
  skate_counts[is.na(skate_counts)] <- 0

  # Summarise to set level then join with set info (to include zero counts)
  #  and just use chumbait hooks
  set_counts_nonzero <- summarise(group_by(skate_counts, setID),
    H_it = sum(chumObsHooksPerSkate),
    species = unique(species),
    N_it = sum(chumCountPerSkate),
    N_it20 = sum(chumCountPerSkate20),
    hooksChumRatio = H_it /
      sum(obsHooksPerSkate),
    # for B.1 in YYR 2014
    hooksChumRatio20 = sum(chumObsHooksPerSkate20) /
      sum(obsHooksPerSkate)
    )

  # for B.4 in YYR 2014, fraction in ():
  # tilde{H}_it / H^*_{it}
  set_counts <- left_join(set_info,
    set_counts_nonzero,
    by = "setID"
  ) %>%
    mutate(
      E_it = effSkateIPHC * hooksChumRatio,
      E_it20 = effSkateIPHC * hooksChumRatio20
    ) %>%
    mutate(
      C_it = N_it / E_it,
      C_it20 = N_it20 / E_it20
    ) %>%
    select(year,
      station,
      lat,
      lon = long,
      E_it,
      N_it,
      C_it,
      E_it20,
      N_it20,
      C_it20,
      usable,
      standard
    ) %>%
    as_tibble()
}
