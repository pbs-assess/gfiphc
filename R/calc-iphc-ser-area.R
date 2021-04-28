#' Calculations restricted to a user-defined area.
#'
#' Calculate Series E and F from the IPHC data (restricted to a given area)
#'
#' Calculate both Series for as many years as possible, including bootstrapped
#'  values, for the data restricted to a user-defined area.
#' @details The two series are:
#'
#'  Series E: first 20 hooks from each skate (like Series A but restricted to a
#'   given area)
#'
#'  Series F: all hooks from each skate (like Series B but restricted to a given area)
#'
#' @param set_counts species-specific set-level counts from [tidy_iphc_survey()]
#'  or other, but with extra column `in_area` indicating if each station is in
#'   the area being considered or not. See vignette for example.
#' @return list of class `IPHC_ser_E_and_F` containing two tibbles, one for each series (ser_E and ser_F).
#'   Each tibble has one row for each set in each year, restricted to only the
#'   sets within the defined area, with columns
#'   year, station, lat, lon, E_it (effective skate number), N_it (number of
#'   fish caught of the given species), C_it (catch rate of given species, as
#'   numbers per effective skate), E_it20, N_it20 and C_it20 are the same but
#'   considering the first 20 hooks of each skate only, usable (whether the set
#'   should be used, based on IPHC codes; note that some should not be used for
#'   geospatial analysis but are included here).
#'
#' @examples
#' \dontrun{
#' TODO:
#' yelloweye <- tidy_iphc_survey(
#'  get_iphc_hooks("yelloweye rockfish"),
#'  get_iphc_skates_info(),
#'  get_iphc_sets_info()
#' )
#' calc_iphc_ser_all(yelloweye)
#' }
#' @export
calc_iphc_ser_E_and_F <- function(set_counts) {
  stopifnot("in_area" %in% names(set_counts))
  set_counts_usable <- filter(set_counts,
                              usable == "Y",
                              in_area == TRUE)

  # Series E
  ser_E_counts <- filter(
    set_counts_usable,
    !is.na(E_it20)
  )
  ser_E_boot <- boot_iphc(select(
    ser_E_counts,
    year,
    C_it20
  ))

  ser_E <- summarise(group_by(ser_E_counts, year),
    Sets = n(),
    num_pos20 = sum(C_it20 > 0),
    I_t20SampleMean = mean(C_it20)
  ) %>%
    filter(!is.na(I_t20SampleMean)) %>% # NA's got carried through, thinking may get error if this ends up empty
    left_join(ser_E_boot, by = "year")
  names(ser_E)[ names(ser_E) == "I_tBootMean"] <- "I_t20BootMean"
  names(ser_E)[ names(ser_E) == "I_tBootLow"] <- "I_t20BootLow"
  names(ser_E)[ names(ser_E) == "I_tBootHigh"] <- "I_t20BootHigh"
  names(ser_E)[ names(ser_E) == "I_tBootCV"] <- "I_t20BootCV"


  # Series F
  ser_F_counts <- filter(
    set_counts_usable,
    !is.na(E_it)
  )

  ser_F_boot <- boot_iphc(select(
    ser_F_counts,
    year,
    C_it
  ))

  ser_F <- summarise(group_by(ser_F_counts, year),
    Sets = n(),
    num_pos = sum(C_it > 0),
    I_tSampleMean = mean(C_it)
  ) %>%
    filter(!is.na(I_tSampleMean)) %>% # NA's got carried through
    left_join(ser_F_boot, by = "year")

  structure(
    list(ser_E = ser_E,
         ser_F = ser_F),
    class = c("IPHC_ser_E_and_F", "list"))
}


##' Calculate Series EF and test if the scaled E and F are significantly different
##'
##' Calculate Series EF (Series E with 1995 and 1996 appropriately scaled from
##'  Series F)
##' @param series_all List of tibbles, one for each of Series E and F,
##'   resulting from `calc_iphc_ser_E_and_F()`
##' @param return_F_if_same_length logical to return Series F if Series E and F
##'  have the same number of years. User should check which is more suitable and
##'  then change this value if necessary (F uses all hooks but E includes 1995
##'  and 1996)
##'
##' @return List containing
##'
##'   ser_longest: the longest time series possible from
##'   Series E and F,
##'
##'   test_EF: the results from the paired t-test, NULL if Series F is longest.
##'
##'   G_E, G_F: geometric means of nonzero values in Series E and Series F
##'   (based on bootstrapped means).
##'
##'   Longest series is either
##'
##'   (i) Series EF (Series E with 1995 and 1996 appropriately scaled from
##'   Series F) if `test_EF$p.value >= 0.05`, because the p-value means that we
##'   cannot reject the null hypothesis that the true difference
##'   in means of the rescaled (by their geometric means) Series E and F equals 0,
##'
##'   (ii) Series E, which is the longest series available if the rescaled
##'  series are significantly different.
##'
##'   (iii) Series F, if the years for which only the first 20 hooks are enumerated
##'    never caught the species (but other years do). Then Series EF becomes Series
##'    E plus some zeros, so we may as well use Series F that uses all the hooks
##'    and so is more likely to catch the species. This is a rare situation (and
##'    likely arises because not all species were specifically identified in
##'    earlier years (or 2013?), but seems to occur for China Rockfish for
##'   Series AB analysis. Return NULL
##'    for t_EF (to then use later to identify that Series F is the longest
##'    series).
##'
##'  (iv) Series F (since all hooks) if E and F contain the same number of
##'   years, but user can change `return_F_if_same_length` as Series E may make
##'   more sense (since goes back to 1995). If E and F contain the same exact
##'   years then F is always returned.
##'
##' See vignette for example.
calc_iphc_ser_EF <- function(series_all,
                             return_F_if_same_length = TRUE) {
  years_EF <- intersect(series_all$ser_E$year,
                        series_all$ser_F$year)
  # TODO: NOT CHECKED THIS:
  # Series B for English Sole catches none, and only returns 1995 and 1996.
  #  Series A caught one English Sole in 2001. Therefore use A as the longest.
  #  No overlapping years since never caught one when all hooks were evaluated
  #  and we have hook-by-hook data. Could have 1995 and 1996 catching one, so
  #  then may want Series B; but this is only when we catch the odd fish.
  if (length(years_EF) == 0) {
    if (nrow(series_all$ser_E) > nrow(series_all$ser_F)) {
      return(list(
        ser_longest = series_all$ser_E,
        test_EF = list(
          t_EF = NULL,
          G_E = NA,
          G_F = NA
        )
      ))
    } else {
      return(list(
        ser_longest = series_all$ser_F,
        test_EF = list(
          t_EF = NULL,
          G_E = NA,
          G_F = NA
        )
      ))
    }
  }
  # If Series E in overlapping years ends up with no catch (Bluntnose Sixgill
  #  Shark for Series A), then return Series F.
  if (nrow(filter(series_all$ser_E, year %in% years_EF, I_t20BootMean > 0)) == 0) {
    return(list(
      ser_longest = series_all$ser_F,
      test_EF = list(
        t_EF = NULL,
        G_E = NA,
        G_F = NA
      )
    ))
  }

  # Geometric means of each series for the overlapping years, excluding zeros
  G_E <- exp(mean(log(dplyr::filter(series_all$ser_E,
                                    year %in% years_EF,
                                    I_t20BootMean > 0
                                    )$I_t20BootMean)))

  G_F <- exp(mean(log(dplyr::filter(series_all$ser_F,
                                    year %in% years_EF,
                                    I_tBootMean > 0
                                    )$I_tBootMean)))

  # If Series E has same number of years as Series F then return F if years are
  # the same, or return E or F depending on `return_F_if_same_length`
  if (length(unique(series_all$ser_E$year)) ==
      length(unique(series_all$ser_F$year))) {

    if (all(unique(series_all$ser_E$year) == unique(series_all$ser_F$year))) {
      return(list(
        ser_longest = series_all$ser_F,
        test_EF = list(
          t_EF = NULL,
          G_E = G_E,
          G_F = G_F
        )))
    } else {
      if(return_F_if_same_length){
        return(list(
          ser_longest = series_all$ser_F,
          test_EF = list(
            t_EF = NULL,
            G_E = G_E,
            G_F = G_F
          )))
      } else {
        return(list(
          ser_longest = series_all$ser_E,
          test_EF = list(
            t_EF = NULL,
            G_E = G_E,
            G_F = G_F
          )))
      }
    }
  }


  # Scale by G_E, geometric mean of bootstrapped means.
  ser_E_scaled <- filter(
    series_all$ser_E,
    year %in% years_EF
  ) %>%
    mutate(
      I_t20SampleMean = I_t20SampleMean / G_E,
      I_t20BootMean = I_t20BootMean / G_E,
      I_t20BootLow = I_t20BootLow / G_E,
      I_t20BootHigh = I_t20BootHigh / G_E
    )
  # exp(mean(log(ser_A_scaled$I_t20BootMean)))  # =1

  ser_F_scaled <- filter(
    series_all$ser_F,
    year %in% years_EF
  ) %>%
    mutate(
      I_tSampleMean = I_tSampleMean / G_F,
      I_tBootMean = I_tBootMean / G_F,
      I_tBootLow = I_tBootLow / G_F,
      I_tBootHigh = I_tBootHigh / G_F
    )
  # exp(mean(log(ser_B_scaled$I_tBootMean)))  # =1

  t_EF <- stats::t.test(ser_E_scaled$I_t20BootMean,
                        ser_F_scaled$I_tBootMean,
                        paired = TRUE)

  if (t_EF$p.value >= 0.05) {
    # Can't reject null hypothesis that true difference
    #  in means equals 0

    # Multiply the ser_F years not in years_EF by G_E/G_F,
    #  naming columns with 20 since rescaling (and to combine with
    #  series_all$ser_E). Note that num_pos20 is not scaled (as
    #  we're implicitly scaling all the catch rates, but the numbers
    #  of sets won't change).
    ser_EF <- dplyr::filter(series_all$ser_F,
                            !year %in% years_EF) %>%   # years in F but not E
      mutate(
        num_pos20 = num_pos,
        I_t20SampleMean = I_tSampleMean * G_E / G_F,
        I_t20BootMean = I_tBootMean * G_E / G_F,
        I_t20BootLow = I_tBootLow * G_E / G_F,
        I_t20BootHigh = I_tBootHigh * G_E / G_F,
        I_t20BootCV = I_tBootCV
      ) %>%
      select(-c(
        "num_pos",
        "I_tSampleMean",
        "I_tBootMean",
        "I_tBootLow",
        "I_tBootHigh",
        "I_tBootCV"
      )) %>%
      rbind(series_all$ser_E)
    return(
      structure(
        list(
          ser_longest = ser_EF,
          test_EF = list(
            t_EF = t_EF,
            G_E = G_E,
            G_F = G_F)),
        class = "IPHC_ser_EF"))  # TODO - add to above ones once happy with
  } else {
    return(
      structure(
        list(
          ser_longest = series_all$ser_E,
          test_EF = list(
            t_EF = t_EF,
            G_E = G_E,
            G_F = G_F)),
        class = "IPHC_ser_EF"))
  }
}

##' Get data, do calculations and plot longest series for the IPHC survey TODO -
##'  may be worth doing, need the format part for gfsynopsis
##'
##' Get data, do calculations and plot longest series for the IPHC survey for
##'  a given species. Will take a while since queries GFbio (and need to be on DFO
##'  network). WON'T CURRENTLY WORK AS plot_iphc_index() won't currently work.
##' @param sp Species names (as used in gfdata and gfplot).
##' @return For the given species, list containing
##'
##'   iphc_set_counts_sp: list returned from [calc_iphc_full_res()]
##'
##'   iphc_set_counts_sp_format: just the longest series, returned from
##'     [format_iphc_longest()] with calculations and formatting to match that
##'     of other surveys
##'
##'   g_iphc_index: plot of just the iphc data (useful for testing all species)
##'     providing there are some data
##' @export
TODO_E_F_iphc_get_calc_plot <- function(sp) {
  set_counts <- get_all_iphc_set_counts(sp)
  iphc_set_counts_sp <- calc_iphc_full_res(set_counts)
  iphc_set_counts_sp_format <-
    format_iphc_longest(iphc_set_counts_sp$ser_longest)

  if (!is.null(iphc_set_counts_sp)) {
    g_iphc_index <- plot_iphc_index(iphc_set_counts_sp_format)
  } else {
    g_iphc_index <- NULL
  }
  list(
    iphc_set_counts_sp = iphc_set_counts_sp,
    iphc_set_counts_sp_format = iphc_set_counts_sp_format,
    g_iphc_index = g_iphc_index
  )
}
