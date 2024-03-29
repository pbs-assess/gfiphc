#' Calculate Series A, B, C and D from the IPHC data
#'
#' Calculate all four series for as many years as possible, including bootstrapped
#'  values. Series with no data are treated slightly different to each other
#'  (ser_C ends up as an empty tibble, which may cause problems down the
#'  road). Default is to only use `standard` stations.
#'
#' @details The four series are:
#'
#'  Series A: first 20 hooks from each skate, only north of WCVI
#'
#'  Series B: all hooks from each skate, only north of WCVI
#'
#'  Series C: all hooks from each skate, full coast
#'
#'  Series D: first 20 hooks from each skate, full coast
#'
#' @param set_counts species-specific set-level counts from [tidy_iphc_survey()]
#'  or other.
#' @param lat_cut_off cut off below which sets are excluded for Series A and B,
#'   default is that used in YYR 2014 assessment.
#' @param only_standard only use the standard stations (default is TRUE); for
#'   first synopsis report all stations were indadvertently used for 2018 (see
#'   Issue 14).
#' @return list containing four tibbles, one for each survey (ser_A, ser_B, ser_C
#'   and ser_D). Each tibble has one row for each set in each year, with columns
#'   year, station, lat, lon, E_it (effective skate number), N_it (number of
#'   fish caught of the given species), C_it (catch rate of given species, as
#'   numbers per effective skate), E_it20, N_it20 and C_it20 are the same but
#'   considering the first 20 hooks of each skate only, usable (whether the set
#'   should be used, based on IPHC codes; note that some should not be used for
#'   geospatial analysis but are included here).
#'
#' @examples
#' \dontrun{
#' yelloweye <- tidy_iphc_survey(
#'  get_iphc_hooks("yelloweye rockfish"),
#'  get_iphc_skates_info(),
#'  get_iphc_sets_info()
#' )
#' calc_iphc_ser_all(yelloweye)
#' }
#' @export
calc_iphc_ser_all <- function(set_counts,
                              lat_cut_off = 50.6,
                              only_standard = TRUE) {
  set_counts_usable <- filter(set_counts,
                              usable == "Y",
                              standard == ifelse(only_standard,
                                                 "Y",
                                                 "N"))

  # Series A
  ser_A_counts <- filter(
    set_counts_usable, lat > lat_cut_off,
    !is.na(E_it20)
  )
  ser_A_boot <- boot_iphc(select(
    ser_A_counts,
    year,
    C_it20
  ))

  ser_A <- summarise(group_by(ser_A_counts, year),
    Sets = n(),
    num_pos20 = sum(C_it20 > 0),
    I_t20SampleMean = mean(C_it20)
  ) %>%
    filter(!is.na(I_t20SampleMean)) %>% # NA's got carried through, thinking may get error if this ends up empty
    left_join(ser_A_boot, by = "year")
  names(ser_A)[ names(ser_A) == "I_tBootMean"] <- "I_t20BootMean"
  names(ser_A)[ names(ser_A) == "I_tBootLow"] <- "I_t20BootLow"
  names(ser_A)[ names(ser_A) == "I_tBootHigh"] <- "I_t20BootHigh"
  names(ser_A)[ names(ser_A) == "I_tBootCV"] <- "I_t20BootCV"


  # Series B
  ser_B_counts <- filter(
    set_counts_usable, lat > lat_cut_off,
    !is.na(E_it)
  )

  ser_B_boot <- boot_iphc(select(
    ser_B_counts,
    year,
    C_it
  ))

  ser_B <- summarise(group_by(ser_B_counts, year),
    Sets = n(),
    num_pos = sum(C_it > 0),
    I_tSampleMean = mean(C_it)
  ) %>%
    filter(!is.na(I_tSampleMean)) %>% # NA's got carried through
    left_join(ser_B_boot, by = "year")

  # Years that full coast is covered (have stations off WCVI):
  years_full_coast <- as.data.frame(summarise(group_by(set_counts, year),
    wcvi = sum(lat < lat_cut_off)
  )) %>%
    filter(wcvi > 3) # want >3 stations just in case,
  #  though >0 is fine (up to 2017)

  # Series C
  ser_C_counts <- filter(
    set_counts_usable, year %in% years_full_coast$year,
    !is.na(E_it)
  )

  if (nrow(ser_C_counts) == 0) {       # pacific tomcod
    ser_C_counts <- ser_B_counts[1, ]  # same names
    ser_C_counts[1, "year"] <- 2003    # fake, else breaks later code
    ser_C_counts[1, 2:ncol(ser_C_counts)] <- NA
  }

  ser_C_boot <- boot_iphc(select(
    ser_C_counts,
    year,
    C_it
  ))

  ser_C <- summarise(group_by(ser_C_counts, year),
    Sets = n(),
    num_pos = sum(C_it > 0),
    I_tSampleMean = mean(C_it)
  ) %>%
    filter(!is.na(I_tSampleMean)) %>% # NA's may have got carried through
    left_join(ser_C_boot, by = "year")

  # Series D
  ser_D_counts <- filter(
    set_counts_usable, year %in% years_full_coast$year,
    !is.na(E_it20)
  )

  ser_D_boot <- boot_iphc(select(
    ser_D_counts,
    year,
    C_it20
  ))

  ser_D <- summarise(group_by(ser_D_counts, year),
    Sets = n(),
    num_pos20 = sum(C_it20 > 0),
    I_t20SampleMean = mean(C_it20)
  ) %>%
    filter(!is.na(I_t20SampleMean)) %>% # NA's may have got carried through
    left_join(ser_D_boot, by = "year")
  names(ser_D)[ names(ser_D) == "I_tBootMean"] <- "I_t20BootMean"
  names(ser_D)[ names(ser_D) == "I_tBootLow"] <- "I_t20BootLow"
  names(ser_D)[ names(ser_D) == "I_tBootHigh"] <- "I_t20BootHigh"
  names(ser_D)[ names(ser_D) == "I_tBootCV"] <- "I_t20BootCV"

  list(ser_A = ser_A, ser_B = ser_B, ser_C = ser_C, ser_D = ser_D)
}

##' Do bootstrap calculations on each IPHC Series
##'
##' To be called from within [calc_iphc_ser_all()],
##'  once for each series.
##' @param ser_year_rates Tibble of two columns containing just year and catch
##'   rates (either C_it or C_it20 depending on the Series).
##' @param num.boots Number of bootstrap replicates to do.
##' @param seed_val Seed for random number generator.
##' @return Tibble containg one row for each year, with columns year, I_tBootMean,
##'   I_tBootLow, I_tBootHigh and I_tBootCV. If ser_year_rates is NULL or all
##'   counts are NA then return year = 2003 and all NA's (to not break later code).
boot_iphc <- function(ser_year_rates,
                      num.boots = 10000,
                      seed_val = 42) {
  if (dim(ser_year_rates)[2] != 2) stop("Tibble must have only two columns.")

  return_NA <- FALSE

  if (nrow(ser_year_rates) == 0 | all(is.na(ser_year_rates[, 2]))) {
    return_NA <- TRUE
  } else {
    # Get rid of the NA years (should make the if below redundant):
    ser_year_rates <- ser_year_rates[which(!is.na(ser_year_rates[, 2])), ]
    if (nrow(ser_year_rates) == 1) {
      if (is.na((ser_year_rates[1, 2]))) {
        return_NA <- TRUE
      }
    }
  }
  if (return_NA) {
    return(tibble(
      year = 2003,
      I_tBootMean = NA,
      I_tBootLow = NA,
      I_tBootHigh = NA,
      I_tBootCV = NA
    )) # Just one row (year), even though gets
    #  left_join'ed in calc_iphc_ser_all
    #  with multiple years
  }

  meanFun <- function(x, I) mean(x[I])

  unique_years <- unique(ser_year_rates$year)
  bcaConf <- tibble(year = unique_years) %>%
    mutate(
      I_tBootMean = NA_real_,
      I_tBootLow = NA_real_,
      I_tBootHigh = NA_real_,
      I_tBootCV = NA_real_
    )

  set.seed(seed_val)
  bool <- list() # list of boot results
  boolCI <- list() # list of boot.ci results
  for (i in 1:length(unique_years))
  {
    this_year_rates <- filter(ser_year_rates, year == unique_years[i]) %>%
      pull(names(ser_year_rates)[2])
    if (all(this_year_rates == 0)) { # e.g. China Rockfish for 2003,
      #  NA for earlier years, else
      #  error in boot.ci.
      bcaConf[bcaConf$year == unique_years[i], "I_tBootMean"] <- 0
      bcaConf[bcaConf$year == unique_years[i], "I_tBootLow"] <- 0
      bcaConf[bcaConf$year == unique_years[i], "I_tBootHigh"] <- 0
      bcaConf[bcaConf$year == unique_years[i], "I_tBootCV"] <- NA
    } else {
      bool[[i]] <- boot::boot(this_year_rates, meanFun, R = num.boots)

      bcaConf[bcaConf$year == unique_years[i], "I_tBootMean"] <-
        mean(bool[[i]]$t)

      boolCI[[i]] <- boot::boot.ci(bool[[i]], type = "bca")
      bcaConf[bcaConf$year == unique_years[i], "I_tBootLow"] <-
        boolCI[[i]]$bca[4]
      bcaConf[bcaConf$year == unique_years[i], "I_tBootHigh"] <-
        boolCI[[i]]$bca[5]

      bcaConf[bcaConf$year == unique_years[i], "I_tBootCV"] <-
        sd(bool[[i]]$t) /
          bcaConf[bcaConf$year == unique_years[i], "I_tBootMean"]
    }
  }
  bcaConf
}


##' Calculate Series AB and test if the scaled A and B are significantly different
##'
##' Calculate Series AB (Series A with 1995 and 1996 appropriately scaled from
##'  Series B)
##' @param series_all List of tibbles, one for each of Series A, B, C and D,
##'   resulting from [calc_iphc_ser_all()]
##' @return List containing
##'
##'   - ser_longest: the longest time series possible from
##'   Series A and B,
##'
##'   - test_AB: the results from the paired t-test, NULL if Series B is longest.
##'
##'   - G_A, G_B: geometric means of nonzero values in Series A and Series D
##'   (based on bootstrapped means).
##'
##'   - type: which type the longest series is, either `AB`, `A`, `B`, or
##'   possibly `C` based on the descriptions below.
##'
##' Longest series is either
##'
##'   (i) Series AB (Series A with 1995 and 1996 appropriately scaled from
##'   Series B) if `test_AB$p.value >= 0.05`, because the p-value means that we
##'   cannot reject the null hypothesis that the true difference
##'   in means of the rescaled (by their geometric means) Series A and B equals 0,
##'
##'   (ii) Series A, which is the longest series available if the rescaled
##'  series are significantly different.
##'
##'   (iii) Series B, if the years for which only the first 20 hooks are enumerated
##'    never catch the species (but other years do). Then Series AB becomes Series
##'    A plus some zeros, so we may as well use Series B that uses all the hooks
##'    and so is more likely to catch the species. This is a rare situation (and
##'    likely arises because not all species were specifically identified in
##'    earlier years (or 2013?), but seems to occur for China Rockfish. Return NULL
##'    for t_AB (to then use later to identify that Series B is the longest
##'    series).
##'
##'    (iv) But can also have Series C being the longest if A is all 0's and
##'    B and C cover the same years. Looks like this happens for Darkblotched.
##'    Had said this was not implemented yet - see Issue 10, but there is an
##'    instance below.
calc_iphc_ser_AB <- function(series_all) {
  years_AB <- intersect(series_all$ser_A$year, series_all$ser_B$year)
  # Series B for English Sole catches none, and only returns 1995 and 1996.
  #  Series A caught one English Sole in 2001. Therefore use A as the longest.
  #  No overlapping years since never caught one when all hooks were evaluated
  #  and we have hook-by-hook data. Could have 1995 and 1996 catching one, so
  #  then may want Series B; but this is only when we catch the odd fish.
  if (length(years_AB) == 0) {
    if (nrow(series_all$ser_A) > nrow(series_all$ser_B)) {
      return(list(
        ser_longest = series_all$ser_A,
        test_AB = list(
          t_AB = NULL,
          G_A = NA,
          G_B = NA,
          type = "A"
        )
      ))
    } else {
      return(list(
        ser_longest = series_all$ser_B,
        test_AB = list(
          t_AB = NULL,
          G_A = NA,
          G_B = NA,
          type = "B"
        )
      ))
    }
  }
  # If Series A in overlapping years ends up with no catch (Bluntnose Sixgill
  #  Shark), then return Series B.
  if (nrow(filter(series_all$ser_A, year %in% years_AB, I_t20BootMean > 0)) == 0) {
    return(list(
      ser_longest = series_all$ser_B,
      test_AB = list(
        t_AB = NULL,
        G_A = NA,
        G_B = NA,
        type = "B"
      )
    ))
  }
  # Geometric means of each series for the overlapping years, excluding zeros
  G_A <- exp(mean(log(filter(
    series_all$ser_A,
    year %in% years_AB,
    I_t20BootMean > 0
  )$I_t20BootMean)))

  G_B <- exp(mean(log(filter(
    series_all$ser_B,
    year %in% years_AB,
    I_tBootMean > 0
  )$I_tBootMean)))

  # If Series A has no more years than Series B then just return Series B....
  if (length(unique(series_all$ser_A$year)) ==
    length(unique(series_all$ser_B$year))) {
    if (all(unique(series_all$ser_A$year) == unique(series_all$ser_B$year))) {
      # ....unless also B and C have the same years, then return C for the
      # full coast. Sandpaper skate may be the only one - turns out 2013
      # and pre-2003 are empty so Series D is not longer (may not have that
      # possibility included) .
      if (all.equal(series_all$ser_B$year, series_all$ser_C$year)) {
        return(list(
          ser_longest = series_all$ser_C,
          test_AB = list(
            t_AB = NULL,
            G_A = NA,
            G_B = NA,
            type = "C"
          )
        ))
      } else {
        return(list(
          ser_longest = series_all$ser_B,
          test_AB = list(
            t_AB = NULL,
            G_A = G_A,
            G_B = G_B,
            type = "B"
          )
        ))
      }
    }
  }

  # Scale by G_A, geometric mean of bootstrapped means.
  ser_A_scaled <- filter(
    series_all$ser_A,
    year %in% years_AB
  ) %>%
    mutate(
      I_t20SampleMean = I_t20SampleMean / G_A,
      I_t20BootMean = I_t20BootMean / G_A,
      I_t20BootLow = I_t20BootLow / G_A,
      I_t20BootHigh = I_t20BootHigh / G_A
    )
  # exp(mean(log(ser_A_scaled$I_t20BootMean)))  # =1

  ser_B_scaled <- filter(
    series_all$ser_B,
    year %in% years_AB
  ) %>%
    mutate(
      I_tSampleMean = I_tSampleMean / G_B,
      I_tBootMean = I_tBootMean / G_B,
      I_tBootLow = I_tBootLow / G_B,
      I_tBootHigh = I_tBootHigh / G_B
    )
  # exp(mean(log(ser_B_scaled$I_tBootMean)))  # =1

  t_AB <- stats::t.test(ser_A_scaled$I_t20BootMean,
    ser_B_scaled$I_tBootMean,
    paired = TRUE
  )

  if (t_AB$p.value >= 0.05) { # Can't reject null hypothesis that true difference
    #  in means equals 0
    # Multiply the ser_B years not in years_AB by G_A/G_B,
    #  naming columns with 20 since rescaling (and to combine with
    #  series_all$ser_A. Note that num_pos20 is not scaled (as
    #  we're implicitly scaling all the catch rates, but the numbers
    #  of sets won't change).
    ser_AB <- filter(series_all$ser_B, !year %in% years_AB) %>%
      mutate(
        num_pos20 = num_pos,
        I_t20SampleMean = I_tSampleMean * G_A / G_B,
        I_t20BootMean = I_tBootMean * G_A / G_B,
        I_t20BootLow = I_tBootLow * G_A / G_B,
        I_t20BootHigh = I_tBootHigh * G_A / G_B,
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
      rbind(series_all$ser_A)
    return(list(
      ser_longest = ser_AB,
      test_AB = list(
        t_AB = t_AB,
        G_A = G_A,
        G_B = G_B,
        type = "AB"
      )
    ))
  } else {
    return(list(
      ser_longest = series_all$ser_A,
      test_AB = list(
        t_AB = t_AB,
        G_A = G_A,
        G_B = G_B,
        type = "A"
      )
    ))
  }
}

##' Compare Series A and D to see if A can be considered a relative index for the whole coast
##'
##' Compare Series A and D to see if A can be considered a relative index for
##'  the whole coast.
##' @param series_all List of tibbles, one for each of Series A, B, C and D,
##'   resulting from [calc_iphc_ser_all()]
##' @return List of t_AD (results of the paired t-test), and geometric means G_A
##'   and G_D. t_AD is NULL if there is no catch in the overlapping years for
##'   Series A or Series D.
##'
compare_iphc_ser_A_D <- function(series_all) {
  years_AD <- intersect(series_all$ser_A$year, series_all$ser_D$year)

  # Geometric means of each series for the overlapping years
  G_A <- exp(mean(log(filter(
    series_all$ser_A,
    year %in% years_AD,
    I_t20BootMean > 0
  )$I_t20BootMean)))

  G_D <- exp(mean(log(filter(
    series_all$ser_D,
    year %in% years_AD,
    I_t20BootMean > 0
  )$I_t20BootMean)))
  # If Series A and Series D all have 0 counts for all the intersecting years
  #  then return NULL for t_AD (else t-test fails), e.g. Darkblotched.
  # if(length(unique(series_all$ser_A$year)) ==
  #                              length( unique(series_all$ser_D$year) ) ){
  #   if(all(unique(series_all$ser_A$year) == unique(series_all$ser_D$year))) {
  #       if( max( c( series_all$ser_A$I_t20SampleMean,
  #                   series_all$ser_D$I_t20SampleMean) ) == 0) {
  #          return(list(t_AD = NULL,
  #                      G_A = G_A,
  #                      G_D = G_D) )
  #       }
  #    }
  #

  # Actually just want the intersecting years, or if either A or D is 0 in those
  #  (Bluntnose Sixgill Shark, I'm looking at you)

  if ((max(filter(
    series_all$ser_A,
    year %in% years_AD
  )$I_t20SampleMean) == 0) |
    max(filter(
      series_all$ser_D,
      year %in% years_AD
    )$I_t20SampleMean) == 0) {
    return(list(
      t_AD = NULL,
      G_A = G_A,
      G_D = G_D
    ))
  }

  # Scale by G_A, geometricetric mean of bootstrapped means.
  ser_A_scaled <- filter(
    series_all$ser_A,
    year %in% years_AD
  ) %>%
    mutate(
      I_t20SampleMean = I_t20SampleMean / G_A,
      I_t20BootMean = I_t20BootMean / G_A,
      I_t20BootLow = I_t20BootLow / G_A,
      I_t20BootHigh = I_t20BootHigh / G_A
    )
  # exp(mean(log(ser_A_scaled$I_t20BootMean)))  # =1

  ser_D_scaled <- filter(
    series_all$ser_D,
    year %in% years_AD
  ) %>%
    mutate(
      I_t20SampleMean = I_t20SampleMean / G_D,
      I_t20BootMean = I_t20BootMean / G_D,
      I_t20BootLow = I_t20BootLow / G_D,
      I_t20BootHigh = I_t20BootHigh / G_D
    )

  t_AD <- stats::t.test(ser_A_scaled$I_t20BootMean,
    ser_D_scaled$I_t20BootMean,
    paired = TRUE
  )
  list(t_AD = t_AD, G_A = G_A, G_D = G_D)
}


##' Compare Series B and C to see if A can be considered a relative index for the whole coast
##'
##' Compare Series B and C to see if A can be considered a relative index for
##'  the whole coast
##' @param series_all List of tibbles, one for each of Series A, B, C and D,
##'   resulting from [calc_iphc_ser_all()]
##' @return List of t_BC (results of the paired t-test), and geometric means G_B
##'   and G_C.
compare_iphc_ser_B_C <- function(series_all) {
  years_BC <- intersect(series_all$ser_B$year, series_all$ser_C$year)

  # Years in C should be a subset of those in B, but for no data they get
  #  returned slightly differently (English Sole).
  if (length(years_BC) == 0) {
    return(list(
      t_BC = NULL,
      G_B = NA,
      G_C = NA
    ))
  }
  # Geometric means of each series for the overlapping years
  G_B <- exp(mean(log(filter(
    series_all$ser_B,
    year %in% years_BC,
    I_tBootMean > 0
  )$I_tBootMean)))

  G_C <- exp(mean(log(filter(
    series_all$ser_C,
    year %in% years_BC,
    I_tBootMean > 0
  )$I_tBootMean)))

  # For widow rockfish needed this, until I realised the only fish ever caught
  #  was in 2018 at an expansion station, which is now fixed in
  #  calc_iphc_full_res(). This may be needed for other species:
  if (is.na(G_B)){
    return(list(
      t_BC = NULL,
      G_B = NA,
      G_C = G_C    # may be NaN not NA
    ))
  }

  if (is.na(G_C)){
    return(list(
      t_BC = NULL,
      G_B = G_B,
      G_C = NA
    ))
  }

  # Scale by G_B, geometric mean of bootstrapped means.
  ser_B_scaled <- filter(
    series_all$ser_B,
    year %in% years_BC
  ) %>%
    mutate(
      I_tSampleMean = I_tSampleMean / G_B,
      I_tBootMean = I_tBootMean / G_B,
      I_tBootLow = I_tBootLow / G_B,
      I_tBootHigh = I_tBootHigh / G_B
    )
  # exp(mean(log(ser_B_scaled$I_tBootMean)))  # =1

  ser_C_scaled <- filter(
    series_all$ser_C,
    year %in% years_BC
  ) %>%
    mutate(
      I_tSampleMean = I_tSampleMean / G_C,
      I_tBootMean = I_tBootMean / G_C,
      I_tBootLow = I_tBootLow / G_C,
      I_tBootHigh = I_tBootHigh / G_C
    )

  t_BC <- stats::t.test(ser_B_scaled$I_tBootMean,
    ser_C_scaled$I_tBootMean,
    paired = TRUE
  )
  list(t_BC = t_BC, G_B = G_B, G_C = G_C)
}


##' Do all the calculations for the IPHC survey.
##'
##' From species-specific set-level counts, derive all the required Series and
##'  test their equivalence.
##' @param set_counts species-specific set-level counts from [tidy_iphc_survey()]
#'  or other.
##' @return List containing
##'
##' ser_longest: tibble for the longest time series that can be made for this
##'     species, as output from [calc_iphc_ser_AB()]; either Series A or AB.
##'     Or Series B if a species is never caught when only the first 20 hooks
##'     are enumerated (1997-2002 or 2013) but is caught when all hooks are
##'     enumerated - then Series AB is just Series A but only includes years
##'     that are in Series B, so we may as well use Series B (all hooks).
##'     May need to think a little more. Just need results of test_BC to
##'     be mentioned later.
##' full_coast: whether or not the longest time series can be considered
##'     representative of the full coast (based on the paired t-tests).
##'
##' ser_all: Series A, B, C and D, as output from [calc_iphc_ser_all()].
##'
##' test_AB: t-test results from [calc_iphc_ser_AB()]
##'
##' test_AD: t-test results from [compare_iphc_ser_A_D()]
##'
##' test_BC: t-test results from [compare_iphc_ser_B_C()]
##'
##' If no observations at all for the species then return NULL.
##' @export
calc_iphc_full_res <- function(set_counts) {
  if (length(unique(c(set_counts$N_it, set_counts$N_it20))) == 1) {
    if (is.na(unique(c(set_counts$N_it, set_counts$N_it20)))) {
      return(NULL)
    }
  }
  series_all <- calc_iphc_ser_all(set_counts)

  # calc_iphc_ser_all defaults to only including standard stations, so need to
  #  check here that we still have counts (we don't for widow rockfish, just
  #  lots of 0's; only catch is in expansion station in 2018):
  if(max(c(series_all$ser_A$num_pos20,
           series_all$ser_B$num_pos,
           series_all$ser_C$num_pos,
           series_all$ser_D$num_pos20)) == 0){
    return(NULL)
  }

  iphc_ser_longest <- calc_iphc_ser_AB(series_all)
  # list of longest series and
  #  paired t-test results
  test_AD <- compare_iphc_ser_A_D(series_all)
  # test_AD$t_AD$p.value                       # Need to say if >0.05 then
  #  Series A representative
  #  of whole coast (having
  # compared with Series D)

  test_BC <- compare_iphc_ser_B_C(series_all)
  # Need to say if >0.05 then
  #  Series B representative
  #  of whole coast (having
  #  compared with Series C)
  # full_coast is TRUE if the longest time series is representative of
  #  the full coast:
  if (!is.null(test_AD$t_AD)) {
    if (is.na(test_AD$t_AD$p.value)) {
      full_coast <- (test_BC$t_BC$p.value >= 0.05) # as B is the longest, see comment below
    } else {
      if (test_AD$t_AD$p.value >= 0.05) {
        full_coast <- (test_AD$t_AD$p.value >= 0.05 &
          test_BC$t_BC$p.value >= 0.05)
      } else {
        full_coast <- (test_AD$t_AD$p.value >= 0.05) # as A is the longest
      }
    }
  } else {
    # test_AD$t_AD is NULL because no catches in either, so B or C then the
    #  longest, currently can only be B (may not have the situation where C
    # is yet - do now, sandpaper skate, so putting in extra check below)
    full_coast <- (test_BC$t_BC$p.value >= 0.05) # as B is the longest
  }
  # Double check if B or C is longest. Maybe no longer all of the above checks.
  if (isTRUE(all.equal(iphc_ser_longest$ser_longest, series_all$ser_B))) {
    full_coast <- TRUE
  }
  if (isTRUE(all.equal(iphc_ser_longest$ser_longest, series_all$ser_C))) {
    full_coast <- TRUE
  }

  list(
    ser_longest = iphc_ser_longest$ser_longest,
    type = iphc_ser_longest$test_AB$type,
    full_coast = full_coast,
    ser_all = series_all,
    test_AB = iphc_ser_longest$test_AB,
    test_AD = test_AD,
    test_BC = test_BC
  )
}

##' Format the longest IPHC time series index to agree with other surveys
##'
##' Format the longest IPHC time series index to agree with other surveys so
##'   that plot_survey_index() in gfplot (and may get moved to here) works automatically. So the mean catch rate
##'   gets renames as `biomass' even though it's numbers per effective skate.
##'
##' @param iphc_set_counts_sp Output from [calc_iphc_full_res()] (only actually
##'   need the ser_longest component and test_AB).
##' @return Renamed ser_longest, with some required columns calculated.
##' @examples
##' \dontrun{
##' # If already loaded data via gfsynopsis then
##' dat_iphc <- readRDS("../gfsynopsis/report/data-cache/iphc/yelloweye-rockfish.rds")
##' set_counts <- dat_iphc$set_counts
##' # Else to load from scratch:
##' # set_counts <- get_all_iphc_set_counts("yelloweye rockfish")
##' iphc_set_counts_sp <- calc_iphc_full_res(set_counts)
##' format_iphc_longest(iphc_set_counts_sp)
##' # Has no data for early years or 2013:
##' sp = "china rockfish"
##' # If already loaded data via gfsynopsis then
##' dat_iphc <- readRDS("../gfsynopsis/report/data-cache/iphc/china-rockfish.rds")
##' set_counts <- dat_iphc$set_counts
##' # Else to load from scratch:
##' # set_counts <- get_all_iphc_set_counts(sp)
##' iphc_set_counts_sp <- calc_iphc_full_res(set_counts)
##' format_iphc_longest(iphc_set_counts_sp)
##' }
##' @export
format_iphc_longest <- function(iphc_set_counts_sp) {
  if (is.null(iphc_set_counts_sp$ser_longest)) {
    return(tibble(
      survey_abbrev = "IPHC FISS",
      year = 2003,
      biomass = NA,
      lowerci = NA,
      upperci = NA,
      mean_cv = NA,
      num_sets = NA,
      num_pos_sets = NA
    ))
  }
  # Series AB is longest, or occasionally Series A (English Sole)
  if ("I_t20SampleMean" %in% names(iphc_set_counts_sp$ser_longest)) {
    new_names <- select(iphc_set_counts_sp$ser_longest,
      year = year,
      biomass = I_t20BootMean,
      lowerci = I_t20BootLow,
      upperci = I_t20BootHigh,
      num_pos_sets = num_pos20,
      num_sets = Sets
    ) %>%
      mutate(
        mean_cv =
          mean(iphc_set_counts_sp$ser_longest$I_t20BootCV,
            na.rm = TRUE
          ),
        survey_abbrev = "IPHC FISS"
      ) %>%
      select(
        survey_abbrev,
        everything()
      )
  } else { # This would be Series B or C (sandpaper skate) cases
    new_names <- select(iphc_set_counts_sp$ser_longest,
      year = year,
      biomass = I_tBootMean,
      lowerci = I_tBootLow,
      upperci = I_tBootHigh,
      num_pos_sets = num_pos,
      num_sets = Sets
    ) %>%
      mutate(
        mean_cv =
          mean(iphc_set_counts_sp$ser_longest$I_tBootCV,
            na.rm = TRUE
          ),
        survey_abbrev = "IPHC FISS"
      ) %>%
      select(
        survey_abbrev,
        everything()
      )
  }
  # This happend for Pearly Prickleback, 0's made it through giving NaN not
  #  NA for CV, messing up figure.
  if (max(new_names$biomass) == 0) {
    return(tibble(
      survey_abbrev = "IPHC FISS",
      year = 2003,
      biomass = NA,
      lowerci = NA,
      upperci = NA,
      mean_cv = NA,
      num_sets = NA,
      num_pos_sets = NA
    ))
  }
  new_names
}

##' Get data, do calculations and plot Series A, B and longest series possible
##' (usually Series AB) for the IPHC survey, and maybe save results
##'
##' Get data, do calculations and plot longest series for the IPHC survey for
##'  a given species. For species that have several years of zero catches,
##'  Series B may be better to use (but shorter) than Series AB because all
##'  hooks were counted. For example Walleye Pollock in 2016 -- see example.
##'
##'  Will take a while since queries GFbio (and need to be on DFO
##'  network). Basically a wrapper for the calculations in the vignette
##'  `data_for_one_species`. Based on `iphc_get_calc_plot_area()`. See
##'  `gfsynopsis::iphc_get_calc_plot()` for function for gfsynopsis
##'  reports. Calling this one `iphc_get_calc_plot_full()` to distinguish it.
##'
##' @param sp Species names (as used in gfdata and gfplot). Or something like
##'   "skates combined" -- see vignette.
##' @param cached_data if TRUE then use cached data (path_data/sp-name.rds)
##' @param cached_results if TRUE then use cached results
##'   (path_results/sp-name-results.rds), else do calculations here and save results
##' @param verbose if TRUE then print out some of the data (useful in vignette loops)
##' @param print_sp_name if TRUE then print out species name (useful in vignette
##'   loops)
##' @param path_data path to save or load the cached data
##' @param path_results path to save or load the cached data
##' @return Saves results in `path_results/species-name-results.RDS`. If `path_results`
##'   is NULL then do not save. For the given species, return list containing
##'
##'   sp_set_counts: list with one element (for consistency), a tibble
##'   `set_counts` (the .RDS file saved when doing `cache_pbs_data_iphc(sp)`).
##'
##'   ser_ABCD_full: list of output from `calc_iphc_full_res()` from doing all
##'    calculations.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' iphc_get_calc_plot_full("redbanded rockfish", cached_data = FALSE) # only at PBS
##' # Example where longest series may not be the most useful since only looking
##'   at first 20 hooks gives all zeros in 2016, but looking at all hooks gives
##'   some non-zero sets. In practice, catches may be too sparse to be useful anyway.
##' x <- iphc_get_calc_plot_full("walleye pollock", cached = FALSE) # only PBS
##' filter(x$series_ABCD_full$ser_longest, year == 2016)
##' # A tibble: 1 x 8
##'   year  Sets num_pos20 I_t20SampleMean I_t20BootMean I_t20BootLow I_t20BootHigh
##'  <dbl> <int>     <int>           <dbl>         <dbl>        <dbl>         <dbl>
##'   2016   132         0               0             0            0             0
##' ... with 1 more variable: I_t20BootCV <dbl>
##' filter(x$series_ABCD_full$ser_all$ser_B, year == 2016)
##' # A tibble: 1 x 8
##'    year  Sets num_pos I_tSampleMean I_tBootMean I_tBootLow I_tBootHigh I_tBootCV
##'   <dbl> <int>   <int>         <dbl>       <dbl>      <dbl>       <dbl>     <dbl>
##' 1  2016   132       2       0.00252     0.00249          0     0.00629     0.719
##' }
iphc_get_calc_plot_full <- function(sp,
                                    cached_data = TRUE,
                                    cached_results = FALSE,
                                    verbose = FALSE,
                                    print_sp_name = TRUE,
                                    path_data = ".",
                                    path_results = NULL){
  if(!cached_data){
    cache_pbs_data_iphc(sp,
                        path = path_data)
  }

  if(print_sp_name){
    print(paste("*****", sp, "*****"))
  }

  sp_set_counts <- readRDS(paste0(path_data, "/", sp_hyphenate(sp)))

  # For combined species:
  if(!("N_it" %in% names(sp_set_counts)) &
     "N_it_sum" %in% names(sp_set_counts)){
    sp_set_counts <- sp_set_counts %>%
      dplyr::rename(N_it = N_it_sum,
                    N_it20 = N_it20_sum,
                    C_it = C_it_sum,
                    C_it20 = C_it20_sum)
  }

  results_RDS_name <- paste0(file.path(path_results,
                                       sp_hyphenate(sp,
                                                    results = TRUE)))
  if(cached_results){
      series_ABCD_full <- readRDS(results_RDS_name)[["series_ABCD_full"]]
    } else {
      series_ABCD_full <- calc_iphc_full_res(sp_set_counts$set_counts)
    }

  if(verbose){
    # Print the first and last values:
    print(sp_set_counts)
    print(tail(sp_set_counts$set_counts))
    print(series_ABCD_full)
    print(paste("*****", sp, "*****"))
  }

  plot_IPHC_ser_four_panels_ABCD(series_ABCD_full,
                                 sp = sp)

  res <- list(sp_set_counts = sp_set_counts,
              series_ABCD_full = series_ABCD_full)

  if(!cached_results){
    if(!is.null(path_results)){
      dir.create(path_results,
                 showWarnings = FALSE)
      saveRDS(res,
              file = results_RDS_name,
              compress = FALSE)
    }
  }
  return(res)
}

##' These next two functions are originally from gfsynopsis, but now that
##'  uses the same approach to plot all surveys (nothing special about iphc);
##'  the IPHC results are wrangled into the same format as for the
##'  others surveys. So these two functions are somewhat redundant, but leaving here in
##'  case someone wants to create the gfsynopsis-style plots for just IPHC
##'  data, by uncommenting the iphc_plot part of plot_iphc_index(). Note this
##'  would then require gfplot (which requires many more packages). So maybe
##'  adapt the function below outside of gfiphc, to keep dependencies down for
##'  gfiphc (thank you!).

##' Plot just the IPHC survey index, though works for all surveys - WON'T
##'   CURRENTLY WORK as commenting out reference to gfplot, need to move that
##'   function here but want to test first. Moving to gfsynopsis or gfplot.
##'
##' Plot just the IPHC survey index (mainly for testing).
##' @param iphc_set_counts_sp_format Set counts for a given species formatted
##'  in the same way as the other survey series, using [format_iphc_longest()]
##' @return ggplot object of the plot
plot_iphc_index <- function(iphc_set_counts_sp_format) {
  survey_cols <- c(
    RColorBrewer::brewer.pal(7L, "Set2"),
    "#303030", "#a8a8a8", "#a8a8a8", "#a8a8a8"
  )
#  iphc_plot <- iphc_set_counts_sp_format %>%
#    gfplot::plot_survey_index(
#      col = c("grey60", "grey20"),
#      survey_cols = survey_cols,
#      xlim = c(1984, 2017)
#    )
#  iphc_plot
}

##' Get data, do calculations and plot longest series for the IPHC survey -
##' original used for first gfsynopsis report.
##'
##' Get data, do calculations and plot longest series for the IPHC survey for
##'  a given species. Will take a while since queries GFbio (and need to be on DFO
##'  network). WON'T CURRENTLY WORK AS plot_iphc_index() won't currently work
##'  (we previously had everything in just gfplot). See `iphc_get_calc_plot()`
##'  to use separately, then move this and plot_iphc_index to use gfsynopsis
##'  (Issue 140 in gfsynopsis).
##'
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
iphc_get_calc_plot_orig <- function(sp) {
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
