#' Calculations restricted to a user-defined area.
#'
#' Calculate Series E and F from the IPHC data (restricted to a given area)
#'
#' Calculate both Series for as many years as possible, including bootstrapped
#'  values, for the data restricted to a user-defined area and (default is)
#'  standard stations only.
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
#' @param only_standard only use the standard stations (default is TRUE).
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
calc_iphc_ser_E_and_F <- function(set_counts,
                                  only_standard = TRUE) {
  stopifnot("in_area" %in% names(set_counts))

  set_counts_usable <- filter(set_counts,
                              usable == "Y",
                              in_area == TRUE,
                              standard == ifelse(only_standard,
                                                 "Y",
                                                 "N"))
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
##'   type: which type the longest series is, either `EF`, `E`, or `F`, based on
##'   the descriptions below.
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
          G_F = NA,
          type = "E"
        )
      ))
    } else {
      return(list(
        ser_longest = series_all$ser_F,
        test_EF = list(
          t_EF = NULL,
          G_E = NA,
          G_F = NA,
          type = "F"
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
        G_F = NA,
        type = "F"
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
          G_F = G_F,
          type = "F"
        )))
    } else {
      if(return_F_if_same_length){
        return(list(
          ser_longest = series_all$ser_F,
          test_EF = list(
            t_EF = NULL,
            G_E = G_E,
            G_F = G_F,
            type = "F"
          )))
      } else {
        return(list(
          ser_longest = series_all$ser_E,
          test_EF = list(
            t_EF = NULL,
            G_E = G_E,
            G_F = G_F,
            type = "E"
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
            G_F = G_F,
            type = "EF")),
        class = "IPHC_ser_EF"))  # TODO - add to above ones once happy with,
                                 # though plot function didn't seem to work
  } else {
    return(
      structure(
        list(
          ser_longest = series_all$ser_E,
          test_EF = list(
            t_EF = t_EF,
            G_E = G_E,
            G_F = G_F,
            type = "E")),
        class = "IPHC_ser_EF"))
  }
}


##' Take set count data for a species and add column indicating whether or not
##' it is inside a given area
##'
##' See example and the vignette.
##'
##' @param set_counts_of_sp input tibble of the species of interest, likely generated from
##'   `cache_pbs_data_iphc` (which seems to save a list with the first element
##'   the desired tibble), with column names `year`, `station`, `lat`, `lon`,
##'   `E_it`, `N_it`, `C_it`, `E_it20`, `N_it20`, `C_it20`, `usable`, and
##'   `in_area` if `indicate_in_area` is TRUE
##' @param area `data.frame` of (PBSmapping) class `PolySet` with column names
##'   `PID`, `SID`, `POS`, `X`, `Y`,
##' @return tibble of `set_counts_of_sp` with extra logical column `in_area` to
##'   say whether or not each set is within `area`. Internally a column `EID` is
##'   created to assign each set-year a unique ID, needed for
##'   `PBSmapping::findPolys`, but this is not returned.
##' @export
##' @author Andrew Edwards
##' @examples
##' @donttest{
##' sp_set_counts_with_area <- add_in_area(yelloweye_rockfish$set_counts,
##'                                        HG_herring_pred_area)
##' # Gives a warning about hole vertices, can ignore.
##' @}
add_in_area <- function(set_counts_of_sp,
                        area = HG_herring_pred_area){

  sp_set_counts_with_area <- set_counts_of_sp %>%
    dplyr::mutate(in_area = FALSE) %>%       # then adjusted below
    dplyr::bind_cols("EID" = 1:nrow(set_counts_of_sp))

  events <- sp_set_counts_with_area %>%
    dplyr::rename(X = lon, Y = lat) %>%
    dplyr::select(year, EID, Y, X)

  locData <- PBSmapping::findPolys(as.data.frame(events),
                                   area)

  EIDs_in_poly <- unique(locData$EID)

  # Give each set an `in_area` logical attribute TRUE/FALSE
  sp_set_counts_with_area$in_area <- sp_set_counts_with_area$EID %in% EIDs_in_poly

  sp_set_counts_with_area <- select(sp_set_counts_with_area,
                                    -c("EID"))

  return(sp_set_counts_with_area)
}



##' Get data, do calculations and plot longest series for the IPHC survey
##'  restricted to a specified area
##'
##' Get data, do calculations and plot longest series for the IPHC survey for
##'  a given species restricted to a given area. Will take a while if queries
##'  GFbio (for which need to be on DFO network), else can use cached data.
##'  If data are for combined species, as cached with `get_combined_species()`,
##'  then the column `N_it_sum` etc. column names are used for `N_it` etc.
##'
##' @param sp Species name (as used in gfdata and gfplot). Or something like
##'   "skates combined" -- see vignette.
##' @param area `data.frame` of (PBSmapping) class `PolySet` with column names
##'   `PID`, `SID`, `POS`, `X`, `Y`,
##' @param cached if TRUE then used cached data (sp-name.rds)
##' @param verbose if TRUE then print out some of the data (useful in vignette loops)
##' @param print_sp_name if TRUE then print out species name (useful in vignette
##'   loops)
##' @return For the given species, list containing
##'
##'   sp_set_counts_with_area: tibble returned from [add_in_area()] of set
##'   counts of the species with with extra logical column `in_area` to
##'   say whether or not each set is within `area`.
##'
##'   ser_E_and_F: list of class `IPHC_ser_E_and_F` containing two tibbles, one
##'   for each series (ser_E and ser_F), as output from [calc_iphc_ser_E_and_F()].
##'
##'   series_longest: list containing `ser_longest`, the longest possible time
##'   series from Series E and F, and other objects, as output form [calc_iphc_ser_EF()].
##' @export
##' @author Andrew Edwards
##' @examples
##' @donttest{
##' iphc_get_calc_plot_area("yelloweye rockfish")
##' ##' # Gives a warning about hole vertices, can ignore.
##' @}
iphc_get_calc_plot_area <- function(sp,
                                    area = HG_herring_pred_area,
                                    cached = TRUE,
                                    verbose = FALSE,
                                    print_sp_name = TRUE
                                    ){
  if(!cached){
    cache_pbs_data_iphc(sp)
  }

  if(print_sp_name){
    print(paste("*****", sp, "*****"))
  }

  sp_set_counts <- readRDS(sp_hyphenate(sp))

  sp_set_counts_with_area <- add_in_area(sp_set_counts$set_counts,
                                         area = area)

  if(!("N_it" %in% names(sp_set_counts_with_area)) &
     "N_it_sum" %in% names(sp_set_counts_with_area)){
    sp_set_counts_with_area <- sp_set_counts_with_area %>%
      dplyr::rename(N_it = N_it_sum,
                    N_it20 = N_it20_sum,
                    C_it = C_it_sum,
                    C_it20 = C_it20_sum)
  }

  ser_E_and_F <- calc_iphc_ser_E_and_F(sp_set_counts_with_area)

  series_longest <- calc_iphc_ser_EF(ser_E_and_F)

  # Not creating gfsynopsis formatted output since shouldn't need that for a
  #  restricted area -- see iphc_get_calc_plot() to add something here.

  if(verbose){
    # Print the first and last values:
    print(sp_set_counts)
    print(tail(sp_set_counts$set_counts))
    print(ser_E_and_F)
    print(series_longest)
    print(paste("*****", sp, "*****"))
  }

  plot_IPHC_ser_four_panels(ser_E_and_F,
                            series_longest,
                            sp = sp)
  list(
    sp_set_counts_with_area = sp_set_counts_with_area,
    ser_E_and_F = ser_E_and_F,
    series_longest = series_longest
  )
}

##' Format results from multiple species for Jennifer's EAFM HG Herring Case
##' Study and save them.
##'
##' Create and save a .csv file in the format requested by Jennifer Bolt:
##'   What would work best for me is a csv file with 3 columns: `Year`,
##'   `Indicator`, `Value`
##' Non-standardized values are best (i.e. not normalised).
##' The `Indicator` column could include:
##' 1. ArrowtoothFlounder_IPHC_SurveyCatchRate
##' 2. ArrowtoothFlounder_IPHC_UpperCI
##' 3. ArrowtoothFlounder_IPHC_LowerCI
##' 4. Rockfish_IPHC_SurveyCatchRate
##' 5. ...
##'
##' Also saves `sp_vec_list` as an .rds file.
##'
##' @param sp_vec A vector of species names. Each one *must* have a corresponding
##'   list `species_name_area` in the workspace which is an output from
##'   `iphc_get_calc_plot_area()` -- see vignette.
##' @param sp_vec_list list of output, with each element corresponding to each
##'   species (and named for that species), and being a tibble of the longest
##'   time series. See vignette, since needs to be created outside of a function.
##' @param filename filename (including .csv) to save .csv file; also saves .rds
##'   of sp_vec_list.
##' @return saves a .csv file and returns the resulting tibble. Columns are:
##'  - Year
##'  - Indicator (e.g. ArrowtoothFlounder_IPHC_SurveyCatchRate,
##'   ArrowtoothFlounder_IPHC_UpperCI)
##'  - Value
##' @export
##' @author Andrew Edwards
##' @examples
##' @donttest{
##' # See vignette.
##' @}
iphc_format_for_EAFM <- function(sp_vec,
                                 sp_vec_list,
                                 filename = "herring-HG-predators-IPHC.csv"){

  saveRDS(sp_vec_list,
          gsub(".csv", ".rds", filename))

  eafm_res <- tibble()        # Long format for EAFM Case Study

  for(sp in sp_vec){
    sp_name_one_word <- gsub(" ", "", simple_cap(sp))
    new_names <- c(paste0(sp_name_one_word,
                          "_IPHC_SurveyCatchRate"),
                   paste0(sp_name_one_word,
                          "_IPHC_LowerCI"),
                   paste0(sp_name_one_word,
                          "_IPHC_UpperCI"))

    this_sp_res <- sp_vec_list[[sp]] %>%
      dplyr::select(Year = year,
                    I_t20BootMean,
                    I_t20BootLow,
                    I_t20BootHigh) %>%
      dplyr::rename_with(.cols = I_t20BootMean,
                         .fn = function(x){new_names[1]}) %>%
      dplyr::rename_with(.cols = I_t20BootLow,
                         .fn = function(x){new_names[2]}) %>%
      dplyr::rename_with(.cols = I_t20BootHigh,
                         .fn = function(x){new_names[3]}) %>%
      tidyr::pivot_longer(!Year,
                          names_to = "Indicator",
                          values_to = "Value")

    eafm_res <- rbind(eafm_res,
                      this_sp_res)
  }

  write.csv(eafm_res,
            filename,
            row.names = FALSE,
            quote = FALSE)

  return(eafm_res)
}
