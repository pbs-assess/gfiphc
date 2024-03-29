# Extract and save the set-level data, skate-level data, and hooks with bait
# returned, plus counts for yelloweye rockfish as an example. Run this line-by-line.

# Needs to be run each year once GFBio (or local spreadsheets, which may occur
#  if counts are only for the first 20 hooks) are updated. Some of these won't
#  change if GFbio data not changed. use_data() can be re-run and Git won't
#  detect changes if data not changed.

# See README for details of order to update files; for example,
#  iphc-2021-data.Rmd needs to be done before this one.

load_all()

# Before running all this code (but after updating get_all_iphc_set_counts() to
#  include new year), run these two lines (with 2022 replaced by current year)
yyr_test <- get_all_iphc_set_counts("yelloweye rockfish")
tail(yyr_test)                                # confirms latest year gets extracted
testthat::expect_equal(dplyr::filter(yyr_test, year < 2022),
                       yelloweye_rockfish$set_counts)   # this will fail once the saved
                                                        # yelloweye_rockfish is updated below


# Set-level information from GFBio
sets_other_years_update <- get_iphc_sets_info()
                                       # extracts the set level data from
                                       #  GFBio for 2003 onwards (excluding 2013
                                       #  and any later years that are in gfiphc).
                                       # Should not change if new data are in gfiphc:
testthat::expect_equal(sets_other_years,
                       sets_other_years_update)
# That didn't give an error for 2021 or 2022 (as expected), but the first run through
# Git did still think sets_other_years got updated here, but second time (after
# fixing the six new stations issue) it didn't - maybe just something to do with
# usethis. .rda files are not too big so don't worry about it too much.
# 2022 - nothing should have changed but Git still detects a change in sets_other_years.rda
sets_other_years <- sets_other_years_update
usethis::use_data(sets_other_years,
                  overwrite = TRUE)


# Skate-level information from GFBio
skates_other_years_update <- get_iphc_skates_info()
                                       # extracts skate level data from GFBio
                                       #  for 2003 onwards (excluding 2013 and
                                       #  any later years that are in spreadsheets)
testthat::expect_equal(skates_other_years,
                       skates_other_years_update)
# Same as for sets_other_years - data idential but Git thinks updated. Probably
#  a use_data thing.
skates_other_years <- skates_other_years_update
usethis::use_data(skates_other_years,
                  overwrite = TRUE)


# Number of hooks with bait returned, from GFBio and gfiphc saved data
cache_pbs_data_iphc("hook with bait")
                                       # extracts counts for the 'species' "hook
                                       #  with bait"; needed if looking into hook
                                       #  competition. Using cache function
                                       #  since the same function is used to
                                       #  obtain data for each species (and in
                                       #  particular is used in gfsynopsis).
hooks_with_bait <- readRDS("hook-with-bait.rds")   # note hook/hooks to help distinguish

usethis::use_data(hooks_with_bait,
                  overwrite = TRUE)


# Extract data on Yelloweye Rockfish from GFBio and gfiphc daved data
cache_pbs_data_iphc("yelloweye rockfish")
                                       # That creates yelloweye-rockfish.rds. [This is
                                       #  what gfsynopsis::get_data_iphc() calls,
                                       #  via gfsynopsis::get_data() in
                                       #  report/make.R of gfsynopsis repo.]
yelloweye_rockfish <- readRDS("yelloweye-rockfish.rds")

usethis::use_data(yelloweye_rockfish,
                  overwrite = TRUE)

# Re-run the first expect_equal on yelloweye and it should fail.

# Check that plot_BC default lat contains the range of latitudes (mainly not larger than the max latitude ,since I'd
# missed that 2018 had gone further north), and also range of longitudes:
formals(plot_BC)$ylim     # should be c(48, 55.4), not easy to check as not retured as numeric

if(max(yelloweye_rockfish$set_counts$lat) > 55.4){
  stop("Need to increase default max of ylim in plot_BC to account for northern stations. And change it in this check also.")
}
if(min(yelloweye_rockfish$set_counts$lat) < 48){
  stop("Need to decrease default min of ylim in plot_BC to account for southern stations. And change it in this check also.")
}

# Longitude - in 2018 I thought not to show the expanded Strait of Georgia
# stations, but best to show them all.
formals(plot_BC)$xlim     # should be c(-134,-123), not easy to check as not retured as numeric

if(max(yelloweye_rockfish$set_counts$lon) > -123){
  stop("Need to increase default max of xlim in plot_BC to account for eastern stations. And change it in this check also.")
}
if(min(yelloweye_rockfish$set_counts$lon) < -134){
  stop("Need to decrease default min of xlim in plot_BC to account for western stations. And change it in this check also.")
}
