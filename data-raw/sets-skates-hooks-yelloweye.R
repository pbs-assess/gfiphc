# Extract and save the set-level data, skate-level data, and hooks with bait
# returned, plus counts for yelloweye rockfish as an example.

# Needs to be run each year once GFBio (or local spreadsheets, which may occur
# if counts are only for the first 20 hooks) are updated.

# Set-level information from GFBio
sets_other_years <- get_iphc_sets_info()
                                       # extracts the set level data from
                                       #  GFBio for 2003 onwards (excluding 2013
                                       #  and any later years that are in spreadsheets)
usethis::use_data(sets_other_years,
                  overwrite = TRUE)


# Skate-level information from GFBio
skates_other_years <- get_iphc_skates_info()
                                       # extracts skate level data from GFBio
                                       #  for 2003 onwards (excluding 2013 and
                                       #  any later years that are in spreadsheets)
usethis::use_data(skates_other_years,
                  overwrite = TRUE)


# Number of hooks with bait returned, from GFBio
cache_pbs_data_iphc("hook with bait")
                                       # extracts counts for the 'species' "hook
                                       #  with bait"; needed if looking into hook
                                       #  competition. Using cache function
                                       #  since the same function is used to
                                       #  obtain data for each species (and in
                                       #  particular is used in gfsynopsis).
hooks_with_bait <- readRDS("hook-with-bait.rds")

usethis::use_data(hooks_with_bait,
                  overwrite = TRUE)


# Extract data on Yelloweye Rockfish from GFBio
cache_pbs_data_iphc("yelloweye rockfish")
                                       # That creates yelloweye-rockfish.rds. [This is
                                       #  what gfsynopsis::get_data_iphc() calls,
                                       #  via gfsynopsis::get_data() in
                                       #  report/make.R of gfsynopsis repo.]
yelloweye_rockfish <- readRDS("yelloweye-rockfish.rds")

usethis::use_data(yelloweye_rockfish,
                  overwrite = TRUE)