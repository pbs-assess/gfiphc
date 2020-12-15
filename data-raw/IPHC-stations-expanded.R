# IPHC stations including whether or not they were part of the standard grid or
#  the expanded grid that started in 2018

# Load file from Dana Haggarty, who got the station locations from the IPHC and
# then intersected the stations with the expansion set she got from the IPHC in
# 2018 and the RCAs/MPAs. Locations are geographic coordinates, WGS1983 datum.

raw_data <- read.csv("IPHC_Stations_All.csv") %>%
  dplyr::as_tibble()

# Remove un-needed columns (FID was just added on by Dana's ArcGIS, regarea is
# 2B for all BC waters, stn_type_n is just numeric code for stn_type), set lon
# to be same format as elsewhere in package,
setData2018 <- dplyr::select(raw_data,
                             station,
                             lat,
                             lon,
                             type = stn_type,
                             rca,
                             mpa,
                             mpa_name) %>%
  dplyr::mutate(year = 2018,
                lon = lon - 360,
                rca = ifelse(rca == 1, "Y", "N"),
                mpa = ifelse(mpa == 1, "Y", "N"),
                mpa_name = ifelse(mpa_name == " ", NA, mpa_name)) %>%
  dplyr::relocate(year)

usethis::use_data(setData2018, overwrite = TRUE)
