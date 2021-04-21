# IPHC stations including whether or not they were part of the standard grid or
#  the expanded grid that started in 2018

# Load file from Dana Haggarty, who got the station locations from the IPHC and
# then intersected the stations with the expansion set she got from the IPHC in
# 2018 and the RCAs/MPAs. Locations are geographic coordinates, WGS1983
# datum. Originally used IPHC_Stations_All.csv, but that had 7 stations missing
# that were in GFbio:
#  from_dana <- read.csv("IPHC_Stations_All.csv")$station
#  from_gfbio <- dplyr::filter(get_iphc_sets_info(), year == 2018)$station
# setdiff(from_dana, from_gfbio)  # 2018 stations in Dana's list but not in gfbio:
# [1] "2094" "2102" "2116" "2138" "3001" "3002" "3003" "3004" "3005" "3008"
# [11] "3009" "3010" "3011" "3012" "3013" "3210" "2241" "2248" "2256" "2257"
# setdiff(from_gfbio, from_dana)  # 2018 stations in gfbio but not in Dana's list:
# [1] "2338" "2336" "2337" "2342" "2341" "2339" "2340"

# Dana then redid the query, and suspected that the seven not in original list
#  were not fished since in MPA, RCAs or along the border. Looks like they were
#  along northern border and then eastern SoG (off my usual maps). Using her updated
#  file:
# from_dana_new <- read.csv("IPHC_FISS_All_Stn_GFBio.csv")$station
# setdiff(from_dana_new, from_gfbio) # same 2018 stations as above in Dana's but
#  not gfbio, that we don't need to worry about:
# [1] 2094 2102 2116 2138 3001 3002 3003 3004 3005 3008 3009 3010 3011 3012 3013
# [16] 3210 2241 2248 2256 2257
# setdiff(from_gfbio, from_dana_new)  # 2018 stations in gfbio but not in Dana's
#  new list:
# character(0)
# So now redo this code with the new .csv file and update setDataExpansion data object.

# Old, not using below:
raw_data_old <- read.csv("IPHC_Stations_All.csv") %>%
  dplyr::as_tibble()
# Updated (with slightly changed capitalisation of column headings, and no
# 'comments' column):
raw_data <- read.csv("IPHC_FISS_All_Stn_GFBio.csv") %>%
   dplyr::as_tibble()

# Remove un-needed columns (FID was just added on by Dana's ArcGIS, regarea is
# 2B for all BC waters, stn_type_n is just numeric code for stn_type), set lon
# to be same format as elsewhere in package,
setDataExpansion <- dplyr::select(raw_data,
                                  station,
                                  lat = Lat,
                                  lon = Lon,
                                  type = Stn_type,
                                  rca = RCA,
                                  mpa = MPA,
                                  mpa_name = MPA_name) %>%
  dplyr::mutate(station = as.character(station),
                lon = lon - 360,
                standard = as.factor(ifelse(type == "standard", "Y", "N")),
                rca = as.factor(ifelse(rca == 1, "Y", "N")),
                mpa = as.factor(ifelse(mpa == 1, "Y", "N")),
                mpa_name = as.factor(ifelse(mpa_name == " ", NA, mpa_name))) %>%
  dplyr::select(-c("type"))

usethis::use_data(setDataExpansion, overwrite = TRUE)
