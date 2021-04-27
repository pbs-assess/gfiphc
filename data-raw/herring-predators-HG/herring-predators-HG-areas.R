# herring-predators-HG-areas.R - defining the areas of interest for predators of
#  Haida Gwaii Pacific Herring. These are to generate indices of abundance for
#  the DFO Ecosystem Approach to Fisheries Management Case Study.

# AreaOfInterest.zip - shape files from Jennifer Boldt, as used for hake (so maybe from
#  Stephane originally?), including BCMap.png and AreaOfInterest.*.
#
# AlI-2100420.7z - adapted files from Rowan, including mergePolys.r and
#  AoI.*. Note that he also extracted some manually from the .csv file, so am
#  just importing and saving the relevant file here.

# library(PBSmapping)
load("AoI.rda")

HG_herring_pred_area <- AoI

usethis::use_data(HG_herring_pred_area,
                  overwrite = TRUE)
