# map-test.R - taking old map code and testing making it into a function
# Get map working for one example, then make into a function and generalise

# Main code from iphcSerBallHooksYYR.Snw, from 17/4/2015

load_all()
require(dplyr)
require(tibble)
# require(xtable)
# require(gplots)                 # for plotCI
# require(boot)
require(PBSmapping)             # for .createIDs

rm(list=ls())

source("../R/iphc-maps.R")

#  by Sebastian Kranz.
figheight = 6
figwidth = 5.7


spCode=442; spName="Yelloweye Rockfish"; shortName = "Yelloweye"; spCodeIphc=89; spAbbr = "No YYR"

latCutOff = 50.6; lon1=-130; lon2=-128.25 # just below 1995 and 1996, so
#  including all of those stations, near norther tip of VI; draw line lon1-2



## % From IPHClocations.Snw, separate file for effskate and usable for 1995:
## <<>>=
## effSkate1995 = read.table("iphcDataPrep/1995EffSktValuesbyStation.csv",
##     header=TRUE, sep=",", strip.white=TRUE)     # Load in effective skate numbers
## effSkate1995 = arrange(effSkate1995, Station)     # order by station
## names(effSkate1995) = c("station", "effSkate", "usable")  # usable is just for 95
## effSkate1995$station = trimWhiteSpace(effSkate1995$station)
## @

## % For original RBR, this was in IPHClocations.Snw, not IPHCjoin6.Snw:
## <<setVals1995>>=
## setVals1995 = summarise(group_by(data1995, year, station), lat=unique(lat),
##     lon=unique(lon), N_it = sum( (specName == spName) * specCount) )
## setVals1995 = arrange(setVals1995, station)    # should be anyway
## # Check with orig data, this should be a vector of 0's:
## bbbbb = sort(setVals1995$N_it[ setVals1995$N_it > 0]) -
##     sort(filter(data1995, specName == spName)$specCount)
## if( max(abs(bbbbb>0)) > 0) stop("check setVals1995")

## # Check stations agree between the two input files:
## if(length( setdiff(setVals1995$station, effSkate1995$station) ) > 0)
##    stop("error in 1995 stations from two different files")

## setVals1995 = merge(setVals1995, effSkate1995, by="station")
## setVals1995[ is.na(setVals1995$N_it), "N_it"] = 0 # manually checked that
##               # these mean no Rockfish caught for that station
## summary(setVals1995)
## filter(setVals1995, usable=="N")   # manually checked on the unusable ones
## @

## Had seen for RBR that one of the unusable ones does have a high {\tt N\_it}, actually the second highest. But not using (IPHC defined them as unusable).

## % For names of 1995 stations:
## %<<names1995, echo=FALSE, results=hide>>=
## %postscript("stations1995names.eps", height = figheight, width = figwidth,
## %           horizontal=FALSE,  paper="special")
## %expandPlot(mfrow=c(1,1),mar=c(2,2,0.5,0.5))
## %plotBC(main="Names of 1995 stations")  #zlev=100)
## %text(lat~lon,  data=setVals1995, labels=setVals1995$station, cex=0.4)
## %dev.off()
## %@
## %\onefig{stations1995names}{Names of 1995 stations.}

## <<keep1995>>=
## setVals1995 = mutate(setVals1995, keep = 1)    # keep means keep, based on
##                                                #  geography for 1995 and1996

## setVals1995[setVals1995$lat < latCutOff, "keep"] = 0    # off Van Island
##                        # To manually exclude any:
## # setVals1995[setVals1995$station %in% c("8P", "8PC", "8Q", "9Q", "12M", "13K",
## #                       "14H", "14HC", "14I", "15F", "15FC", "15H",
## #                       "16F"), "keep"] = 0

## # setVals1995 - 1995 data: one row for each station,
## #  usable=usable that year, keep=above geographic cut off
## #   N_it = number of spName caught, effSkate=effective skate number, lat, lon
## @


# from gfiphc vignette:
sp <- "yelloweye rockfish"
sp_set_counts <- readRDS(paste0(gsub(" ", "-", sp), ".rds"))
                                       # For each set, contains the various
                                       # catch rates for this species, the lat
                                       # and lon, and whether the set
                                       # is usable or not. Includes the basic
                                       # information for each set, but further
# details are available in the
# objects described below
# It's a list with first element sp_set_counts$set_counts

hooks_with_bait <- readRDS("hook-with-bait.rds")
sets_other_years <- readRDS("sets-other-years.rds")
skates_other_years <- readRDS("skates-other-years.rds")


# postscript("stations1995mapA.eps", height = figheight, width = figwidth,
#           horizontal=FALSE,  paper="special")

# expandPlot(mfrow=c(1,1))
plot_iphc_map(sp_set_counts$set_counts, sp_short_name = "Yelloweye Rockfish", years = 1995)

plot_iphc_map(sp_set_counts$set_counts, sp_short_name = "Yelloweye Rockfish", years = 1996)

plot_iphc_map(sp_set_counts$set_counts, sp_short_name = "Yelloweye Rockfish", years = 1997) # won't work yet

plot_iphc_map(sp_set_counts$set_counts, sp_short_name = "Yelloweye Rockfish", years = 2008)
# has an unusable

plot_iphc_map(sp_set_counts$set_counts, sp_short_name = "Yelloweye Rockfish", years = 2003)
