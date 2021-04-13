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
plot_iphc_map(sp_set_counts$set_counts, sp = "Yelloweye Rockfish", years = 1995)

plot_iphc_map(sp_set_counts$set_counts, sp = "Yelloweye Rockfish", years = 1996)

plot_iphc_map(sp_set_counts$set_counts, sp = "Yelloweye Rockfish", years = 1997) # won't work yet

plot_iphc_map(sp_set_counts$set_counts, sp = "Yelloweye Rockfish", years = 2008)
# has an unusable

plot_iphc_map(sp_set_counts$set_counts, sp = "Yelloweye Rockfish", years = 2003)

# \onefig{stations1995mapA}{Locations of the \Sexpr{dim(setVals1995)[1]} stations in 1995, of which \Sexpr{dim(filter(setVals1995, usable=="Y", N_it == 0))[1]} did not catch \Sexpr{spName} (red open circles), \Sexpr{dim(filter(setVals1995, usable=="Y", N_it > 0))[1]} stations did catch it (red closed circles), and \Sexpr{sum(setVals1995$usable == "N")} were deemed unusable by the IPHC (grey closed circles) and so are not considered further. The black line indicates the geographic cut-off, below which \Sexpr{sum(setVals1995$keep == 0)} stations (black crosses) are thus excluded.}


OLD:

\section{Load in 1996 data, plot map and get desired {\tt setVals1996keep}}

Now work out the catch rates, {\tt C\_it = N\_it / E\_it}, and eliminate un-needed columns:
<<echo=TRUE, results=verbatim>>=
load("iphcDataPrep/iphc9602.RData")     # Loads in data1996to2002
                                        #  (set-by-set, all hooks in 1996,
                                        #  first 20 for 1997+, all sp)
# This gets set-by-set data for the required species
# To find spCodeIPHC do:
# range(filter(data1996to2002, spNameIPHC == "Yelloweye Rockfish")$spCodeIPHC)
# spp = data1996to2002$spNameIPHC   # to list all:
# sort(unique(spp))
setVals1996to2002 = summarise(group_by(data1996to2002, year, station),
    # lon=round(unique(lonMean), digits=2), lat=round(unique(latMean), digits=2),
    lon = unique(lon), lat = unique(lat), depthAvge = unique(depthAvge),
    N_it = sum((spCodeIPHC == spCodeIphc) * catchCount),
    E_it = unique(E_it), skates = unique(skates), usable = unique(usable),
    hooksObserved = unique(hooksObserved))
    # hooksRetrieved = unique(hooksRetrieved),
    # skatesHauled = unique(skatesHauled),
setVals1996 = filter(setVals1996to2002, year < 1996.5)
setVals1996 = mutate(setVals1996, C_it = N_it / E_it)
setVals1996[,"year"] = as.integer(setVals1996$year) # so makes integer in xtables
setVals1996 = tbl_df(setVals1996)
@


<<keep1996>>=
setVals1996 = mutate(setVals1996, keep = 1)    # keep means keep, based on
                                               #  geography
setVals1996[setVals1996$lat < latCutOff, "keep"] = 0    # off Van Island
@

<<echo=FALSE, results=hide>>=
postscript("stations1996mapA.eps", height = figheight, width = figwidth,
           horizontal=FALSE,  paper="special")
expandPlot(mfrow=c(1,1))
plotBC(main="All 1996 stations")  #zlev=100)
points(lat~lon, data=filter(setVals1996, usable=="Y", N_it == 0),
       col="red", cex=1) # , cex=0.6
points(lat~lon, data=filter(setVals1996, usable=="Y", N_it > 0),
       col="red", pch=20, cex=1.2) # , cex=0.6
points(lat~lon,
      data=filter(setVals1996, usable == "N"),
      col="grey", pch=20, cex=1.2)
points(lat~lon,
      data=filter(setVals1996, keep == 0),
      col="black", pch=4, cex=1.2)
lines(c(lon1, lon2), rep(latCutOff, 2))
legend("bottomleft", legend=c(paste("Did not catch", shortName),
      paste("Did catch", shortName), "Unusable station", "Omitting from index"),
       pch=c(1, 20, 20, 4), pt.cex=c(1, 1.2, 1.2, 1.2),
       col=c("red", "red", "grey", "black"))
dev.off()
@

\onefig{stations1996mapA}{Locations of the \Sexpr{dim(setVals1996)[1]} stations in 1996, of which \Sexpr{dim(filter(setVals1996, usable=="Y", N_it == 0))[1]} did not catch \Sexpr{spName} (red open circles), \Sexpr{dim(filter(setVals1996, usable=="Y", N_it > 0))[1]} stations did catch it (red closed circles), and \Sexpr{sum(setVals1996$usable == "N")} were deemed unusable by the IPHC (grey closed circles) and so are not considered further. The black line indicates the geographic cut-off, below which \Sexpr{sum(setVals1996$keep == 0)} stations (black crosses) are thus excluded.}

Don't need the lats and lons in the final {\tt setValsKeep} for the analysis. However, do have to filter and only include the usable and keep stations.
<<setVals1996keep>>=
setVals1996keep = filter(setVals1996, usable=="Y", keep == 1)
setVals1996keep = select(setVals1996keep, year, block = station,
    E_it, N_it, C_it)
@

\section{Load in 2003-2014 data and get desired {\tt setVals0314keep}}

Basing this first on {\tt IPHChookAnalysis.Snw} (up to the bootstrapping) and then {\tt IPHCjoin6.Snw}.

% COPYING FROM IPHChookAnalysis.Snw from start of that file up to 'Simple
%  Bootstrapped Confidence Intervals' in that file. Think all bootstrapping
%  will be based on what was in IPHCjoin6.Snw.

@
Analysing data that has been tidied up in {\tt iphc0314.Snw}. Don't need the set-based data yet, though that was useful in that file to spot skates that had no catch.

\subsection{Load in data and simplify it}

<<loadData, echo=TRUE, results=verbatim>>=
load("iphcDataPrep/iphc0314.RData")   # Loads in data - hook-by-hook
                                      #  and blockLocs0314 - locations of blocks,
                                      #  not by year, all with keep=1

dataOrig = data
rm(data)                       # Going to define it later based on geography
                               # Just have dataOrig, as loaded in. Then remove
                               #  un-needed columns (hookID and skateID aren't
                               #  quite right anyway for manually modified rows):
dataOrig = select(dataOrig, -c(tripID, hookID, skateID, set,
    OLDobsHooksPerSkate, direction))
summary(dataOrig)
# glimpse(dataOrig)
data = dataOrig
data
@

So {\tt data} is a \Sexpr{dim(data)[1]}$\times$\Sexpr{dim(data)[2]} (local) dataOrig frame. Each row is a unique hook, with some that have no catch (species 999) to give a {\tt hooksPerSkate} for that skate. Has already been arranged in order of year and block. See {\tt iphc0314.Snw} for further details.

\subsection{Check that bait field only changes for 2012}

<<bait, echo=TRUE, results=verbatim>>=
data %>% group_by(year, bait) %>% tally()
@
That's good, only 110 (chum salmon) used until 2011, then that and the other two in 2012. Use just the 110 values later.


\section{Roll up to skate then set level, with $N_{it}$, 2003-2014}

Get unique set data from this hooks data set. First need to look at the skate level, and then sum the {\tt hookPerSkate}'s for each set. Also look at bait because that can be summarised at skate level. And add up the counts of a particular species for each skate. See Section \ref{sec:notation} for notation, some of which gets used here. [Some of this could maybe have been done in {\tt iphc0314.Snw}, but think I may have kept variable names the same in 20-hook analysis, so keep this all here to avoid any potential confusion.]
<<totalHooks>>=
# skateSumm = select(data, year, block, skate, hooksPerSkate,
#    obsHooksPerSkate, effSkate, bait)
# skateUniqHooks = unique(skateSumm)         # Picks unique rows
# Now do with group_by, since then can automatically do the count for the species
skateUniqHooks = summarise(group_by(data, year, block, skate),
   hooksPerSkate = unique(hooksPerSkate),
   obsHooksPerSkate = unique(obsHooksPerSkate),
   bait = unique(bait), effSkate = unique(effSkate),
   fishPerSkate = sum( (species == spCode) * catchCount))

if( dim(unique(select(data, year, block, skate)))[1] != dim(skateUniqHooks)[1])
                                     { stop("check skateUniqHooks")}
skateUniqHooks = mutate(skateUniqHooks,
    chumObsHooksPerSkate =  obsHooksPerSkate * (bait == 110),
    chumFishPerSkate =  fishPerSkate * (bait == 110))
                             # Add on field for no. hooks with chum, and how
                             #  how many fish of spCode those hooks caught
                             # (Just 2012 for now, will maybe be 2014+ - no)
data.frame(head(filter(skateUniqHooks, year == 2012, block == 2002)))
if( diff(range(select(filter(skateUniqHooks, year != 2012), obsHooksPerSkate)
      - select(filter(skateUniqHooks, year != 2012), chumObsHooksPerSkate) ))!=0)
                              {stop("check skateUniqHooks outside year 2012")}
setUniqHooks = summarise( group_by(skateUniqHooks, year, block),
    H_itOut = sum(hooksPerSkate), H_itObs = sum(obsHooksPerSkate), K_itAll = n(),
    K_itChum = sum(bait == 110), effSkate = unique(effSkate),
    H_itObsChum = sum(chumObsHooksPerSkate), N_itAll = sum(fishPerSkate),
    N_itChum = sum(chumFishPerSkate))
    # Unique sets, from hooks data, H_itOut is as I originally had and is the
    #  hooks that go out (which I need for the numbering for first 20),H_itObs is
    #  then just for the observed hooks, using new values. Will give error if
    #  unique(effSkate) not a single value. H_itObsChum is observed hooks
    #  that had chum on. K_itAll is number of skates (all bait), K_itChum is
    #  number of skates with chum. N_itAll is number of spCode fish in set
    #  using all skates, N_itChum is just based on chum.
if( dim(unique(select(data, year, block)))[1] != dim(setUniqHooks)[1])
                                     { stop("check setUniqHooks")}
# Add column to make effSkateChum only be based on Chum bait:
setUniqHooks = mutate(setUniqHooks, effSkateChum = effSkate *
    H_itObsChum / H_itObs)
setUniqHooks = arrange(setUniqHooks, year, block) # in case didn't stay in order
# Early effSkateChum won't change:
# data.frame(head(setUniqHooks))
# 2012 effSkateChum now relate to number of observed chum-bait hooks (next section)
data.frame(head(filter(setUniqHooks, year == 2012)))
# For 2012, all K_itChum values seem to be 4:
summary((filter(setUniqHooks, year == 2012))$K_itChum)
@

% Can see above that for 2012, block 2002, the three Redbanded were caught on non-Chum skates.

\subsection{Check that {\tt effSkateChum} is somewhat consistent with number of observed chum-bait hooks}

Estimate the effective skate number based on just the number of observed chum-bait hooks, then look at examples that differ from given values by $>1$ or $>0.5$.
<<effSkateChum>>=
setUniqHooksSamp = mutate(setUniqHooks, estEffSkateChum = H_itObsChum/100)
as.data.frame(filter(setUniqHooksSamp, abs(estEffSkateChum - effSkateChum) > 1))
as.data.frame(filter(setUniqHooksSamp, abs(estEffSkateChum - effSkateChum) >0.5))
@
Only a few examples (others should have been sorted out in manual tidying up before), I'd thought there might be more. None look too bad, block 2101 suggests a typo, but leave for now as not hugely in error.

And for 2012, it shouldn't be much more than 4:
<<>>=
summary(filter(setUniqHooks, year == 2012)$effSkateChum)
@
Looks okay.

\subsection{So we have {\tt setUniqHooks} okay, with counts for \Sexpr{spName}; now define notation}\label{sec:notation}

<<>>=
setUniqHooks
@

\subsection{Create index value for each year}

Simplify to give a set-level dataframe with the correct terminology, containing just what is needed now:
<<setVals0314>>=
setVals0314 = select(setUniqHooks, year=year, block=block, # H_it = H_itObsChum,
    E_it = effSkateChum, N_it = N_itChum)
setVals0314 = mutate(setVals0314, C_it = N_it / E_it)
setVals0314
@

\subsubsection{Filter locations geography, to determine {\tt setVals0314keep}}
         % Need all the setVals0314 for the maps

<<filtering>>=
blockLocs0314[blockLocs0314$lat < latCutOff, "keep"] = 0    # don't keep some

setVals0314keep= filter(setVals0314, block %in%
    filter(blockLocs0314, keep == 1)$block)  # Keep only blocks in geog. region
# data = filter(dataOrig, block %in%
#    filter(blockLocs0314, keep == 1)$block)  # Keep only blocks in geog. region
@


<<yearVals>>=
yearVals0314keep = summarise(group_by(setVals0314keep, year), n_t = n(),
    setsWithSp = sum(N_it != 0), sum.C_it = sum(C_it))
@
% For Redbanded:
% So after doing everything at the hook-by-hook level, making corrections, accounting for bait, etc., compared to the same calculations at the set-by-set level (a few weeks ago):

% {\tt setsWithSp} is the same, except 69 and 63 now was 70 and 71 for 2011 and 2012.

% {\tt sum.C\_it} is exactly the same for 2003, 2005, 2007-2010, $<1$ different for 2004, 2006, and for 2011 was 162.44 (just $>1$ less and for 2012 was 141.20, so 20 less. The final value is clearly important to have correct -- set-level calculation didn't account for the bait experiment, and couldn't do because the data was already rolled up at the set level.

<<index>>=
yearVals0314keep = mutate(yearVals0314keep, I_t = sum.C_it / n_t)
yearVals0314keep
@


For RBR: So the lowest year is 2007, whereas original calculation from the set-level data had 2012 as lowest. Note that can't use the {\tt setsWithSp} values here for 2012 to compare to other years because less skates with Chum so less to choose from -- would have to calculate something like that at the skate level, to look at proportion of empty skates.

% END of what was from IPHChookAnalysis.Snw ****


\section{Now combine for sets keeping for 1995, 1996 and 2003+}
<<>>=
setValsKeep = rbind(setVals1995keep, setVals1996keep, setVals0314keep)
@

% This was from IPHCjoin6.Snw.   Want it for the bootstrapping. And I think for the Keep for 03-12

\section{How many blocks never catch a \Sexpr{spName} for 2003 onwards?}

% Modifying from the 20-hook version, which presumably came from earlier full hook version. Add ..keep to things here.

Now to investigate blocks that never catch a \Sexpr{spName} for the stations being considered. Do for 2003$+$ ({\tt zeroBlocks0314}) etc, and easy to just filter on the 1995 and 1996 data. [At one point may have had {\tt zeroBlocks} containing all, but that doesn't make sense since names aren't consistent between years, so maybe things just got moved around].
% , but be aware that block names are different in 1995, 1996 and for 2003$+$. Check that none overlap:% Should do as map, not doing figure as gets messed up with funny block names for 1995 (and presumably 1996) data.

<<zeroBlocks0314>>=
# yyy1 = intersect(filter(setValsKeep, year == 1995)$block,
#    filter(setValsKeep, year == 1996)$block)
#yyy2 = intersect(filter(setValsKeep, year == 1995)$block,
#    filter(setValsKeep, year == 2003)$block)
# yyy3 = intersect(filter(setValsKeep, year == 1996)$block,
#    filter(setValsKeep, year == 2003)$block) # blocks are same for 2003 onwards
# if(length(yyy1) + length(yyy2) + length(yyy3) > 0) stop("check block names")
# blockValsKeep = summarise(group_by(setValsKeep, block), totalN_it = sum(N_it))
# postscript("blockValsKeep.eps", height = figheight, width = figwidth,
#           horizontal=FALSE,  paper="special")
# plot(filter(blockValsKeep, totalN_it > 0), col="blue", xlab="Block",
#     ylab="Numbers caught in each block from 2003 onwards")
# points(filter(blockValsKeep, totalN_it == 0), col="red", pch=20)
# dev.off()
blockVals0314 = summarise(group_by(setVals0314, block), totalN_it = sum(N_it))
zeroBlocks0314 = filter(blockVals0314, totalN_it == 0)$block # Blocks always 0
nonZeroBlocks0314 = filter(blockVals0314, totalN_it > 0)$block  # Blocks with catch
alwaysZero0314 = length(zeroBlocks0314)

# Blocks we're keeping (think I calc using the above stuff for the figs anyway)
blockVals0314keep = summarise(group_by(setVals0314keep, block),
    totalN_it = sum(N_it))
zeroBlocks0314keep = filter(blockVals0314keep, totalN_it == 0)$block
                                        # Blocks always 0
nonZeroBlocks0314keep = filter(blockVals0314keep, totalN_it > 0)$block
                                        # Blocks with catch
alwaysZero0314keep = length(zeroBlocks0314keep)
@

\section{BCA bootstrap confidence intervals for all years}

First, get non-bootstrapped summary:
<<>>=
# Non-bootstrapped values at the year level:
yearValsKeep = summarise(group_by(setValsKeep, year), n_t = n(),
     setsWithSp = sum(N_it != 0), sum.C_it = sum(C_it))
yearValsKeep = mutate(yearValsKeep, I_t = sum.C_it / n_t, propWithoutSp =
    (n_t - setsWithSp) / n_t )
yearValsKeep
@

Now for the bootstrapping results.
<<booting>>=
boolKeep = list()        # list of boostrap outputs
boolCIkeep = list()
confLow = 0.025        # Confidence levels
confHigh = 0.975
num.boots=10000        # number of bootstrap replicates

meanFun = function(x, I) mean(x[I])
bcaConfKeep = mutate(yearValsKeep, I_tBootMean = NA, I_tBootLow = NA,
    I_tBootHigh = NA, I_tBootCV = NA)
if(confLow != 0.025) { stop("see boolKeep calcs -have not included an option
     for changing the confidence level, as I think you only include 0.95 or 0.90
     etc. and these might not strictly be 2.5-97.5 intervals, as I'm not exactly
     sure how they're calculated") }
years = as.integer(yearValsKeep$year)      # so integers in xtables
for(i in 1:length(years))
  {
  ddKeep = filter(setValsKeep,year == years[i])$C_it
  boolKeep[[i]] = boot(ddKeep, meanFun, R = num.boots)   # list of boot results
  bcaConfKeep[bcaConfKeep$year==years[i], "I_tBootMean"] =
      mean(boolKeep[[i]]$t)
  boolCIkeep[[i]] = boot.ci(boolKeep[[i]], type="bca")
  bcaConfKeep[bcaConfKeep$year==years[i], "I_tBootLow"] =
      boolCIkeep[[i]]$bca[4]
  bcaConfKeep[bcaConfKeep$year==years[i], "I_tBootHigh"] =
      boolCIkeep[[i]]$bca[5]
  bcaConfKeep[bcaConfKeep$year==years[i], "I_tBootCV"] =
      sd(boolKeep[[i]]$t) /
      bcaConfKeep[bcaConfKeep$year==years[i], "I_tBootMean"]
  }
@

Moving, rather than copying, table and figure to the proper write up as may want to adapt it all there.


\clearpage

\section{Summary results, plus use some earlier maps prob. See final RBR probably for text..............doing this to line up with {\tt IPHCJOIN6.Snw} for comparing results....need big header............................................}

Might want Figure \ref{fig:all2003mapA} referenced instead..... check IPHCjoin7, that had something about the extra map. See final RBR write up for text.

For the 2003$+$ series, Figure \ref{fig:all2003map} shows the locations of the \Sexpr{dim(blockLocs0314)[1] - length(nonZeroBlocks0314)} stations that never caught \Sexpr{spName} in any year, and the \Sexpr{length(nonZeroBlocks0314)} stations that caught it at least once.   **That's for all, not just the ones we're keeping.

<<echo=FALSE, results=hide>>=
postscript("all2003mapA.eps", height = figheight, width = figwidth,
           horizontal=FALSE,  paper="special")
#expandPlot(mfrow=c(1,1),mar=c(2,2,0.5,0.5))
expandPlot(mfrow=c(1,1),mar=c(1.8,2,1.0,0.5))
plotBC(main = "All 2003+ stations")  #zlev=100)
points(lat~lon, data=blockLocs0314, col="blue", cex=1) # , cex=0.6
points(lat~lon,
      data=filter(blockLocs0314, block %in% nonZeroBlocks0314),
      col="blue", pch=20, cex=1.2)
legend("bottomleft", legend=c(paste("Never caught", shortName),
       paste("Have caught", shortName)),
       pch=c(1, 20), pt.cex=c(1, 1.2), col=c("blue", "blue"))
dev.off()
@

\onefig{all2003mapA}{Locations of all \Sexpr{dim(blockLocs0314)[1]} stations for the IPHC survey for 2003 onwards. There are \Sexpr{dim(blockLocs0314)[1] - length(nonZeroBlocks0314)} stations that never caught \Sexpr{spName} (blue open circles), and \Sexpr{length(nonZeroBlocks0314)} stations that did catch it at least once.}

% Do still want just 2003 onwards with the cutoff, so copying the above and
%   editing based on iphcSerA20hooksYYR.Snw:

<<echo=FALSE, results=hide>>=
postscript("all2003mapAkeep.eps", height = figheight, width = figwidth,
           horizontal=FALSE,  paper="special")
#expandPlot(mfrow=c(1,1),mar=c(2,2,0.5,0.5))
expandPlot(mfrow=c(1,1),mar=c(1.8,2,1.0,0.5))
plotBC(main = "All 2003-2012 and 2014 stations")  #zlev=100)
points(lat~lon, data=blockLocs0314, col="blue", cex=1) # , cex=0.6
points(lat~lon,
      data=filter(blockLocs0314, block %in% nonZeroBlocks0314),
      col="blue", pch=20, cex=1.2)
points(lat~lon,
      data=filter(blockLocs0314, keep == 0),
      col="black", pch=4, cex=1.2)
legend("bottomleft",
      legend=c(paste("Never caught ", shortName),
                paste("Have caught", shortName),
                "Omitting from index"),
       pch=c(1, 20, 4), pt.cex=c(1, 1.2, 1.2), col=c("blue", "blue", "black"))
dev.off()
@

\onefig{all2003mapAkeep}{Locations of all \Sexpr{dim(blockLocs0314)[1]} stations for the IPHC survey for 2003 onwards. There are \Sexpr{dim(blockLocs0314)[1] - length(nonZeroBlocks0314)} stations that never caught \Sexpr{spName} (blue open circles), and \Sexpr{length(nonZeroBlocks0314)} stations that did catch it at least once. Black crosses indicate the \Sexpr{sum(blockLocs0314$keep == 0)} stations being excluded from the analyses.}   % $




<<stations9596Map, echo=FALSE, results=hide>>=
col96 = "darkgreen"
postscript("stations9596.eps", height = figheight, width = figwidth,
           horizontal=FALSE,  paper="special")
expandPlot(mfrow=c(1,1))
plotBC(main="All 1995 and 1995 stations")  #zlev=100)
points(lat~lon, data=filter(setVals1995, usable == "Y"),
       col="red", cex=1) # , cex=0.6
points(lat~lon, data=filter(setVals1996, usable == "Y"),
       col=col96, pch=20, cex=0.8) # , cex=0.6
      # Note order is different to how they're plotted:
legend("bottomleft", legend=c("Usable 1995 stations",
      "Usable 1996 stations"),
       pch=c(1, 20), pt.cex=c(1, 0.8),
       col=c("red", col96))
dev.off()
@

\onefig{stations9596}{Locations of all \Sexpr{dim(filter(setVals1995, usable=="Y"))[1]} usable stations in 1995 (red open circles) and all \Sexpr{dim(filter(setVals1996, usable=="Y"))[1]} usable stations in 1996 (closed \Sexpr{col96} circles).}


<<stations959603Map, echo=FALSE, results=hide>>=
postscript("stations959603.eps", height = figheight, width = figwidth,
           horizontal=FALSE,  paper="special")
expandPlot(mfrow=c(1,1))
plotBC(main="All 1995, 1996 and 2003+ stations")  #zlev=100)
# text(lat~lon,  data=blockLocs0314, labels=blockLocs0314$block, cex=0.3)
points(lat~lon,
      data=blockLocs0314,
      col="blue", pch=20, cex=1)
points(lat~lon, data=filter(setVals1995, usable == "Y"),
       col="red", cex=1) # , cex=0.6
points(lat~lon, data=filter(setVals1996, usable == "Y"),
       col=col96, pch=20, cex=0.8) # , cex=0.6
      # Note order is different to how they're plotted:
legend("bottomleft", legend=c("Usable 1995 stations", "Usable 1996 stations",
      "All 2003+ stations"),
       pch=c(1, 20, 20), pt.cex=c(1, 0.8, 1),
       col=c("red", col96, "blue"))
dev.off()
@

\onefig{stations959603}{Locations of all \Sexpr{dim(filter(setVals1995, usable=="Y"))[1]} usable stations in 1995 (red open circles), all \Sexpr{dim(filter(setVals1996, usable=="Y"))[1]} usable stations in 1996 (\Sexpr{col96} closed circles) and all \Sexpr{dim(blockLocs0314)[1]} stations for 2003 onwards (closed blue circles).}

% See IPHCjoin6.Snw for names of 1996 stations (especially if want to eliminate
%  any specific ones - doubt we will though.

<<stationsKeepMap, echo=FALSE, results=hide>>=
postscript("stations959603Keep2.eps", height = figheight, width = figwidth,
           horizontal=FALSE,  paper="special")
expandPlot(mfrow=c(1,1))
plotBC(main="All 1995, 1996 and 2003+ stations")  #zlev=100)
# text(lat~lon,  data=blockLocs0314, labels=blockLocs0314$block, cex=0.3)
points(lat~lon,
      data=blockLocs0314,
      col="blue", pch=20, cex=1)
points(lat~lon, data=filter(setVals1995, usable == "Y"),
       col="red", cex=1) # , cex=0.6
points(lat~lon, data=filter(setVals1996, usable == "Y"),
       col=col96, pch=20, cex=0.8) # , cex=0.6
points(lat~lon,
      data=filter(blockLocs0314, keep == 0),
      col="black", pch=4, cex=1.2)
points(lat~lon,
      data=filter(setVals1995, keep == 0),
      col="black", pch=4, cex=1.2)
points(lat~lon,
      data=filter(setVals1996, keep == 0),
      col="black", pch=4, cex=1.2)
      # Note order is different to how they're plotted:
legend("bottomleft", legend=c("Usable 1995 stations", "Usable 1996 stations",
      "All 2003+ stations", "Omitting from index"),
       pch=c(1, 20, 20, 4), pt.cex=c(1, 0.8, 1.0, 1.2),
       col=c("red", col96, "blue", "black"))
dev.off()
@

% Omit to include in summary and not mess up numbering
% \onefig{stations959603Keep2}{As for Figure \ref{fig:stations959603}, but with stations being omitted from the index series indicated by black crosses, namely \Sexpr{sum(setVals1995$keep == 0)} stations for 1995, \Sexpr{sum(setVals1996$keep == 0)} stations for 1996 and \Sexpr{sum(blockLocs0314$keep == 0)} for 2003$+$.}



<<stationsNotKeep, echo=FALSE, results=hide>>=
postscript("stationsNotKeep959603.eps", height = figheight, width = figwidth,
           horizontal=FALSE,  paper="special")
expandPlot(mfrow=c(1,1))
plotBC(main="Stations being excluded")  #zlev=100)
points(lat~lon, data=filter(setVals1995, usable=="Y", keep == 0, N_it == 0),
       col="red", cex=1) # , cex=0.6
points(lat~lon, data=filter(setVals1995, usable=="Y", keep == 0, N_it > 0),
       col="red", pch=20, cex=1.2) # , cex=0.6
points(lat~lon, data=filter(setVals1996, usable=="Y", keep == 0, N_it == 0),
       col=col96, cex=0.8) # , cex=0.6
points(lat~lon, data=filter(setVals1996, usable=="Y", keep == 0, N_it > 0),
       col=col96, pch=20, cex=0.8) # , cex=0.6
points(lat~lon, data=filter(blockLocs0314, keep == 0, !(block %in% nonZeroBlocks0314)),
       col="blue", cex=1) # , cex=0.6
points(lat~lon,
      data=filter(blockLocs0314, keep == 0, block %in% nonZeroBlocks0314),
      col="blue", pch=20, cex=1.2)
legend("bottomleft", legend=c(paste("Did not catch", shortName, "1995"),                         paste("Did catch", shortName, "1995"),
                paste("Did not catch", shortName, "1996"),
                paste("Did catch", shortName, "1996"),
                paste("Did not catch", shortName, "2003+"),
                paste("Did catch", shortName, "2003+")),
       pch=c(1, 20, 1, 20, 1, 20), pt.cex=c(1, 1.2, 0.8, 0.8, 1, 1.2),
       col=c("red", "red", col96, col96, "blue", "blue"))
dev.off()
@

\onefig{stationsNotKeep959603}{Locations of the stations which are being discarded, and whether or not they caught \Sexpr{spName}. Of these stations, for 1995 \Sexpr{dim(filter(setVals1995, usable=="Y", keep == 0, N_it == 0))[1]} did not catch \Sexpr{spName} and \Sexpr{dim(filter(setVals1995, usable=="Y", keep == 0, N_it > 0))[1]} did, for 1996 \Sexpr{dim(filter(setVals1996, usable=="Y", keep == 0, N_it == 0))[1]} did not catch \Sexpr{spName} and \Sexpr{dim(filter(setVals1996, usable=="Y", keep == 0, N_it > 0))[1]} did, and for the 2003$+$ series, \Sexpr{dim(filter(blockLocs0314, keep == 0, !(block %in% nonZeroBlocks0314)))[1]} never caught it while \Sexpr{dim(filter(blockLocs0314, keep == 0, block %in% nonZeroBlocks0314))[1]} did.}



<<stationsKeeping, echo=FALSE, results=hide>>=
postscript("stationsKeeping959603.eps", height = figheight, width = figwidth,
           horizontal=FALSE,  paper="special")
expandPlot(mfrow=c(1,1))
plotBC(main="Stations being kept")  #zlev=100)
points(lat~lon, data=filter(setVals1995, usable=="Y", keep == 1, N_it == 0),
       col="red", cex=1) # , cex=0.6
points(lat~lon, data=filter(setVals1995, usable=="Y", keep == 1, N_it > 0),
       col="red", pch=20, cex=1.2) # , cex=0.6
points(lat~lon, data=filter(setVals1996, usable=="Y", keep == 1, N_it == 0),
       col=col96, cex=0.8) # , cex=0.6
points(lat~lon, data=filter(setVals1996, usable=="Y", keep == 1, N_it > 0),
       col=col96, pch=20, cex=0.8) # , cex=0.6
points(lat~lon, data=filter(blockLocs0314, keep == 1, !(block %in% nonZeroBlocks0314)),
       col="blue", cex=1) # , cex=0.6
points(lat~lon,
      data=filter(blockLocs0314, keep == 1, block %in% nonZeroBlocks0314),
      col="blue", pch=20, cex=1.2)
legend("bottomleft",
       legend=c(paste("Did not catch", shortName, "1995"),
                paste("Did catch", shortName, "1995"),
                paste("Did not catch", shortName, "1996"),
                paste("Did catch", shortName, "1996"),
                paste("Did not catch", shortName, "2003+"),
                paste("Did catch", shortName, "2003+")),
       pch=c(1, 20, 1, 20, 1, 20), pt.cex=c(1, 1.2, 0.8, 0.8, 1, 1.2),
       col=c("red", "red", col96, col96, "blue", "blue"))
dev.off()
@

\onefig{stationsKeeping959603}{Locations of the stations which are being kept to use for the index, and whether or not they caught \Sexpr{spName}. Of these stations, in 1995 \Sexpr{dim(filter(setVals1995, usable=="Y", keep == 1, N_it == 0))[1]} did not catch \Sexpr{spName} and \Sexpr{dim(filter(setVals1995, usable=="Y", keep == 1, N_it > 0))[1]} did, in 1996 \Sexpr{dim(filter(setVals1996, usable=="Y", keep == 1, N_it == 0))[1]} did not catch \Sexpr{spName} and \Sexpr{dim(filter(setVals1996, usable=="Y", keep == 1, N_it > 0))[1]} did, and for the 2003$+$ series, \Sexpr{dim(filter(blockLocs0314, keep == 1, block %in% nonZeroBlocks0314))[1]} never caught it while \Sexpr{dim(filter(blockLocs0314, keep == 1, !(block %in% nonZeroBlocks0314)))[1] } did.}




\section{Notation}\label{sec:notationIPHC}

% Notation based on {\tt IPHChookAnalysis20.Snw}, which did change slightly from  {\tt IPHCsetTidy.Snw} and {\tt IPHChookTidy.Snw} because then I hadn't distinguish all hooks sent out, observed hooks and chum-bait hooks. Chum-bait calculations on the data are done later.

The index series from the IPHC surveys consists of the mean catch rate for each year. The mean for a year is the mean of the catch rates of all sets within that year. The catch rate of a set has units of number of \spName~caught per effective skate. The catch rates within a year are bootstrapped, to give bootstrapped means, bias-corrected and adjusted (BCa) bootstrapped 95\% confidence intervals, and bootstrapped coefficients of variation (CV). The bootstrapped means and CVs are used as input for the statistical catch-at-age model.

The effective skate number provided by the IPHC is for all skates used, which in 2012 will include skates that were not baited with Chum Salmon (Eric Soderlund, IPHC, pers.~comm.). But we wish to only include the Chum Salmon baited skates, and so we first modify the effective skate number. The effective skate number depends on the number of observed hooks (Eric Soderlund, IPHC, pers.~comm.), rather than the number of hooks that were deployed.

Define:

$H_{it}$ -- number of observed chum-bait hooks in set $i$ in year $t$, % {\tt H\_itObsChum}$=${\tt H\_it} later,

$H_{it}^*$ -- number of observed hooks for all bait types (equals $H_{it}$ for $t < 2012$) % {\tt H\_itObs},

$E_{it}'$ -- effective skate number of set $i$ in year $t$ from IPHC, which is based on all observed hooks (regardless of bait), % {\tt effSkate}

$E_{it}$ -- effective skate number of set $i$ in year $t$ based only on observed chum-bait hooks, % so is {\tt effSkateChum}$=${\tt E\_it} later.

The effective skate number scales linearly with the number of hooks \citep{yocld08}, and so the desired $E_{it}$ is
\eb
E_{it} = \dfrac{H_{it}}{H_{it}^*} E_{it}'.
\ee

The desired index for the species of interest, in this case \spName, is:

$I_{t}$ -- count index for year $t$ (with units of numbers of fish per effective skate).

Further define:

$n_t$ -- the number of usable sets in year $t$,

$N_{it}$ -- the number of \spName~caught on set $i=1,2,...,n_t$ in year $t$, based on observed chum-bait hooks, % {\tt N\_itChum}$=${\tt N\_it}.

$C_{it}$ -- catch rate (with units of numbers of fish per effective skate) of \spName~for set $i$ in year $t$, based on observed chum-bait hooks. % {\tt C\_it}, such that

Adapting equations on page 3 of \cite{yocld08}, we then have:
\eb
C_{it} = \frac{N_{it}}{E_{it}}.
\label{catchPerSetIPHC}
\ee
The catch rate index for year $t$, $I_t$ (numbers per effective skate), is then the mean catch rate across all sets:    % $I_{t\survey}$
\eb
I_{t} = \frac{1}{n_t} \sum_{i=1}^{n_t} C_{it} = \frac{1}{n_t} \sum_{i=1}^{n_t} \frac{N_{it}}{E_{it}}.
\label{indexIPHC}
\ee
The catch rates $C_{it}$ within each year are boostrapped \Sexpr{num.boots} times to obtain the bootstrap statistics.

\subsection{Results - presumably use the text in {\tt IPHCcombin46.Snw} for a write up. Tables and figures and captions are correct}

<<effTable, results=tex, echo=FALSE>>=
# qwe = summarise(group_by(setValsKeep, year), min = min(E_it), mean = mean(E_it), max = max(E_it), lower = quantile(E_it, 0.025), higher=quantile(E_it, 0.975))
effSumm = summarise(group_by(setValsKeep, year), lower = quantile(E_it, 0.025),
    mean = mean(E_it), higher=quantile(E_it, 0.975))
names(effSumm) = c("Year", "Lower", "Mean", "Higher")
effSummTab = xtable(effSumm,
    caption=eval(paste("Summary of effective skate numbers, $E\\_{it}$, for each
     year. Lower and Higher are the 2.5\\% and 97.5\\% quantiles,
     respectively.")),
     lab="tab:effSumm")
# omit to have later in summary
# print(effSummTab, table.placement="tp", caption.placement="top",
#    include.rownames = FALSE, sanitize.text.function=function(x){x})  # was !ht
@
% Table \ref{tab:effSumm} shows that the values of the effective skate numbers does change over time, but with no clear trend. The lowest value for 2012 is due to only four skates (those with Chum Salmon as bait) being usable. The year 1995 is also low, but has the same mean as 2007 and 2008. ***1996

% The number of sets each year is lower in 1995 than for 2003$+$ (Table \ref{tab:bcaConfKeep}). Year 2008 has one fewer set because for station number 2113 the hook tally sheet was lost overboard \citep{yfcd11}.  ***1996


<<bcaConfKeepTable, results=tex, echo=FALSE>>=
xxxxbcaKeep = bcaConfKeep[ , c("year", "n_t", "propWithoutSp", "I_t",
    "I_tBootMean", "I_tBootLow", "I_tBootHigh", "I_tBootCV")]
                                   # duplicated to rearrange & change names
names(xxxxbcaKeep) = c("Year", "Sets, $n_t$", spAbbr,
         "Sample $\\bar{I}_t$", "B'ed $I_t$",
         "B'ed $I_t$ lower",
         "B'ed $I_t$ higher",
         "B'ed $I_t$ CV")
# can't say 2.5% and 97.5% since that might not be correct, just say 95%
xxxxbcaKeepTab = xtable(xxxxbcaKeep,
    caption=eval(paste("Catch rates by year based on the stations being kept
     in Figure
     \\ref{fig:stations959603Keep2}. B'ed means bootstrapped
     value. `\\spAbbr' is the proportion of sets that did not catch \\spName     that year. Lower and higher are the
     lower and upper bounds of the 95\\% bias-corrected and adjusted (BCa)
     confidence intervals. **IGNORE ME**")),
     lab="tab:bcaConfKeep")
# Omit to have later in summary:
# print(xxxxbcaKeepTab, table.placement="tp", caption.placement="top",
#      include.rownames = FALSE, sanitize.text.function=function(x){x})  # was !ht
@
<<bootPlot, echo=FALSE, results=hide>>=
postscript("bcaConfKeep959603.eps", height = figheight, width = figwidth,
           horizontal=FALSE,  paper="special")
plotCI(years, bcaConfKeep$I_tBootMean, li=bcaConfKeep$I_tBootLow,
       ui=bcaConfKeep$I_tBootHigh, col="black", barcol="blue", lwd=1,
       ylim=c(0, max(bcaConfKeep$I_tBootHigh)),
       xlab="Year", ylab="Catch rate index (numbers per effective skate)")
points(years, bcaConfKeep$I_t, col="red", pch=20, cex=0.7)
# For all hooks (which prob want on same plot at some point) it was:
# plotCI(years, bcaConfExcl0$I_tBootMean, li=bcaConfExcl0$I_tBootLow,
#       ui=bcaConfExcl0$I_tBootHigh, col="black", barcol="blue", lwd=1,
#       ylim=c(0, max(bcaConfExcl0$I_tBootHigh)),
#       xlab="Year", ylab="Catch rate index (numbers per effective skate)")
# points(years, bcaConfExcl0$I_t, col="red", pch=20, cex=0.7)
dev.off()
@

% **May want something like this automated:
% The proportion of sets each year with zero catch of \spName~(Table \ref{tab:bcaConfKeep}) ranges from \Sexpr{round(min(filter(bcaConfKeep, years > 2002.5)$propWithoutSp), digits=2)}-\Sexpr{round(max(filter(bcaConfKeep, years > 2002.5)$propWithoutSp), digits=2)} for the 2003$+$ series, with the highest value being for 2012, which has the lowest effective skate number because of the bait experiment. A higher proportion of zeros (\Sexpr{min(round(filter(bcaConfKeep, years < 2002.5)$propWithoutSp, digits=2))}-\Sexpr{max(round(filter(bcaConfKeep, years < 2002.5)$propWithoutSp, digits=2))}) occurs for earlier data. ***Need to say/think more about this?? Add something about 1996 if necessary.  %$

The bootstrapped results are shown in (Table \ref{tab:bcaConfKeep}) and plotted in Figure \ref{fig:bcaConfKeep959603}.

% \onefig{bcaConfKeep959603}{Catch rate index (number of individual \Sexpr{spName} caught per skate). For a given year, the catch rate for each set is calculated from (\ref{catchPerSetIPHC}). These catch rates are then resampled for \Sexpr{num.boots} bootstrap values, from which a bootstrapped mean (open blue circles) and 95\% bias-corrected and adjusted confidence intervals (blue bars) are calculated. Red closed circles are sample means (not bootstrapped).}

% c) How to deal with all the zero catches? Could use an approach based on his MEE paper (see my written notes). Would more properly account for the zeros. He couldn't say whether or not the CV's would end up being reduced. Bootstrapping seems okay, especially as always at the same site. For write-up I can say:

\bibliographystyle{resDoc}
\bibliography{../../abbrev4}                   %  if starting a new page

\section{Summary of results, plus some maps from earlier will go into the write-up}

This analysis is for 1995, 1996 and 2003$+$, for all observed hooks. Excluding the stations indicated in the following maps [will do one for each year, for now they're overlapped]. Excluded stations are those south of \Sexpr{latCutOff}$^{\circ}$, which is the *****edit as necessary*****: northern tip of Vancouver Island. This is because some years the survey did not occur off the west coast of Vancouver Island.

\subsection{Maps showing locations of stations for all years}

\clearpage

\onefig{stations959603Keep2}{All stations, with those being omitted in the analyses indicated by black crosses, namely \Sexpr{sum(setVals1995$keep == 0)} stations for 1995, \Sexpr{sum(setVals1996$keep == 0)} stations for 1996 and \Sexpr{sum(blockLocs0314$keep == 0)} for 2003$+$.}   %$

A summary of effective skate numbers for each year is shown in Table \ref{tab:effSumm}.

<<effTable2, results=tex, echo=FALSE>>=
print(effSummTab, table.placement="tp", caption.placement="top",
      include.rownames = FALSE, sanitize.text.function=function(x){x})  # was !ht
@

The resulting bootstrapped catch rate index is shown in Figure \ref{fig:bcaConfKeep959603}, with values in Table \ref{tab:bcaConfKeep}

The bootstrapped results are shown in (Table \ref{tab:bcaConfKeep}) and plotted in Figure \ref{fig:bcaConfKeep959603}.

\onefig{bcaConfKeep959603}{Catch rate index (number of individual \Sexpr{spName} caught per skate). For a given year, the catch rate for each set is calculated from (\ref{catchPerSetIPHC}). These catch rates are then resampled for \Sexpr{num.boots} bootstrap values, from which a bootstrapped mean (open blue circles) and 95\% bias-corrected and adjusted confidence intervals (blue bars) are calculated. Red closed circles are sample means (not bootstrapped).}

<<effTable2, results=tex, echo=FALSE>>=
print(xxxxbcaKeepTab, table.placement="tp", caption.placement="top",
      include.rownames = FALSE, sanitize.text.function=function(x){x})  # was !ht
@

<<>>=
save.image(file="iphcSerBallHooksYYR.RData")
@

\end{document}
