# gfiphc: An R package for extracting and analysing groundfish data from the IPHC longline survey in BC

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/pbs-assess/gfiphc.svg?branch=master)](https://travis-ci.com/pbs-assess/gfiphc)

[![R-CMD-check](https://github.com/pbs-assess/gfiphc/workflows/R-CMD-check/badge.svg)](https://github.com/pbs-assess/gfiphc/actions)
<!-- badges: end -->

[If the badge above says `build error` you can probably ignore it and install
the package as usual. It's a Travis thing and I'll soon update it to GitHub Actions]

The International Pacific Halibut Commission (IPHC) conducts an annual stock assessment longline survey in waters from California to Alaska, including British Columbia (BC) waters. The survey's main goal is to provide data on Pacific Halibut (*Hippoglosus stenolepis*) for stock assessment purposes. However, data are also recorded on other species caught, making it the longest ongoing groundfish survey in BC waters and hence a valuable source of data for many species.

The gfiphc package contains functions for:

- extracting data from the GFBio (GroundFish Biology) database, housed at the Pacific Biological Station, Nanaimo, BC, Canada
- plotting maps (and movies) showing locations of stations each year
- deriving relative biomass index trends from the IPHC survey for non-halibut groundfish species along the full BC coast, taking into account the changing survey methodologies and using consistent published methods
- input into the [groundfish synopsis report](https://github.com/pbs-assess/gfsynopsis) for over 100 species of BC groundfish, which is to be updated every couple of years
- generating trends for a group of species (e.g. "skates combined")
- deriving relative biomass index trends for a specified area within BC waters, again taking into account the changing survey methodologies.

The latter example is being used to generate predators of Haida Gwaii Pacific Herring as information for a Case Study in DFO's Ecoystem Approach to Fisheries Management initiative.

Users outside of PBS can request someone at PBS to extract the data (try Andrew Edwards first - he has the data extracted and saved from running the `All species` vignette below) and then the analyses within the package will work. The set-level information (not species specific) is built into the package, as is some species-specific data.

## A quick look

To see the available data for your particular species of interest, look at the saved [All species](http://htmlpreview.github.io/?https://github.com/pbs-assess/gfiphc/blob/master/vignettes/data_for_all_species.html) vignette, and scroll down to the species (the order is the same as in [Anderson et al., 2019](https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2019/2019_041-eng.html)). If there are no time series shown, or there are but they have lots of zeros, then you can conclude that the survey does not really catch your species of interest (unless you want to look at occasional observations). If the data do look useful then you'll likely want install and understand the package.

## Vignettes

The vignettes are the best place to start, as they should cover many analyses users would want to do. The links here go to  the saved .html versions on GitHub. The raw Rmarkdown code is available in the [vignettes](vignettes/) folder and should be used as a template.

- [Single species](http://htmlpreview.github.io/?https://github.com/pbs-assess/gfiphc/blob/master/vignettes/data_for_one_species.html) - shows example data that are saved in the package, and for extracting and generating the longest index possible for a single species for the whole BC coast.
- [Restricted area](http://htmlpreview.github.io/?https://github.com/pbs-assess/gfiphc/blob/master/vignettes/analysis_for_restricted_area.html) - for extracting and generating the longest index possible for a restricted area of interest.
- [Herring predators](http://htmlpreview.github.io/?https://github.com/pbs-assess/gfiphc/blob/master/vignettes/analysis_for_HG_herring_predators.html) - extraction of multiple species, and groups of species, for a restricted area, and analysed and saved as inputs for the Haida Gwaii Pacific Herring Case Study.
- [All species](http://htmlpreview.github.io/?https://github.com/pbs-assess/gfiphc/blob/master/vignettes/data_for_all_species.html) - extraction of 113 species, to test updating of the groundfish synopsis report [(Anderson et al. 2019)](https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2019/2019_041-eng.html).

## Installation

In R you can install the latest version of the package with:

```
devtools::install_github("pbs-assess/gfiphc")
```

## Methods to generate indices of abundance

Methods are described in detail in Appendix G of the groundfish synopsis report [(Anderson et al. 2019)](https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2019/2019_041-eng.html). The methods build on those developed for assessments of
Redbanded Rockfsh [(Edwards et al. 2017)](https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2017/2017_058-eng.html) and Yelloweye Rockfsh [(Yamanaka et al. 2018)](https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2018/2018_001-eng.html) . The Redbanded assessment was the first to develop an abundance index from the IPHC survey that went back to 1995, and included data up to 2012. For the Yelloweye assessment the methods were extended to demonstrate that the index based on waters north of Vancouver Island could be considered representative of the coastwide population. The synopsis report includes preliminary investigations into hook competition.

Updates since the 2019 synopsis report include

- splitting the original functions out of `gfplot` and into their own package `gfiphc`
- vignettes and better documentation
- Series A-D automatically exclude the expanded grid stations for 2018 (these were included in the original synopsis report)
- 2019 data are now in GFBio and automatically extracted in the queries here
- 2020 data are included in `gfiphc` since only the first 20 hooks of each skate were enumerated (and previous years in GFBio had all hooks enumerated).
- 2020 data included also used an expanded grid, defined slightly differently to the expanded grid in 2018. We have stuck with the 2018 definition of which stations are not part of the standard grid. See [iphc-2020-data.pdf](data-raw/iphc-2020-data.pdf), and it's associated `.Rmd` if desired, for details. Also see that for instructions on extracting future years' data from the new IPHC data website. See the IPHC website for full details on survey protocols -- note that fishing in 2020 was mostly in July and August, whereas it is usually May to August.
- the boostrapping now uses 10,000 bootstrap samples instead of 1000, as it made a difference for some species.

Note that the analyses still exclude hook competition, but we are working on that (using outputs from `gfiphc`).

## Differing data-collection protocols

A particular issue is that the data resolution is not the same across the years. This is explained in Table G.1 in the synopsis report. An updated version of that table is given here:

|Year         |Hooks enumerated          |Data resolution    |Location of data       |WCVI? |
|-------------|--------------------------|-------------------|-----------------------|-------|
|1995         |All                       |Set-by-set         |gfiphc package         |N      |
|1996         |All                       |Set-by-set         |gfiphc package         |N      |
|1997-1998    |First 20 of each skate    |Set-by-set         |gfiphc package         |N      |
|1999         |First 20 of each skate    |Set-by-set         |gfiphc package         |Y      |
|2000         |First 20 of each skate    |Set-by-set         |gfiphc package         |N      |
|2001-2002    |First 20 of each skate    |Set-by-set         |gfiphc package         |Y      |
|2003-2011    |All                       |Hook-by-hook       |DFO database GFBio     |Y      |
|2012         |All (bait experiment)     |Hook-by-hook       |DFO database GFBio     |Y      |
|2013         |First 20 of each skate    |Set-by-set         |gfiphc package         |Y      |
|2014-2017    |All                       |Hook-by-hook       |DFO database GFBio     |Y      |
|2018         |All (+ expansions stns)   |Hook-by-hook       |DFO database GFBio     |Y      |
|2019         |All                       |Hook-by-hook       |DFO database GFBio     |Y      |
|2020         |First 20 of each skate    |Set-by-set         |gfiphc package         |N      |

<!-- for putting back into gfsynopsis, note that 2018 is now separated out) -->
In 2018 there were extra expansion stations surveyed (see the vignettes), and in 2020 only the first 20 hooks were enumerated. For 2020, the data were downloaded from the IPHC website and included in the package (see below).

The structure of the data in GFBio is described by [Cooke and Olsen (2020)](https://waves-vagues.dfo-mpo.gc.ca/Library/40879100.pdf), but gfiphc's R functions and built-in SQL queries mean that gfiphc users do not need to have knowledge of GFBio.

## Summary of Series that can be constructed

Due to the differing data-collection protocols, we developed different ways to obtain as long a time series as possible (see Appendix G). We first defined Series A, B, C, and D in Table G.2 (reproduced below), combined and compared them as possible to define Series AB for the north of West Coast of Vancouver Island (WCVI), and tested whether Series AB could be considered as representing the full coast (see Appendix G for full details). Here we also look at a user-defined restricted area of interest, defining Series E to be based on the first 20 hooks from each skate, and Series F to be based on all hooks from each skate (see the the vignette). Series A-F are defined as:

|                                | Only north of WCVI | Full coast | Restricted area of interest |
|--------------------------------|--------------------|------------|-----------------------------|
| First 20 hooks from each skate | A (24)             | D (20)     | E                           |
| All hooks from each skate      | B (18)             | C (16)     | F                           |

with numbers in parentheses indicating the number of years for which data for each Series are available. For E and F this depends on the particular area of interest.

For any spatiotemporal analyses, note that there the IPHC assign a "don't use for spatiotemporal models" code for
each set, so look into that if necessary.

## Updating with new data each year

For 2020, only the first 20 hooks were evaluated from each skate. So, like for 2013, the data have been included in this package. For future years, copy the code `data-raw/iphc-2020-data.Rmd` (and rename for a new year) and follow the instructions for downloading the data from the IPHC website, checking the data, and saving it formatted for this package. The results [iphc-2020-data.pdf](data-raw/iphc-2020-data.pdf) is also available for easier reading (only commit a final version of future .pdf's, not as you are working on it).

So:

1. Adapt `data-raw/iphc-2020-data.Rmd` as just described.

1. Create/adapt new functions to include the data in the calculations. For data in GFbio this should be automatic, else see what was done just after commit a5ecd2a for 2020, or (easier) see these notes made updated in 2021:

1. Further files in `R/` that need checking (search through for most recent year and update; I checked all `R/` files in 2021 and these are the ones; this could be automated but it's actually good to look through the code each year:

   * gfiphc.R  
   * iphc-data.R
   * plot-iphc.R

1. Check that the lines for new year `data_names_XXXX` in `check_iphc_spp_name()` in `iphc-data.R` got updated.

1. Re-run (line by line to check) `data-raw/sets-skates-hooks-yelloweye.R` to save some more new data into package.

1. GOT TO HERE

1. Go through and rerun the vignettes, adapting any called functions or printed results as necessary to include latest year of data, making sure the latest year is included and the maps make sense.

1. May have to repeat this process as necessary. For example, in 2020 I saved the data, but then kept examining it in `data-raw/iphc-2020-data.Rmd`, realising need to change a `standard` designation in `setDataExpansion`, so then had to rerun all of these steps to update all .rda files.

1. Re-run `data-raw/iphc-****-data.Rmd` again to use all the finalised saved data, and commit its .pdf file.

1. Update the two tables above of what data are available each year and what Series can be made, checking with vignettes. Add notes as necessary (e.g. 2021 only subsampled off WCVI).
 
1. Update version number.

## Citation

If you use this package please cite it as it is helpful for maintaining it:

Edwards, A.M., S.C. Anderson, E.A. Keppel and C.Grandin (2021). gfiphc: Data Extraction and Analysis for Groundfish Data from the IPHC Longline Survey in BC. R package version 1.0.0.

Check `citation("gfiphc")` for the latest version number, and for the bibtex entry (you have to manually add the year).

## Issues, problems

Please report any problems as a [GitHub Issue](https://github.com/pbs-assess/gfiphc/issues), using a minimal working example if possible (and please check the closed issues first).
