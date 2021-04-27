# gfiphc: An R package for extracting and analysing groundfish data from the IPHC longline survey in BC

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/pbs-assess/gfiphc.svg?branch=master)](https://travis-ci.com/pbs-assess/gfiphc)

<!-- badges: end -->

The International Pacific Halibut Commission (IPHC) conducts an annual stock assessment longline survey in waters from California to Alaska, including British Columbia (BC) waters. The survey's main goal is to provide data on Pacific Halibut (*Hippoglosus stenolepis*) for stock assessment purposes. However, data are also recorded on other species caught, making it the longest ongoing groundfish survey in BC waters and hence a valuable source of data for many species. 

The gfiphc package contains functions for: 

- extracting data from the GFBio database (some of the IPHC data is saved within the package itself)
- plotting maps (and movies) showing locations of stations each year
- deriving relative biomass index trends from the IPHC survey for non-halibut groundfish species along the full BC coast, taking into account the changing survey methodologies
- input into the [groundfish synopsis report](https://github.com/pbs-assess/gfsynopsis) for over 100 species of BC groundfish, which is to be updated every couple of years
- deriving relative biomass index trends for a specified area within BC waters, again taking into account the changing survey methodologies. 

The latter example is being used to generate predators of Haida Gwaii Pacific Herring as information for a Case Study in DFO's Ecoystem Approach to Fisheries Management initiative. 

The functions in this package were originally part of gfplot and have now been split out into their own package.

## Vignettes (under development - rendered .html versions will be saved at some point)

- [Single species](vignettes/data_for_one_species.Rmd) - for extracting and generating the longest index possible for a single species for the whole BC coast.
- [Restricted area](vignettes/data_for_restricted_area.Rmd) - for extracting and generating the longest index possible for a restricted area of interest.
- [Herring predators](vignettes/data_for_HG_herring_predators.Rmd) - extraction of multiple species, and groups of species, for a restricted area. Then saved to be inputs for the Haida Gwaii Pacific Herring Case Study. 

## Methods

Methods are described in detail in Appendix G of the groundfish synopsis report [Anderson et al. (2019)](https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2019/2019_041-eng.html). The methods build on those developed for assessments of
Redbanded Rockfsh [(Edwards et al. 2017)](https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2017/2017_058-eng.html) and Yelloweye Rockfsh [(Yamanaka et al. 2018)](https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2018/2018_001-eng.html) . The Redbanded assessment was the first to develop an abundance index from the IPHC survey that went back to 1995, and included data up to 2012. For the Yelloweye assessment the methods were extended to demonstrate that the index based on waters north of Vancouver Island could be considered representative of the coastwide population. The synopsis report includes preliminary investigations into hook competition.

## Installation

You can install the latest version of the package with:

``` r
devtools::install_github("pbs-assess/gfiphc")
```

## Issues, problems

Please report any problems as a [GitHub Issue](https://github.com/pbs-assess/gfiphc/issues), using a minimal working example if possible (and please check the closed issues first).