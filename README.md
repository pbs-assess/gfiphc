# gfiphc: An R package for extracting and analysing groundfish data from the IPHC longline survey in BC

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/pbs-assess/gfiphc.svg?branch=master)](https://travis-ci.com/pbs-assess/gfiphc)

<!-- badges: end -->

The International Pacific Halibut Commission (IPHC) conducts an annual stock assessment longline survey in waters from California to Alaska, including British Columbia (BC) waters. The survey's main goal is to provide data on Pacific Halibut (*Hippoglosus stenolepis*) for stock assessment purposes.

The gfiphc package contains functions for deriving relative biomass index trends from the IPHC survey for non-halibut groundfish species in British Columbia waters. It is used as part of the [groundfish synopsis report](https://github.com/pbs-assess/gfsynopsis) and is also useable on its own.

The functions in this package were originally part of gfplot and have now been split out into their own package.

See the [vignette](vignettes/data_for_one_species.Rmd) for code for extracting data for a single species. 

## Installation

You can install the latest version of the package with:

``` r
devtools::install_github("pbs-assess/gfiphc")
```
