---
# Example from https://joss.readthedocs.io/en/latest/submitting.html
title: 'StreamCatTools: An R package for working with StreamCat and LakeCat watershed data in R'
tags:
  - R
  - Watersheds
  - NHDPlus
  - API
authors:
  - name: Marc H. Weber
    orcid: 0000-0002-9742-4744
    affiliation: 1
  - name: Ryan A. Hill
    orcid: 0000-0001-9583-0426
    affiliation: 2
  - name: Travis Hudson
  - name: Alan Brooks
  - name: Selia Markley
  
affiliations:
 - name: Office of Water, United States Environmental Protection Agency
   index: 1
 - name: Pacific Ecological Systems Division, United States Environmental Protection Agency
   index: 2
citation_author: Weber et. al.
date: 25 July 2025
year: 2025
bibliography: paper.bib
output: rticles::joss_article
csl: apa.csl
journal: JOSS
---

# Summary

`StreamCatTools` provides functions for easily working with, visualizing and analyzing the StreamCat[@hill2016streamcat] and LakeCat[@hill2018lakecat] data and API within **R**. StreamCat and LakeCat provide hundreds of landscape metrics for both the local catchment and full watershed for every stream reach and lake depicted in the medium resolution National Hydrography Dataset Plus Version 2.1 (NHDPlus21)[@mckay2012nhdplus]

# Statement of Need

StreamCat is awesome! 

# Package Overview

How it works

Installing `StreamCatTools` 

``` r
install.packages("SSN2")
```

`StreamCatTools` is loaded into an **R** session:

``` r
library(remotes)
install_github("USEPA/StreamCatTools", build_vignettes=FALSE)
```

```
## Using GitHub PAT from the git credential store.
```

```
## Downloading GitHub repo USEPA/StreamCatTools@HEAD
```

```
## rlang        (1.1.2  -> 1.1.6  ) [CRAN]
## glue         (1.6.2  -> 1.8.0  ) [CRAN]
## cli          (3.6.2  -> 3.6.5  ) [CRAN]
## bit          (4.0.5  -> 4.6.0  ) [CRAN]
## purrr        (1.0.2  -> 1.1.0  ) [CRAN]
## bit64        (4.0.5  -> 4.6.0-1) [CRAN]
## Rcpp         (1.0.12 -> 1.1.0  ) [CRAN]
## units        (0.8-5  -> 0.8-7  ) [CRAN]
## e1071        (1.7-14 -> 1.7-16 ) [CRAN]
## sf           (1.0-19 -> 1.0-21 ) [CRAN]
## s2           (1.1.7  -> 1.1.9  ) [CRAN]
## maplegend    (0.2.0  -> 0.3.0  ) [CRAN]
## classInt     (0.4-10 -> 0.4-11 ) [CRAN]
## sp           (2.1-4  -> 2.2-0  ) [CRAN]
## raster       (3.6-26 -> 3.6-32 ) [CRAN]
## terra        (1.8-29 -> 1.8-60 ) [CRAN]
## digest       (0.6.34 -> 0.6.37 ) [CRAN]
## curl         (6.2.2  -> 6.4.0  ) [CRAN]
## utf8         (1.2.4  -> 1.2.6  ) [CRAN]
## stringi      (1.8.4  -> 1.8.7  ) [CRAN]
## pillar       (1.10.1 -> 1.11.0 ) [CRAN]
## generics     (0.1.3  -> 0.1.4  ) [CRAN]
## tibble       (3.2.1  -> 3.3.0  ) [CRAN]
## R.oo         (1.27.0 -> 1.27.1 ) [CRAN]
## openssl      (2.3.2  -> 2.3.3  ) [CRAN]
## tzdb         (0.4.0  -> 0.5.0  ) [CRAN]
## httr2        (1.1.2  -> 1.2.1  ) [CRAN]
## pbapply      (1.7-2  -> 1.7-4  ) [CRAN]
## data.table   (1.15.4 -> 1.17.8 ) [CRAN]
## zip          (2.3.2  -> 2.3.3  ) [CRAN]
## arrow        (18.1.0 -> 21.0.0 ) [CRAN]
## mapsf        (0.12.0 -> 1.0.0  ) [CRAN]
## maptiles     (0.9.0  -> 0.10.0 ) [CRAN]
## dataRetri... (2.7.18 -> 2.7.20 ) [CRAN]
## nhdplusTools (1.3.1  -> 1.3.2  ) [CRAN]
```

```
## Installing 35 packages: rlang, glue, cli, bit, purrr, bit64, Rcpp, units, e1071, sf, s2, maplegend, classInt, sp, raster, terra, digest, curl, utf8, stringi, pillar, generics, tibble, R.oo, openssl, tzdb, httr2, pbapply, data.table, zip, arrow, mapsf, maptiles, dataRetrieval, nhdplusTools
```

```
## Installing packages into 'C:/Users/mweber/R/library'
## (as 'lib' is unspecified)
```

```
## package 'rlang' successfully unpacked and MD5 sums checked
## package 'glue' successfully unpacked and MD5 sums checked
## package 'cli' successfully unpacked and MD5 sums checked
## package 'bit' successfully unpacked and MD5 sums checked
## package 'purrr' successfully unpacked and MD5 sums checked
## package 'bit64' successfully unpacked and MD5 sums checked
## package 'Rcpp' successfully unpacked and MD5 sums checked
## package 'units' successfully unpacked and MD5 sums checked
## package 'e1071' successfully unpacked and MD5 sums checked
## package 'sf' successfully unpacked and MD5 sums checked
## package 's2' successfully unpacked and MD5 sums checked
## package 'maplegend' successfully unpacked and MD5 sums checked
## package 'classInt' successfully unpacked and MD5 sums checked
## package 'sp' successfully unpacked and MD5 sums checked
## package 'raster' successfully unpacked and MD5 sums checked
## package 'terra' successfully unpacked and MD5 sums checked
## package 'digest' successfully unpacked and MD5 sums checked
## package 'curl' successfully unpacked and MD5 sums checked
## package 'utf8' successfully unpacked and MD5 sums checked
## package 'stringi' successfully unpacked and MD5 sums checked
## package 'pillar' successfully unpacked and MD5 sums checked
## package 'generics' successfully unpacked and MD5 sums checked
## package 'tibble' successfully unpacked and MD5 sums checked
## package 'R.oo' successfully unpacked and MD5 sums checked
## package 'openssl' successfully unpacked and MD5 sums checked
## package 'tzdb' successfully unpacked and MD5 sums checked
## package 'httr2' successfully unpacked and MD5 sums checked
## package 'pbapply' successfully unpacked and MD5 sums checked
## package 'data.table' successfully unpacked and MD5 sums checked
## package 'zip' successfully unpacked and MD5 sums checked
## package 'arrow' successfully unpacked and MD5 sums checked
## package 'mapsf' successfully unpacked and MD5 sums checked
## package 'maptiles' successfully unpacked and MD5 sums checked
## package 'dataRetrieval' successfully unpacked and MD5 sums checked
## package 'nhdplusTools' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
## 	C:\Users\mweber\AppData\Local\Temp\RtmpItl2by\downloaded_packages
## -- R CMD build -----------------------------------------------------------------
##          checking for file 'C:\Users\mweber\AppData\Local\Temp\RtmpItl2by\remotes6ce08342428c2\USEPA-StreamCatTools-72a916c/DESCRIPTION' ...  v  checking for file 'C:\Users\mweber\AppData\Local\Temp\RtmpItl2by\remotes6ce08342428c2\USEPA-StreamCatTools-72a916c/DESCRIPTION' (686ms)
##       -  preparing 'StreamCatTools': (2s)
##    checking DESCRIPTION meta-information ...  v  checking DESCRIPTION meta-information
##       -  checking for LF line-endings in source and make files and shell scripts (394ms)
##   -  checking for empty or unneeded directories
##    Omitted 'LazyData' from DESCRIPTION
## -  building 'StreamCatTools_0.6.0.tar.gz'
##      
## 
```

```
## Installing package into 'C:/Users/mweber/R/library'
## (as 'lib' is unspecified)
```

Examples


# Discussion

Let's talk StreamCat!

# Acknowledgements

Examples of using StreamCat and LakeCat make extensive use of `nhdplusTools`[@blodgett2016nhdplustools] and the functions for accessing the API are facilitated through use of `httr2`. Figures were created using `ggplot2` [@wickham2016ggplot2]. 

We would like to sincerely thank the editor and reviewers for all of their helpful feedback which greatly improved both the software and the manuscript.

The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.

# References
