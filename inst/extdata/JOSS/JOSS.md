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
  - name: Alan Brookes
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

`StreamCatTools` provides functions for easily working with, visualizing and analyzing the StreamCat[@hill2016streamcat] and LakeCat[@hill2018lakecat] data and API within **R**. StreamCat and LakeCat provide hundreds of landscape metrics for both the local catchment and full watershed for every stream reach and lake depicted in the medium resolution National Hydrography Dataset Plus Version 2.1 (NHDPlusV21)[@mckay2012nhdplus]

# Statement of Need

Easily accessible, robust, and consistent watershed data is an underpinning of hydrology research, water quality monitoring programs, and predictive modelling applications, to name just a few examples.  The StreamCat[@hill2016streamcat] and LakeCat[@hill2018lakecat] datasets fill this need by providing nationally consistent watershed data that has had stringent quality control applied for the conterminous United States (CONUS). The data encompasses hundreds of watershed metrics for every stream reach and lake feature represented in the NHDPlusV21[@mckay2012nhdplus]. StreamCatTools fills this need for easily accessible watershed metrics for CONUS by: (1) providing a simple interface in R to the StreamCat and LakeCat web services, (2) providing convenient functionality to find available StreamCat and LakeCat metric names and information, (3) extracting StreamCat and LakeCat metrics by COMID (a unique identifier in the NHDPlusV2 framework), by state, by county, by NHD Hydroregion, or for all of CONUS. Providing this valuable watershed data via web services in R follows the FAIR principles laid out in [@wilkinson2016fair].

# Package Overview

How it works

Installing `StreamCatTools` 

``` r
# library(remotes)
# install_github("USEPA/StreamCatTools", build_vignettes=FALSE)
```

`StreamCatTools` is loaded into an **R** session:

``` r
library(StreamCatTools)
```

Examples


# Discussion

Let's talk StreamCat!

# Acknowledgements

Examples of using StreamCat and LakeCat make extensive use of `nhdplusTools`[@blodgett2016nhdplustools] and the functions for accessing the API are facilitated through use of `httr2`. Figures were created using `ggplot2` [@wickham2016ggplot2]. 

We would like to sincerely thank the editor and reviewers for all of their helpful feedback which greatly improved both the software and the manuscript.

The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.

# References
