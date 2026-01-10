---
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
    orcid:  
    affiliation: 3
  - name: Selia Markley
    orcid: 
    affiliation: 4
  - name: Alan Brookes
    orcid: 
    affiliation: 2
  
affiliations:
 - name: Office of Water, United States Environmental Protection Agency
   index: 1
 - name: Office of Research and Development, United States Environmental Protection Agency
   index: 2
 - name: Oak Ridge Associated Universities Student Services Contractor c/o United States Environmental Protection Agency
   index: 3
 - name: Oak Ridge Institude for Science and Education Fellow c/o United States Environmental Protection Agency
   index: 4

citation_author: Weber et. al.
date: 25 July 2025
year: 2025
bibliography: paper.bib
output:
  bookdown::pdf_book:
    base_format: rticles::joss_article
csl: apa.csl
journal: JOSS

header-includes:
  - \usepackage{graphicx}
  - \providecommand{\tightlist}{\setlength{\itemsep}{0pt}\setlength{\parskip}{0pt}}
  - \providecommand{\pandocnewline}{\\}
  - \providecommand{\pandocbounded}[1]{#1}
  - \providecommand{\pandocboundedpreserve}[1]{#1}
  - \usepackage{fvextra}  % extends fancyvrb
  - \DefineVerbatimEnvironment{Highlighting}{Verbatim}{breaklines,breakanywhere,breakautoindent=true,commandchars=\\\{\},fontsize=\small}
  - \DefineVerbatimEnvironment{Verbatim}{Verbatim}{breaklines,breakanywhere,breakautoindent=true,fontsize=\small}
---

# Summary

`StreamCatTools` provides functions for easily working with, visualizing and analyzing StreamCat [@hill2016streamcat] <!-- In PDF, all authors are listed in citations rather than "Hill et al." -->and LakeCat [@hill2018lakecat] watershed metrics within **R**. The StreamCat and LakeCat datasets provide hundreds of landscape metrics for both the local catchment (e.g. landscape draining to a particular stream reach) and full watershed for every stream reach and lake depicted in the medium resolution National Hydrography Dataset Plus Version 2.1 (NHDPlusV21)[@mckay2012nhdplus] for the contiguous United States (CONUS). `StreamCatTools` functions wrap the application programming interface (API) for the StreamCat and LakeCat online database and facilitate simple, straightforward access and use of these datasets within R.

# Statement of Need

Easily accessible, robust, and consistent watershed data is an underpinning of hydrology research, water quality monitoring programs, and predictive modelling applications, to name just a few examples.  The StreamCat [@hill2016streamcat] and LakeCat [@hill2018lakecat] datasets fill this need by providing nationally consistent curated watershed data for CONUS that has had stringent quality control applied. The data encompasses hundreds of watershed metrics for every stream reach and lake feature represented in the NHDPlusV21 [@mckay2012nhdplus]. StreamCatTools fills the need for easily accessible watershed metrics for CONUS by: (1) providing a simple interface in R to the StreamCat and LakeCat web services, (2) providing convenient functionality to find available StreamCat and LakeCat metric names and information<!-- Can we  be more specific about information? Maybe not. -->, (3) extracting StreamCat and LakeCat metrics by COMID (a unique identifier in the NHDPlusV2 framework), by state, by county, by NHD Hydro-region, or for all of CONUS. Providing this valuable watershed data via web services in R follows the FAIR principles laid out in [@wilkinson2016fair].

# StreamCatTools Functionality

`StreamCatTools` provides a simple streamlined set of functions to easily query and ingest watershed landscape metrics into an R session. Figure \@ref(fig:flowchart)<!-- Text may need to be larger for journal pub --> shows the overall framework of the StreamCat database, application programming interface, and functionality in the package that simplifies data access in R using web services for the StreamCat and LakeCat datasets. 

<!-- Spell out API in figure caption -->

\begin{figure}

{\centering \includegraphics[width=0.7\linewidth]{Flowchart} 

}

\caption{Diagram of the StreamCat and StreamCatTools framework. A backend Oracle database and web service are exposed through api.gov to functions in StreamCatTools which simplify accesss and analysis of the data in R via the application programming interface.}(\#fig:flowchart)
\end{figure}

The core functions in `StreamCatTools` leverage the `httr2` library [@wickham2025httr2] for a modern, pipeable method for working with APIs. Specifically, `StreamCatTools` simplifies the calls to the API for StreamCat and LakeCat data within the R programming language to allow a user to: (1) access details on available StreamCat and LakeCat metrics, (2) match users' sites to NHDPlusV21 waterbodies (streams or lakes) to access StreamCat and Lakecat data, and (3) retrieve StreamCat and LakeCat watershed metrics by COMID, by state, by county, by hydroregion, or for all of CONUS.  Additionally, several convenience functions retrieve data from particular years for data with time series, such as the National Land Cover Database (NLCD) [@usgsnlcd] <!-- citation looks odd in PDF --> and the National Nutrient Inventory (NNI) [@nutinventory]. 

You can install the most recent version of `StreamCatTools` from CRAN by running:


``` r
# install.packages("StreamCatTools")
```

You can install the most recent development version from GitHub by running:

``` r
# library(remotes)
# install_github("USEPA/StreamCatTools", build_vignettes=FALSE)
```

`StreamCatTools` is loaded into an **R** session:


``` r
library(StreamCatTools)
```

`StreamCatTools` includes several functions to facilitate accessing and working with the data. First, users can list metric names and find out more about StreamCat and LakeCat data available in StreamCat using using the `sc_get_params` function. This function also provides details about metrics, such as area of interest for which data are available (e.g., local catchment vs. watershed) and years of available data if applicable. `sc_get_params` returns a tibble which facilitates automated access and searches of metric characteristics. Additional functions provided for getting metadata about the underlying StreamCat and LakeCat data include the `sc_fullname` and `lc_fullname` functions and the `sc_get_params` and `lc_get_params` functions, respectively. <!-- Some of this seems redundant to what we just said (will think about word-smithing - MW) --> Users can also filter metric names and information by the metric year(s), the indicator categories for metrics, the metric dataset names, or the areas of interest the metrics are available for using the `sc_get_metric_names` or `lc_get_metric_names` functions. More details on these functions can be found at the [package introduction page](https://usepa.github.io/StreamCatTools/articles/Articles/Introduction.html). 

The primary package functionality is in the `sc_get_data` and `lc_get_data` functions which allow users to extract catchment or watershed metrics of interest by providing NHDPlusV21 COMIDs for streams or lakes within the database. Users can also request data for a given state(s), county(ies), hydroregion(s), or all of CONUS.  Additionally, convenience functions are provided for accessing the NLCD and NNI datasets, respectively, using `sc_nlcd` and `lc_nlcd` and `sc_nni` and `lc_nni`. 

The following example shows a request for two catchment and watershed metrics for three stream reaches:


``` r
df <- sc_get_data(metric='pcturbmd2019,damdens',
                  aoi='cat,ws', 
                  comid='179,1337,1337420')
```

The first line requests percent of NLCD medium intensity developed land cover in 2019 and the density of dams. The second line specifies that the geographic area of interest (aoi) is the drainage to the local reach scale (i.e. 'cat', short for catchment) and the full watershed. Finally, the final line request this data for three NHDPlusV2 stream segments specified by their unique COMIDs.

# Applications

Watershed metrics from `StreamCatTools` can be easily visualized with functions from `nhdplusTools` [@blodgett2016nhdplustools] and `ggplot2` [@wickham2016ggplot2]. The example below plots the NLCD percent imperviousness for the the local drainage (catchment in NHDPlusV2 syntax) and displays the values mapped to each stream reach and to the overall basin boundary:


``` r
library(nhdplusTools)
library(ggplot2)
library(ggspatial)
library(StreamCatTools)

start_comid = 23763517
nldi_feature <- list(featureSource = "comid", featureID = start_comid)
flowline_nldi <- nhdplusTools::navigate_nldi(nldi_feature, mode = "UT", data_source = "flowlines", distance=5000)
df <- sc_get_data(metric='pctimp2019', aoi='cat', comid=flowline_nldi$UT_flowlines$nhdplus_comid)
flowline_nldi <- flowline_nldi$UT_flowlines
flowline_nldi$PCTIMP2019 <- df$pctimp2019cat[match(flowline_nldi$nhdplus_comid, df$comid)]
basin <- nhdplusTools::get_nldi_basin(nldi_feature = nldi_feature)

calapooia <- ggplot() +
    geom_sf(data = basin,
            fill = NA,
            color = "black",
            linewidth = 1) +
    geom_sf(data = flowline_nldi,
            aes(colour = PCTIMP2019)) +
    scale_y_continuous() +
    scale_color_distiller(palette = "Spectral") +
    labs(color = "Pct Imperviousness") +
    theme_minimal(12) +
    ggtitle('Percent Imperviousness for \nthe Calapooia River Basin 2019')

plot(calapooia)
```

![](JOSS_files/figure-latex/unnamed-chunk-5-1.pdf)<!-- --> 
<!-- To Do -->
Also show: new functionality for accessing and plotting NNI data.  Perhaps any other uses or applications we are aware of (such as CASTools R Shiny app).

# Acknowledgements

Examples of using StreamCat and LakeCat make extensive use of `nhdplusTools`[@blodgett2016nhdplustools] and the functions for accessing the API are facilitated through use of `httr2`[@wickham2025httr2]. Figures were created using `ggplot2` [@wickham2016ggplot2]. 

We would like to sincerely thank the editor and reviewers for all of their helpful feedback which greatly improved both the software and the manuscript.

The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.The information in this document has been funded entirely by the United States Environmental Protection Agency (USEPA), in part through appointments to the USEPA’s Internship/Research Participation Program at the Office of Research and Development administered by the Oak Ridge Institute for Science and Education through an interagency agreement. The views expressed in this article are those of the authors and do not necessarily represent the views or policies of the USEPA. 

# References
