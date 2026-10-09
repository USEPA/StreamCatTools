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
    affiliation: 1
  - name: Selia Markley
    affiliation: 4
  - name: Travis Hudson
    affiliation: 3
  - name: Allen Brookes
    affiliation: 2

affiliations:
  - name: Office of Water, United States Environmental Protection Agency
    index: 1
  - name: United States Environmental Protection Agency Retired
    index: 2
  - name: Oak Ridge Associated Universities Student Services Contractor c/o United States Environmental Protection Agency
    index: 3
  - name: Oak Ridge Institute for Science and Education Fellow c/o United States Environmental Protection Agency
    index: 4

citation_author: Weber et al.
date: 25 August 2026
year: 2026
bibliography: paper.bib
csl: apa.csl
journal: JOSS
output: rticles::joss_article 
---



# Summary

`StreamCatTools` provides an R interface to the StreamCat[@hill2016streamcat] and LakeCat[@hill2018lakecat] data and APIs for retrieving national scale watershed and lake-basin metrics. The package gives researchers and managers access to hundreds of catchment- and watershed-scale metrics without manual geospatial accumulation.

# Statement of Need

Nationally consistent watershed data are essential for hydrology, water-quality assessment, and ecological modeling. The StreamCat [@hill2016streamcat] and LakeCat [@hill2018lakecat] datasets provide such data for every NHDPlusV21 [@mckay2012nhdplus] stream reach and lake basin in the conterminous United States (CONUS) and `StreamCatTools` makes these data accessible in R through a simple API wrapper for metric discovery and batched retrieval of data by regions such as NHDPlusV21 catchment (COMID), state, county, hydroregion, or CONUS, lowering the barrier to reproducible analysis and supporting the FAIR principles (Findable, Accessible, Interoperable, and Reusable) [@wilkinson2016fair].

# State of the Field

R packages for watershed science usually cover one of three tasks: network navigation, observation retrieval, or general geospatial layers. `hydrogeofetch` [@blodgett2026hydrogeofetch] and related tools support stream-network traversal, `dataRetrieval` [@dataRetrieval] supports hydrologic and water-quality observations, and many other packages provide raw geospatial data. The remaining gap is analysis-ready metrics already summarized to catchments and upstream watersheds and `StreamCatTools` provides programmatic access to these data in R, enabling researchers to retrieve covariates easily and reproducibly.

# Package Overview

`StreamCatTools` is a lightweight R interface to the StreamCat API. The package allows for metadata discovery, metric lookup, and data retrieval using `httr2` [@wickham2025httr2]. The core functions retrieve variables at multiple AOIs: catchment (cat), watershed (ws), riparian catchment (catrp100), riparian watershed (wsrp100), and an other category for non-standard metrics). They also support National Land Cover Database (NLCD) [@NLCD] and National Nutrient Inventory (NNI) [@nutinventory] time-series products.

\begin{figure}

{\centering \includegraphics[width=0.95\linewidth]{Flowchart} 

}

\caption{Diagram of the StreamCat and StreamCatTools framework. The backend Oracle database and REST web service are exposed through api.epa.gov to functions in StreamCatTools that simplify access and analysis of the data in R via the application programming interface (API). The example here shows the general categories of Oracle database tables in the StreamCat Oracle database.}\label{fig:flowchart}
\end{figure}

You can install `StreamCatTools` from *CRAN* or GitHub:


``` r
install.packages("StreamCatTools")
```



``` r
install.packages("pak")
library(pak)
pkg_install("github::USEPA/StreamCatTools")
```

`StreamCatTools` is loaded into an **R** session:


``` r
library(StreamCatTools)
```

Users can inspect metric names and areas of interest before querying the API:


``` r
aois <- sc_get_params(param='aoi')
aois
```

```
#> [1] "cat"      "catrp100" "other"    "ws"       "wsrp100"
```


``` r
names <- sc_get_params(param='metric_names')
names[1:10]
```

```
#>  [1] "agkffact"      "al2o3"         "bankfulldepth" "bankfullwidth"
#>  [5] "bfi"           "canaldens"     "cao"           "cbnf"         
#>  [9] "chem"          "clay"
```

The same pattern applies to LakeCat metadata retrieval with `lc_get_params()`.

In StreamCat and LakeCat individual catchments (the local drainage area for a stream or lake) are aggregated to the watershed using either a weighted average or a count/sum depending on the metric [@hill2016streamcat]. `sc_get_params()` and `lc_get_params()` return variable descriptions, units, years, and metric categories (Table 1)


Table: Example of variable information returned by the variable_info parameter in the sc_get_params function.

|Metric                  |Short Description                           |Year      |Units                  |
|:-----------------------|:-------------------------------------------|:---------|:----------------------|
|agkffact[AOI]           |Ag Soil Erodibility Kf Factor               |NA        |Unitless               |
|bfi[AOI]                |Base Flow Index                             |NA        |Percent                |
|huden[Year][AOI]        |Mean Housing Density                        |2010      |Count/Square Kilometer |
|inorgnwetdep[Year][AOI] |Mean Annual Precipitation-Weighted Nitrogen |2008      |Kilogram/Hectare/Year  |
|n_ags_[Year][AOI]       |Nitrogen Agricultural Surplus               |1987-2017 |Kilograms              |
|pcthighsev[Year][AOI]   |Percent High Burn Severity Class For Year   |1984-2018 |Percent                |

`sc_fullname()` and `lc_fullname()` return the full descriptive name for a given metric.:


``` r
sc_fullname(metric='pctgrs2019')
```

```
#> [1] "Grassland/Herbaceous Percentage 2019"
```

Users can also filter available metrics by year, category, dataset, and AOI using `sc_get_metric_names()` and `lc_get_metric_names()` (Table 2).


``` r
metrics <- sc_get_metric_names(category = c('Deposition','Climate'),
                               aoi=c('Cat','Ws'))
my_data <- head(metrics[,c('Category','Metric','AOI')],10)
```


Table: Example of metric names returned by the sc_get_metric_names function.

|Category   |Metric                  |AOI     |
|:----------|:-----------------------|:-------|
|Climate    |bfi[AOI]                |Cat, Ws |
|Deposition |inorgnwetdep[Year][AOI] |Cat, Ws |
|Deposition |nh4[Year][AOI]          |Cat, Ws |
|Deposition |no3[Year][AOI]          |Cat, Ws |
|Climate    |precip8110[AOI]         |Cat, Ws |
|Climate    |precip9120[AOI]         |Cat, Ws |
|Climate    |precip[Year][AOI]       |Cat, Ws |
|Deposition |sn[Year][AOI]           |Cat, Ws |
|Climate    |tmax8110[AOI]           |Cat, Ws |
|Climate    |tmax9120[AOI]           |Cat, Ws |

The `sc_get_data()` and `lc_get_data()` functions allow users to extract catchment or watershed metrics by chosen regional unit - below we request two catchment and watershed metrics (NLCD  medium intensity developed land cover and density of dams) for three stream reaches:


``` r
df <- sc_get_data(metric='pcturbmd2019,damdens',
                  aoi='cat,ws', 
                  comid='179,1337,1337420')
```

A parallel LakeCat request can be made by county or other spatial filter such as county FIPS codes for a state or county-level summary.:


``` r
df <- lc_get_data(metric='pctwdwet2006', aoi='ws', county='41003')
head(df)
```

```
#>      comid pctwdwet2006ws
#> 1 23769033      0.0000000
#> 2 23769029      0.1701645
#> 3 23769021      2.4697951
#> 4 23769003      0.0000000
#> 5 23768999      8.1185071
#> 6 23768983      0.0000000
```

Helper functions are available to discover the relevant FIPS codes for counties for data retrieval and simplify state- and county-level queries:  


``` r
df <- sc_get_data(metric='pctwdwet2006', aoi='ws', county='41003')
```


``` r
df <- sc_get_params(param='county') |> dplyr::filter(state == 'OR' & county_name == 'Benton County') |> dplyr::pull(fips)
```


``` r
# State-wide watershed queries are large; run the live call once, then
# cache to disk so subsequent knits read the cache instead of
# re-hitting (or stalling on) the API. Commit the .rds alongside paper.Rmd.
ct_cache <- "clay_agkffact_CT_ws.rds"
if (file.exists(ct_cache)) {
  df <- readRDS(ct_cache)
} else {
  df <- sc_get_data(metric = 'clay,agkffact', aoi = 'ws', state = 'CT')
  saveRDS(df, ct_cache)
}
head(df)
```

```
#>     comid agkffactws   clayws
#> 1 7702768     0.0177 6.100716
#> 2 7702200     0.0009 6.243493
#> 3 7702202     0.0011 6.368938
#> 4 7701112     0.0177 6.080299
#> 5 7701168     0.0054 6.092434
#> 6 7701214     0.0040 6.082292
```

`StreamCatTools` also provides convenience functions for the NLCD and NNI datasets through `sc_get_nlcd()`, `lc_get_nlcd()`, `sc_get_nni()`, and `lc_get_nni()`:


``` r
df <- sc_get_nlcd(comid='1337420', year='2019', aoi='ws')
```


``` r
df <- sc_get_nni(year='1987, 1990, 2005, 2017', aoi='cat,ws',
                 comid='179,1337,1337420')
```

# Software design

`StreamCatTools` is designed as a lightweight wrapper around the StreamCat and LakeCat REST services. Requests are issued through `httr2`, with functions grouped into matching `sc_*` and `lc_*` families for stream and lake metrics. Data access functions accept metric names, spatial filters, and AOI syntax and return analysis-ready data. The package integrates with spatial workflows through `sf` and `hydrogeofetch`, and the lake-basin workflow uses DuckDB-backed geometry access for efficient retrieval.

# Applications and Discussion

`StreamCatTools` can be used to link watershed metrics with spatial analyses and ecological models. For example, users can calculate county- or state-scale summaries of land cover or hydrologic stressors and then combine those outputs with `ggplot2` or `hydrogeofetch`-based mapping:


``` r
df <- sc_get_data(metric='pctagslphigh2019', aoi='ws', county='41003') |>
  dplyr::summarise(mean_pctagslphigh2019ws = mean(pctagslphigh2019ws, na.rm=TRUE))
df
```

```
#>   mean_pctagslphigh2019ws
#> 1                4.406472
```


``` r
library(hydrogeofetch)
library(ggplot2)
library(ggspatial)
library(StreamCatTools)

start_comid = 23763517
nldi_feature <- list(featureSource = "comid", featureID = start_comid)
flowline_nldi <- hydrogeofetch::navigate_nldi(nldi_feature, mode = "UT", data_source = "flowlines", distance=5000)
df <- sc_get_data(metric='pctimp2019', aoi='cat', comid=flowline_nldi$UT_flowlines$nhdplus_comid)
flowline_nldi <- flowline_nldi$UT_flowlines
flowline_nldi$PCTIMP2019 <- df$pctimp2019cat[match(flowline_nldi$nhdplus_comid, df$comid)]
basin <- hydrogeofetch::get_nldi_basin(nldi_feature = nldi_feature)
```

Figure \ref{fig:calapooia} plots the NLCD percent imperviousness for the local drainage mapped to each stream reach and to the overall basin boundary.

\begin{figure}

{\centering \includegraphics[width=0.95\linewidth]{paper_files/figure-latex/calapooia-1} 

}

\caption{Map of NLCD percent imperviousness for each catchment for the Calapooia River watershed in Oregon.}\label{fig:calapooia}
\end{figure}

Watersheds for lakes can also be retrieved using `lc_get_watershed()` to visualize lake-basin land-cover composition as in Figure \ref{fig:landcover}.


``` r
library(ggplot2)
library(patchwork)
library(ggforce)

df <- lc_get_nlcd(comid='19334077', year='2019', aoi='ws')
lake <- hydrogeofetch::get_waterbodies(id = 19334077)
ws <- lc_get_watershed(comid = 19334077, huc2 = "01",huc2_filter = "01", 
                      threads = 2,retries = 5, verbose=FALSE, progress=FALSE)
```

\begin{figure}

{\centering \includegraphics[width=0.95\linewidth]{paper_files/figure-latex/landcover-1} 

}

\caption{NLCD land cover proportions with an example lake watershed.}\label{fig:landcover}
\end{figure}

Functions for plotting NNI metrics and time-series exploration for nitrogen and phosphorus budgets are also included [@MarkleyNNI], enabling \ref{fig:NNI}.


``` r
library(StreamCatTools)
library(ggplot2)
library(ggpattern)
com <- '22812041'
sc_plotnni(comid = com, include.nue = TRUE)
```

\begin{figure}

{\centering \includegraphics[width=0.95\linewidth]{paper_files/figure-latex/NNI-1} 

}

\caption{Annual time series of nitrogen and phosphorus budget data for the Mississippi-Atchafalaya River Basin.}\label{fig:NNI}
\end{figure}

Future development includes plotting functions and additional metrics. A complementary package, `StreamCatR` is also being developed to allow users to process their own landscape metrics to hydrologic frameworks with network topology. This supports a broader objective of making national-scale watershed data more accessible and reproducible for management and research applications. 

# Research impact

Providing metric discovery, batched retrieval, and spatial integration in a single R interface, `StreamCatTools` facilitates use of this valuable CONUS scale data in monitoring, ecological assessment, and management decisions. `StreamCatTools` supports Clean Water Act authorities in state integrated reporting (i.e. Oregon Department of Environmental Quality [@oregondeq_ir26tsd] and Alabama Department of Environmental Management [@adem_wqmonitoringstrategy_2025]), as well as species-distribution and aquatic-connectivity studies.


# Acknowledgements

Examples of using StreamCat and LakeCat make extensive use of `hydrogeofetch` [@blodgett2026hydrogeofetch] and the functions for accessing the API are facilitated through use of `httr2` [@wickham2025httr2]. Figures were created using `ggplot2` [@wickham2016ggplot2]. 

We sincerely thank Michael Dumelle and Jeff Hollister for helpful comments which improved this manuscript. The views expressed are those of the authors and do not necessarily represent the views or policies of the U.S. Environmental Protection Agency.

# AI Usage Disclosure

The R code for this R package was initially developed by Marc Weber and minimal generative AI was used for the most recent version of the package where several functions were refactored.

# References

