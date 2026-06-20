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
  - name: Selia Markley
    orcid: 
    affiliation: 4
  - name: Travis Hudson
    orcid:  
    affiliation: 3
  - name: Alan Brookes
    orcid: 
    affiliation: 2
  
affiliations:
 - name: Office of Water, United States Environmental Protection Agency
   index: 1
 - name: United States Environmental Protection Agency Retired
   index: 2
 - name: Oak Ridge Associated Universities Student Services Contractor c/o United States Environmental Protection Agency
   index: 3
 - name: Oak Ridge Institude for Science and Education Fellow c/o United States Environmental Protection Agency
   index: 4

citation_author: Weber et. al.
date: 25 August 2026
year: 2026
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

Easily accessible, robust, and consistent watershed data is an underpinning of hydrology research, water quality monitoring programs, and predictive modelling applications, to name just a few examples.  The StreamCat [@hill2016streamcat] and LakeCat [@hill2018lakecat] datasets fill this need by providing nationally consistent curated watershed data for CONUS that has had stringent quality control applied. The data encompasses hundreds of watershed metrics for every stream reach and lake feature represented in the National Hydrography Dataset Plus Version 2.1 (NHDPlusV21) [@mckay2012nhdplus]. StreamCatTools fills the need for easily accessible watershed metrics for CONUS by: (1) providing a simple interface in the R programming language to the StreamCat and LakeCat web services, (2) providing convenient functionality to find available StreamCat and LakeCat metric names and information about variables, and (3) extracting StreamCat and LakeCat metrics by COMID (a unique identifier in the NHDPlusV2 framework), by state, by county, by NHD Hydro-region, or for all of CONUS. Providing this valuable watershed data via web services in R follows the FAIR principles laid out in [@wilkinson2016fair].

# Package Overview

`StreamCatTools` provides a simple streamlined set of functions to easily query and ingest watershed landscape metrics into an R session. Figure \@ref(fig:flowchart)<!-- Text may need to be larger for journal pub --> shows the overall framework of the StreamCat database, application programming interface, and functionality in the package that simplifies data access in R using web services for the StreamCat and LakeCat datasets. 

<!-- Spell out API in figure caption -->

\begin{figure}

{\centering \includegraphics[width=0.9\linewidth]{Flowchart} 

}

\caption{Diagram of the StreamCat and StreamCatTools framework. A backend Oracle database and web service are exposed through api.gov to functions in StreamCatTools which simplify accesss and analysis of the data in R via the application programming interface.}(\#fig:flowchart)
\end{figure}

The core functions in `StreamCatTools` leverage the `httr2` library [@wickham2025httr2] for a modern, pipeable method for working with APIs. Specifically, `StreamCatTools` simplifies the calls to the API for StreamCat and LakeCat data within the R programming language to allow a user to: (1) access details on available StreamCat and LakeCat metrics, (2) match users' sites to NHDPlusV21 waterbodies (streams or lakes) to access StreamCat and Lakecat data, and (3) retrieve StreamCat and LakeCat watershed metrics by COMID, by state, by county, by hydroregion, or for all of CONUS.  Additionally, several convenience functions retrieve data from particular years for data with time series, such as the National Land Cover Database (NLCD) [@usgsnlcd] <!-- citation looks odd in PDF --> and the National Nutrient Inventory (NNI) [@nutinventory]. Lastly, there is plotting functionality for use with NNI metrics as well as a function to retrieve the watersheds for specific lake features as an `sf` object.  Users can likewise access watersheds for specific COMIDs using the `nhdplusTools` package for visualization of and plotting of watersheds to go along with StreamCat or LakeCat metrics.

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

`StreamCatTools` includes several functions to facilitate accessing and working with the data. First, users can list metric names and find out more about StreamCat and LakeCat data available in StreamCat using using the `sc_get_params` and `lc_get_params` functions. The parameters `aoi` and `metric_names` provide a user with the available areas of interest for metrics and names of metrics.  The areas of interest available for all metrics in StreamCat and Lakecat are 'cat' and 'ws' (catchment and watershed) and in StreamCat some metrics are available at the 100 meter riparian buffer scale ('catrp100' and 'wsrp100') and for certain metrics the designation of 'other' needs to be used (where the metric is in-stream or derived and not part of the other listed areas of interest). Extracting this information in `StreamCatTools` looks like this:


``` r
aois <- sc_get_params(param='aoi')

name_params <- sc_get_params(param='metric_names')

print('areas of interest include: ')
```

```
## [1] "areas of interest include: "
```

``` r
cat(paste0(aois,collapse = "\n"))
```

```
## cat
## catrp100
## other
## ws
## wsrp100
```


``` r
print('A selection of available StreamCat metrics include: ')
```

```
## [1] "A selection of available StreamCat metrics include: "
```

``` r
cat(paste0(name_params[1:10],collapse = "\n"))
```

```
## agkffact
## al2o3
## bankfulldepth
## bankfullwidth
## bfi
## canaldens
## cao
## cbnf
## chem
## clay
```

And the same information for LakeCat metrics is derived in similar fashion:


``` r
aois <- lc_get_params(param='aoi')

name_params <- lc_get_params(param='metric_names')

print('areas of interest include: ')
```

```
## [1] "areas of interest include: "
```

``` r
cat(paste0(aois, collapse = "\n"))
```

```
## cat
## ws
```


``` r
print('A selection of available LakeCat metrics include: ')
```

```
## [1] "A selection of available LakeCat metrics include: "
```

``` r
cat(paste0(name_params[1:10],collapse = "\n"))
```

```
## agkffact
## al2o3
## bfi
## canaldens
## cao
## cbnf
## clay
## coalminedens
## compstrgth
## damdens
```

StreamCat and LakeCat are built around the concepts of local drainage area (i.e catchment) and watershed (i.e. the local drainage area and all upstream catchments) [@hill2016streamcat].  This approach uses the NHDPlusV21 hydrographic framework of catchments as the building block for summarizing landscape information represented in landscape data and then aggregating both the catchment landscape summary and all upstream catchments using the weighted average of metrics in most cases, but using a sum or count for certain metrics. `sc_get_params` and `lc_get_params` also include a 'variable_info' parameter to return more detailed metadata for metrics including both short and long metric descriptions, years available (if applicable), units, and the metric category. 


``` r
var_info <- sc_get_params(param='variable_info')

my_data <- head(var_info[c(5,10,29,33,45,102),c('metric','short_description','year','units')])

knitr::kable(
  my_data,
  format = "latex",
  booktabs = TRUE,
  longtable = TRUE,
  col.names = c('Metric', 'Short Description', 'Year', 'Units'),
  align = "llll"
)  |> 
  kableExtra::kable_styling(
    latex_options = c("repeat_header"),
    font_size = 7
  )  |> 
  kableExtra::column_spec(1, width = "1.0in")  |> 
  kableExtra::column_spec(2, width = "1.0in")  |> 
  kableExtra::column_spec(3, width = "1.0in")  |> 
  kableExtra::column_spec(4, width = "1.0in")
```

\begingroup\fontsize{7}{9}\selectfont

\begin{longtable}{>{\raggedright\arraybackslash}p{1.0in}>{\raggedright\arraybackslash}p{1.0in}>{\raggedright\arraybackslash}p{1.0in}>{\raggedright\arraybackslash}p{1.0in}}
\toprule
Metric & Short Description & Year & Units\\
\midrule
\endfirsthead
\multicolumn{4}{@{}l}{\textit{(continued)}}\\
\toprule
Metric & Short Description & Year & Units\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
agkffact[AOI] & Ag Soil Erodibility Kf Factor & NA & Unitless\\
bfi[AOI] & Base Flow Index & NA & Percent\\
huden[Year][AOI] & Mean Housing Density & 2010 & Count/Square Kilometer\\
inorgnwetdep[Year][AOI] & Mean Annual Precipitation-Weighted Nitrogen & 2008 & Kilogram/Hectare/Year\\
n\_ags\_[Year][AOI] & Nitrogen Agricultural Surplus & 1987-2017 & Kilograms\\
\addlinespace
pcthighsev[Year][AOI] & Percent High Burn Severity Class For Year & 1984-2018 & Percent\\*
\end{longtable}
\endgroup{}

Additional functions provided for getting metadata about the underlying StreamCat and LakeCat data include the `sc_fullname` and `lc_fullname` functions to provide the descriptive full name for any given metric in StreamCat or LakeCat:


``` r
fullname <- sc_fullname(metric='pctgrs2019')
fullname
```

```
## [1] "Grassland/Herbaceous Percentage 2019"
```

Users can also filter metric names and information by the metric year(s), the indicator categories for metrics, the metric dataset names, or the areas of interest available for a given metric using the `sc_get_metric_names` or `lc_get_metric_names` functions:


``` r
metrics <- sc_get_metric_names(category = c('Deposition','Climate'),
                               aoi=c('Cat','Ws'))
my_data <- head(metrics[,c('Category','Metric','AOI')],10)
knitr::kable(
  my_data,
  format = "latex",
  booktabs = TRUE,
  longtable = TRUE,
  col.names = c('Category','Metric','AOI'),
  align = "lll"
)  |> 
  kableExtra::kable_styling(
    latex_options = c("repeat_header"),
    font_size = 7
  )  |> 
  kableExtra::column_spec(1, width = "1.2in") |>  
  kableExtra::column_spec(2, width = "1.4in") |>   
  kableExtra::column_spec(3, width = "1.0in")   
```

\begingroup\fontsize{7}{9}\selectfont

\begin{longtable}{>{\raggedright\arraybackslash}p{1.2in}>{\raggedright\arraybackslash}p{1.4in}>{\raggedright\arraybackslash}p{1.0in}}
\toprule
Category & Metric & AOI\\
\midrule
\endfirsthead
\multicolumn{3}{@{}l}{\textit{(continued)}}\\
\toprule
Category & Metric & AOI\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
Climate & bfi[AOI] & Cat, Ws\\
Deposition & inorgnwetdep[Year][AOI] & Cat, Ws\\
Deposition & nh4[Year][AOI] & Cat, Ws\\
Deposition & no3[Year][AOI] & Cat, Ws\\
Climate & precip8110[AOI] & Cat, Ws\\
\addlinespace
Climate & precip9120[AOI] & Cat, Ws\\
Climate & precip[Year][AOI] & Cat, Ws\\
Deposition & sn[Year][AOI] & Cat, Ws\\
Climate & tmax8110[AOI] & Cat, Ws\\
Climate & tmax9120[AOI] & Cat, Ws\\*
\end{longtable}
\endgroup{}

More details on these functions can be found at the [package introduction page](https://usepa.github.io/StreamCatTools/articles/Articles/Introduction.html). 

The primary package functionality is in the `sc_get_data` and `lc_get_data` functions which allow users to extract catchment or watershed metrics of interest by providing NHDPlusV21 COMIDs for streams or lakes within the database. Users can also request data for a given state(s), county(ies), hydroregion(s), or all of CONUS.  Additionally, convenience functions are provided for accessing the NLCD and NNI datasets, respectively, using `sc_nlcd` and `lc_nlcd` and `sc_nni` and `lc_nni`. 

The following example shows a request for two catchment and watershed metrics for three stream reaches:


``` r
df <- sc_get_data(metric='pcturbmd2019,damdens',
                  aoi='cat,ws', 
                  comid='179,1337,1337420')
```

The first line requests percent of NLCD medium intensity developed land cover in 2019 and the density of dams. The second line specifies that the geographic area of interest (aoi) is the drainage to the local reach scale (i.e. 'cat', short for catchment) and the full watershed. Finally, the final line request this data for three NHDPlusV2 stream segments specified by their unique COMIDs.

# Applications and Discussion

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

![](JOSS_files/figure-latex/unnamed-chunk-12-1.pdf)<!-- --> 

Watersheds for lakes can also be retrieved using the `lc_get_watershed` function in order to visualize LakeCat metrics along with the plotted watersheds for lake features:


``` r
library(ggplot2)
library(patchwork)
library(ggforce)

df <- lc_get_nlcd(comid='19334077', year='2019', aoi='ws')
lake <- nhdplusTools::get_waterbodies(id = 19334077)
ws <- lc_get_watershed(comid = 19334077, huc2 = "01",huc2_filter = "01", 
                      threads = 2,retries = 5)
```

```
##   |                                                                              |                                                                      |   0%[10:28:32] duckdb version: 1.5.4
##   |                                                                              |==========                                                            |  14%  |                                                                              |====================                                                  |  29%[10:28:32] Loading and configuring httpfs ...
##   |                                                                              |==============================                                        |  43%  |                                                                              |========================================                              |  57%[10:28:32] Describing schema ...
##   |                                                                              |==================================================                    |  71%  |                                                                              |============================================================          |  86%[10:28:34] Querying COMID = 19334077 within HUC2=01
## [10:28:40] Rows returned: 1
##   |                                                                              |======================================================================| 100%
```

``` r
df <- df |>
  dplyr::mutate(
    PctConifer = pctconif2019ws,
    PctDeciduous = pctdecid2019ws,
    PctMixedForest = pctmxfst2019ws,
    PctAgriculture = pctcrop2019ws + pcthay2019ws,
    PctShrub = pctshrb2019ws,
    PctGrass = pctgrs2019ws,
    PctWetland = pcthbwet2019ws + pctwdwet2019ws,
    PctUrban = pcturbhi2019ws + pcturblo2019ws + pcturbmd2019ws + pcturbop2019ws
  ) |>
  dplyr::select(PctConifer, PctDeciduous, PctMixedForest, PctAgriculture, PctShrub, PctGrass, PctWetland, PctUrban)

landcover_df <- df |>
  tidyr::pivot_longer(
    cols = c(PctConifer, PctDeciduous, PctMixedForest, PctAgriculture,
             PctShrub, PctGrass, PctWetland, PctUrban),
    names_to = "category",
    values_to = "value"
  ) |>
  dplyr::mutate(
    category = factor(category,
      levels = c("PctConifer","PctDeciduous","PctMixedForest","PctAgriculture",
                 "PctShrub","PctGrass","PctWetland","PctUrban")
    ),
    pct_label = ifelse(value >= 5, paste0(round(value, 1), "%"), "")  # blank label if < 5%
  )

pie_colors <- c(
  PctConifer      = "#08519c",
  PctDeciduous    = "#74c476",
  PctMixedForest  = "#fec44f",
  PctAgriculture  = "#d8b365",
  PctShrub        = "#8c510a",
  PctGrass        = "#bdbdbd",
  PctWetland      = "#41b6c4",
  PctUrban        = "#de2d26"
)

# --- map ---
p_map <- ggplot() +
  geom_sf(data = ws, fill = "grey90", color = "black", linewidth = 0.6) +
  geom_sf(data = lake, fill = "#4393c3", color = "#2166ac", linewidth = 0.3) +
  theme_void() +
  labs(title = "Watershed Boundary") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# --- pie ---
p_pie <- ggplot(landcover_df, aes(x = "", y = value, fill = category)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = pct_label), position = position_stack(vjust = 0.5), size = 3, color = "black") +
  scale_fill_manual(values = pie_colors, name = "Land Cover") +
  theme_void() +
  labs(title = "Land Cover Composition") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

p_map + p_pie + plot_layout(ncol = 2, widths = c(1, 1))
```

![](JOSS_files/figure-latex/unnamed-chunk-13-1.pdf)<!-- --> 

Plotting functions to specifically plot NNI metrics in StreamCat have been developed and are also available [@MarkleyNNI] such as the following example to plot annual time series of nitrogen and phosphorus budget data for a given watershed such as the Mississippi-Atchafalaya River Basin:


``` r
library(StreamCatTools)
library(ggplot2)
library(ggpattern)
com <- '22812041'
sc_plotnni(comid = com, include.nue = TRUE)
```

```
## If the plot does not render to the plot window when calling the function either save the plot or resize the plot window
```

```
## Retrieving data for the year 2024
```

![](JOSS_files/figure-latex/unnamed-chunk-14-1.pdf)<!-- --> 

Future functionality for `StreamCatTools` includes expanding the scope of plotting functions as well as expanding the range of metrics and ease of querying these metrics.  `StreamCatTools` currently facilitates easy ingestion of StreamCat and LakeCat watershed landscape metrics into workflows in R which is of great use to state watershed planners, researchers and non-governmental organizaitons and borne out by the over 5000 package downloads and over 350 citations of the underlying StreamCat and LakeCat data served by the `StreamCatTools` package.

# Acknowledgements

Examples of using StreamCat and LakeCat make extensive use of `nhdplusTools`[@blodgett2016nhdplustools] and the functions for accessing the API are facilitated through use of `httr2`[@wickham2025httr2]. Figures were created using `ggplot2` [@wickham2016ggplot2]. 

We would like to sincerely thank the editor and reviewers for all of their helpful feedback which greatly improved both the software and the manuscript.

The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.The information in this document has been funded entirely by the United States Environmental Protection Agency (USEPA), in part through appointments to the USEPA’s Internship/Research Participation Program at the Office of Research and Development administered by the Oak Ridge Institute for Science and Education through an interagency agreement. The views expressed in this article are those of the authors and do not necessarily represent the views or policies of the USEPA. 

# References

