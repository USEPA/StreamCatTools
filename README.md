# StreamCatTools <img src="man/figures/logo.png" align="right" alt="" width="150" />


[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/USEPA/StreamCatTools/workflows/R-CMD-check/badge.svg)](https://github.com/USEPA/StreamCatTools/actions)


## StreamCatTools: Tools to work with the [StreamCat](https://www.epa.gov/national-aquatic-resource-surveys/streamcat-dataset) API within R and access the full suite of StreamCat catchment and waterhsed scale metrics for all NHDPlusV2 stream reaches and catchments.


`StreamCatTools` is an R package for accessing StreamCat data via the StreamCat API and for working with site data in conjunction with StreamCat and NHDPlus. `StreamCatTools` is in development but currently contains a core set of functions for working with the StreamCat API in R.

### Installation

You can install and load the most recent approved version from GitHub by running:

```r
library(remotes)
install_github("USEPA/StreamCatTools", build_vignettes=TRUE)
```

To view the vignette in RStudio, run
```r
vignette("Introduction", "StreamCatTools")
```
### Contributing
Contributions to development of the package are welcome and encouraged. Please consider the following guidlelines for contributing to `StreamCatTools`:

- We encourage package users to submit bugs and enhancement requests by submitting issues - see the [Quickstart for GitHub Issues](https://docs.github.com/en/issues/tracking-your-work-with-issues/quickstart) page if you are new to GitHub issues
- Contributions should use the standard GitHub [fork - pull-request workflow](https://gist.github.com/Chaser324/ce0505fbed06b947d962)
- We encourage use of the [tidyverse style guide](https://style.tidyverse.org/) for contributions
- Contributions should ideally be tested using [testthat](https://testthat.r-lib.org/)
- If you contribute documentation the package uses pkgdown and running `pkgdown::build_site()` will refresh the pkgdown pages for the package

### Python Implementation of StreamCat API
[PyNHD](https://github.com/hyriver/pynhd), part of the [HyRiver](https://github.com/hyriver) suite of Python packages, also provides access to StreamCat data via the API in Python, along with other NHDPlus value-added attributes for catchments and catchment and network accumulated values for catchments available via [USGS ScienceBase](https://sciencebase.usgs.gov/)

### Disclaimer
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use.  EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information.  Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA.  The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.
