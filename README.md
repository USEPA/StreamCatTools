# StreamCatTools

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-blue.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/USEPA/StreamCatTools/workflows/R-CMD-check/badge.svg)](https://github.com/USEPA/StreamCatTools/actions)



`StreamCatTools` is a draft R package for accessing StreamCat data via the StreamCat API and for working with site data in conjunction with StreamCat and NHDPlus. The StreamCat API is currently for users on the EPA network so the functions in the package will not currently work for those outside the EPA. `StreamCatTools` is alpha development - this is a work in progress, look for a working package release soon.

## Installation

You can install and load the most recent approved version from GitHub by running:

```r
library(remotes)
install_github("USEPA/StreamCatTools", build_vignettes=TRUE)
```

To view the vignette in RStudio, run
```r
vignette("Introduction", "StreamCatTools")
```

## Disclaimer
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use.  EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information.  Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA.  The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.