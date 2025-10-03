# StreamCatTools (development version)

-   Added code coverage to the package
-   Added two new functions - `sc_get_metric_names` and `lc_get_metric_names` 
    to make metric names and descriptions more searchable and slightly updated 
    parameters in both `sc_get_params` and `lc_get_params` functions

# StreamCatTools 0.7.0

-   Updated both `sc_get_data` and `lc_get_data` to pass parameters in
    POST request body rather than header, providing ability to pass lengthy
    lists of COMIDS to those requests
-   Fixed a bug in package tests that was causing an error when 
    running `testthat`    
    
# StreamCatTools 0.6.0

-   Added functionality in `sc_get_data` and `lc_get_data` to accommodate 
    large numbers (e.g. > 700) COMIDs in header for POST request

# StreamCatTools 0.5.0

-   Overhauled `sc_get_data` and `lc_get_data` functions to be able to pull 
    all metrics from StreamCat or LakeCat for an area of interest
-   Updated `sc_get_data` and `lc_get_data` to be able to pass hundreds of 
    COMIDs at a time as a function parameter and not error out in call to server
-   Updated `sc_get_params` and `lc_get_params` to now return variable information,
    as well as just metric names and full names, and return tables of state and 
    county abbreviations, names and FIPS codes


# StreamCatTools 0.4.0

-   Overhauled all functions to use the new [api.epa.gov](https://api.epa.gov/StreamCat/streams/metrics) 
    endpoints for StreamCat described in [swagger documentation](https://usepa.github.io/StreamCatWebServices_Public/#/)
-   Streamlined sc_get_data and lc_get_data requests with `httr2` 
-   Adopted functionality in all package functions to use the Oracle ORDs Database 
    REST API rather than previous JAVA REST service

# StreamCatTools 0.3.0

-   Incorporated POST requests in `httr2` requests to deal with many COMIDs in requests
-   Added documentation to vignette for working with LakeCat data in the package

# StreamCatTools 0.2.0

-   Added LakeCat functions to the package:
    - `lc_get_data`
    - `lc_get_params`
-   Changed lifecycle to `stable`

# StreamCatTools 0.1.1.9002

-   Fixed `sc_get_data` to pass COMIDs to function more robustly
-   Added `sc_nlcd` helper function to more easily grab NLCD based metrics from StreamCat
-   Updated package documentation

# StreamCatTools 0.1.1.9001

-   API is publicly released and the package works now for everyone (not just those 
    inside EPA network)

# StreamCatTools 0.1.1.9000

-   Added a `NEWS.md` file to track changes to the package.
