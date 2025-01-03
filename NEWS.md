# StreamCatTools 0.4.0

-   Overhauled all functions to use the new [epa.api.gov](https://api.epa.gov/StreamCat) endpoints for StreamCat described in [swagger documentation](https://usepa.github.io/StreamCatWebServices_Public/#/)
-   Streamlined sc_get_data and lc_get_data requests with `httr2` 
-   Adopted functionality in all package functions to use the Oracle ORDs Database REST API rather than previous JAVA REST service

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

-   API is publicly released and the package works now for everyone (not just those inside EPA network)

# StreamCatTools 0.1.1.9000

-   Added a `NEWS.md` file to track changes to the package.
