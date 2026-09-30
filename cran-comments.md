This is a minor update that:

-   Refactors several functions (e.g., `sc_get_data`, `lc_get_data`, and `sc_get_comids`) 
    for improved functionality and speed 

-------

## Resubmission

This is a resubmission. In this version I have

-    Fixed Possibly misspelled words in DESCRIPTION by single-quoting 
     'LakeCat' (3:45) and 'StreamCat' (3:31)
-    Fixed typo for functionality in StartHere.Rmd:44    
-    Fixed typo for guidelines in README.md:42
-    Fixed typo for metrics in lc_get_params.R:143 and sc_get_params.R:141


## R CMD check results

Here is the output from `devtools::check()` on R Version 4.6.0,
devtools version 2.5.2, and Windows 11 x64 operating system

Duration: 5m 27.5s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## revdepcheck results

We checked 231 reverse dependencies (228 from CRAN + 3 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

We saw 1 new problem
* hydrogeofetch
We failed to check 0 packages

Package maintainer notified (1 day ago) and determined not an actual issue.
