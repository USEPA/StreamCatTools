This is a patch update that:

- Handles a web service being down in testing using `testthat::skip_on_cran()`
- Handles a web service being down in vignettes
- Due to the lapse in government funding in federal agencies in the US, 
  the web service StreamCatTools is built around may be down for some time

-------

## Resubmission

This is a resubmission. 

## R CMD check results

Here is the output from `devtools::check()` on R Version 4.4.1,
devtools version 2.4.6, and Windows 11 x64 operating system

0 errors ✔ | 0 warnings ✔ | 2 notes ✖

R CMD check succeeded

NOTES:

checking installed package size ... NOTE
    installed size is  5.0Mb
    sub-directories of 1Mb or more:
      doc   4.4Mb

checking for future file timestamps ... NOTE
  unable to verify current time
