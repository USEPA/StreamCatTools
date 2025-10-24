This is a patch update that:

- Handles a web service being down in testing using `testthat::skip_on_cran()`
- Handles a web service being down in vignettes by converting most vignettes to
  articles and leaving just one `Start Here` vignette
- Due to the lapse in government funding in federal agencies in the US, 
  the web service StreamCatTools is built around may be down for some time
- Because of the lapse in government funding in federal agencies in the US, I
  may not be able to respond readily to any CRAN emails to my government 
  email account weber.marc@epa.gov but my personal email is mweber36@gmail.com
  

-------

## Resubmission

This is a resubmission. 

## R CMD check results

Here is the output from `devtools::check()` on R Version R version 4.5.0,
devtools version 2.4.6, and Windows 11 x64 operating system

Duration: 2m 20.4s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔
