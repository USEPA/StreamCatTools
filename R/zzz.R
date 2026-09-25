# Package-wide helpers
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "aoi_tokens",
    "year_tokens",
    "n_aoi",
    "n_year",
    "metric",
    ".pt_row_internal",
    ".valid_comids",
    ".missing_COMIDs",
    ".missing_comids"
  ))
}
