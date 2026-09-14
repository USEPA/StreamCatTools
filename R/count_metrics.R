# count_metrics.R
# Deduplicate the StreamCat / LakeCat "over 600 / over 300" headline figures into:
#   (1) distinct conceptual indicators   -> unique base metric names
#   (2) AOI expansion                     -> Cat / Ws / (RipBuf, Cat/WsRp100 etc.)
#   (3) year expansion                    -> NLCD vintages and other multi-year layers
# so you can see how a raw column count inflates past the number of distinct variables.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
})

# ---- helper: turn a variable_info tibble into the three counts -------------
summarize_metrics <- function(vi, label) {
  if (!"metric" %in% names(vi)) {
    stop("`vi` must contain a `metric` column.", call. = FALSE)
  }

  vi <- vi
  aoi <- if ("aoi" %in% names(vi)) vi$aoi else rep("NA", nrow(vi))
  year <- if ("year" %in% names(vi)) vi$year else rep("NA", nrow(vi))

  vi <- vi |>
    mutate(
      metric = tolower(str_trim(metric)),
      aoi    = ifelse(is.na(aoi) | aoi == "", "NA", aoi),
      year   = ifelse(is.na(year) | year == "", "NA", year)
    )

  # collapse to one row per base metric, unioning aoi + year tokens across rows
  per_metric <- vi |>
    group_by(metric) |>
    summarise(
      aoi_tokens  = list(sort(unique(str_split(paste(aoi,  collapse = ","), "\\s*,\\s*")[[1]]))),
      year_tokens = list(sort(unique(str_split(paste(year, collapse = ","), "\\s*,\\s*")[[1]]))),
      .groups = "drop"
    ) |>
    mutate(
      n_aoi     = lengths(aoi_tokens),
      n_year    = pmax(lengths(year_tokens), 1L),   # NA-only -> counts as 1
      n_columns = n_aoi * n_year                     # physical columns for this metric
    )

  cat(sprintf("\n==== %s ====\n", label))
  cat(sprintf("Distinct conceptual indicators (base metric names): %d\n",
              nrow(per_metric)))
  cat(sprintf("Metrics offered in >1 AOI:                          %d\n",
              sum(per_metric$n_aoi > 1)))
  cat(sprintf("Metrics with multiple years:                        %d\n",
              sum(per_metric$n_year > 1)))
  cat(sprintf("Fully-expanded physical columns (metric x AOI x yr): %d\n",
              sum(per_metric$n_columns)))
  invisible(per_metric)
}

# ---- Option A: use your own package (idiomatic) ---------------------------
# library(StreamCatTools)
# sc_vi <- sc_get_params(param = "variable_info")   # StreamCat metadata tibble
# lc_vi <- lc_get_params(param = "variable_info")   # LakeCat metadata tibble
# summarize_metrics(sc_vi, "StreamCat")
# summarize_metrics(lc_vi, "LakeCat")

# ---- Option B: standalone, no package dependency (clean-room check) --------
if (sys.nframe() == 0L) {
  library(jsonlite)

  fetch_vi <- function(base) {
    # variable_info is exposed through the metrics endpoint's parameter listing;
    # the R package pulls it here. Adjust the query if the schema shifts.
    url <- paste0(base, "?variable_info=variable_info")
    fromJSON(url)$items
  }

  sc_vi <- fetch_vi("https://api.epa.gov/StreamCat/streams/metrics")
  lc_vi <- fetch_vi("https://api.epa.gov/StreamCat/lakes/metrics")

  sc <- summarize_metrics(sc_vi, "StreamCat")
  lc <- summarize_metrics(lc_vi, "LakeCat")

  # ---- LakeCat overlap check: how many LakeCat metrics are borrowed from StreamCat
  # The LakeCat tables carry an `inStreamCat` flag per metric. If present in vi,
  # this shows how much of LakeCat's total is NOT independent of StreamCat:
  if ("instreamcat" %in% tolower(names(lc_vi))) {
    flag <- lc_vi[[which(tolower(names(lc_vi)) == "instreamcat")]]
    cat(sprintf("\nLakeCat metrics flagged inStreamCat (shared w/ StreamCat): %d of %d rows\n",
                sum(flag %in% c(1, "1", TRUE, "Yes", "yes")), length(flag)))
  }

  # ---- category breakdown (Natural vs Anthropogenic vs derived/special) ------
  sc_by_cat <- sc_vi |>
    distinct(metric = tolower(metric), category) |>
    count(category, sort = TRUE, name = "n_distinct_metrics")
  print(sc_by_cat)
}
