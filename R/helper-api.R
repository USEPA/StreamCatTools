skip_if_api_unavailable <- function(x, label = "API") {
  if (is.null(x) ||
      (is.data.frame(x) && nrow(x) == 0L) ||
      (is.list(x) && length(x) == 0L) ||
      (is.character(x) && length(x) == 0L)) {
    testthat::skip(paste(label, "is unavailable; upstream service returned no data."))
  }
  invisible(x)
}