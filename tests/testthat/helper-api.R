skip_if_api_unavailable <- function(x, label = "API") {
  if (is.null(x) ||
      (is.data.frame(x) && nrow(x) == 0L) ||
      (is.list(x) && length(x) == 0L) ||
      (is.character(x) &&
       (length(x) == 0L || identical(x, "the service appears to be down currently")))) {
    testthat::skip(paste(label, "is unavailable; upstream service returned no data."))
  }
  invisible(x)
}
