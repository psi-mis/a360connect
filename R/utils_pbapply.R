#' Adapt lapply function
#'
#' Adapt lapply function to show progress bar, and provide a default fallback if
#' pbapply is not loaded.
#'
#' @noRd
pb_lapply <- function(x, fun, ...) {
  if (requireNamespace("pbapply", quietly = TRUE)) {
    pbapply::pblapply(x, fun, ...)
  } else {
    lapply(x, fun, ...)
  }
}
