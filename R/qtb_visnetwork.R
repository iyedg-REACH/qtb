#' visNetwork dependency graph for `period`
#'
#' Visualise the pipeline dependency graph for the specified data collection period
#'
#' @param period a string period id
#' @param ... arguments passed down to \link[targets]{tar_visnetwork}
#'
#' @return A visNetwork HTML widget object
#' @export
qtb_visnetwork <- function(period, ...) {
  targets::tar_visnetwork(targets_only = T, names = targets::contains(period), ...)
}
