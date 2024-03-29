#' Generate a Data Extract from Raw Data
#'
#' @param raw_data Data Frame of the Raw JMMI Survey Data
#'
#' @return Data Frame with two columns for the submission UUIDs and Enumerator IDs
#' @export
augment_data_extract <- function(raw_data) {
  data_extract <- select(
    raw_data,
    all_of(c("_uuid", "enumerator_id"))
  ) |>
    dplyr::rename(
      uuid = .data[["_uuid"]],
      `enumerator ID` = .data[["enumerator_id"]]
    )

  return(data_extract)
}
