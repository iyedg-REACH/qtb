augment_data_extract <- function(raw_data, data_extract) {
  gen_data_extract <- dplyr::select(raw_data, all_of(c("_uuid", "enumerator_id"))) |>
    dplyr::rename(
      uuid = `_uuid`,
      `enumerator ID` = enumerator_id
    )

  return(gen_data_extract)
}
