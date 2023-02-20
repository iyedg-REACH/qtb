augment_data_extract <- function(raw_data, data_extract) {
  gen_data_extract <- dplyr::select(raw_data, all_of(c("_uuid", "q_name"))) |>
    dplyr::rename(
      uuid = `_uuid`,
      `enumerator ID` = q_name
    )

  return(gen_data_extract)
}
