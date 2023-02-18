write_data_extract <- function(raw_data, cleaning_log_path) {
  wb <- openxlsx::loadWorkbook(cleaning_log_path)

  data_extract <- dplyr::select(raw_data, dplyr::all_of(c("_uuid", "q_name")))

  openxlsx::writeData(wb,
    sheet = "01_data_extract",
    x = data_extract,
    startRow = 2,
    colNames = FALSE
  )

  openxlsx::saveWorkbook(wb, here::here("out_file.xlsx"))
}
