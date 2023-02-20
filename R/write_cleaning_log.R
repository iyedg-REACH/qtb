write_cleaning_log <- function(wb, raw_data,
                               clean_data,
                               base_path) {
  openxlsx::addWorksheet(wb, sheetName = "Clean Data")
  openxlsx::writeDataTable(wb, sheet = "Clean Data", x = clean_data)

  openxlsx::addWorksheet(wb, sheetName = "Raw Data")
  openxlsx::writeDataTable(wb, sheet = "Raw Data", x = raw_data)

  openxlsx::saveWorkbook(wb, fs::path(fs::path_abs(base_path), "my_output.xlsx"))

}
