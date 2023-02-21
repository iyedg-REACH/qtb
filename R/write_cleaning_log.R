write_cleaning_log <- function(wb,
                               raw_data,
                               clean_data,
                               data_extract,
                               logbook,
                               summary,
                               base_path) {
  openxlsx::addWorksheet(wb, sheetName = "Clean Data")
  openxlsx::writeDataTable(wb, sheet = "Clean Data", x = clean_data)

  openxlsx::addWorksheet(wb, sheetName = "Raw Data")
  openxlsx::writeDataTable(wb, sheet = "Raw Data", x = raw_data)


  openxlsx::writeData(wb,
    sheet = "01_data_extract",
    x = data_extract,
    startRow = 2,
    colNames = FALSE
  )

  openxlsx::writeData(wb,
    sheet = "02_Logbook",
    x = logbook,
    startRow = 2,
    colNames = FALSE
  )

  openxlsx::writeData(wb,
    sheet = "00_Summary",
    x = summary,
    startRow = 12,
    colNames = FALSE
  )

  openxlsx::saveWorkbook(wb, fs::path(fs::path_abs(base_path), "my_output.xlsx"))
}
