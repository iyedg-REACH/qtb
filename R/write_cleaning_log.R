#' Export a Cleaning Log of Augmented data
#'
#' @param wb Workbook of the Template Cleaning Log
#' @param raw_data Data Frame of the Raw JMMI Survey Data
#' @param clean_data Data Frame of the Augmented Raw Data
#' @param data_extract Data Frame of the Augmented Data Extract
#' @param logbook  Data Frame of the Augmented Logbook
#' @param summary  Data Frame of the Augmented Summary
#' @param base_path Path to the export directory
#'
#' @return
#' @export
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

  actions <- list(
    "Recoded" = openxlsx::createStyle(bgFill = "#FFFF00"),
    "Translated" = openxlsx::createStyle(bgFill = "#00B050"),
    "Checked" = openxlsx::createStyle(bgFill = "#538DD5"),
    "Corrected" = openxlsx::createStyle(bgFill = "#1F497D", fontColour = "#FFFFFF"),
    "Added" = openxlsx::createStyle(bgFill = "#7030A0", fontColour = "#FFFFFF"),
    "Removed" = openxlsx::createStyle(bgFill = "#808080", fontColour = "#FFFFFF")
  )


  openxlsx::conditionalFormatting(wb, "00_Summary",
    cols = 2,
    rows = 12:1000,
    rule = "==\"Recoded\"", style = actions$Recoded
  )


  openxlsx::conditionalFormatting(wb, "00_Summary",
    cols = 2,
    rows = 12:1000,
    rule = "==\"Translated\"", style = actions$Translated
  )


  openxlsx::conditionalFormatting(wb, "00_Summary",
    cols = 2,
    rows = 12:1000,
    rule = "==\"Checked\"", style = actions$Checked
  )

  openxlsx::conditionalFormatting(wb, "00_Summary",
    cols = 2,
    rows = 12:1000,
    rule = "==\"Corrected\"", style = actions$Corrected
  )


  openxlsx::conditionalFormatting(wb, "00_Summary",
    cols = 2,
    rows = 12:1000,
    rule = "==\"Added\"", style = actions$Added
  )

  openxlsx::conditionalFormatting(wb, "00_Summary",
    cols = 2,
    rows = 12:1000,
    rule = "==\"Removed\"", style = actions$Removed
  )

  openxlsx::dataValidation(wb, "00_Summary",
    col = 2,
    rows = 12:1000,
    type = "list",
    value = "'00_Summary'!$B$3:$B$8"
  )

  openxlsx::writeData(wb,
    sheet = "00_Summary",
    x = summary,
    startRow = 12,
    colNames = FALSE
  )

  openxlsx::saveWorkbook(wb, fs::path(fs::path_abs(base_path), "my_output.xlsx"), overwrite = T)
}
