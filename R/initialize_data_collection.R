init_data_collection <- function(period, base_path) {
  qtb:::validate_period_label(period)

  data_collection_path <- fs::path(base_path, as.character(period))

  cleaning_log_template_name <- "Research_cycle_data_cleaning_logbook_template_v3_202208_FINAL.xlsx"

  cleaning_log_template_path <- system.file("extdata",
    cleaning_log_template_name,
    package = "qtb"
  )

  if (!dir.exists(data_collection_path)) {
    dir.create(data_collection_path)
    cli::cli_alert_info("Creating the base folder {data_collection_path}")
    file.copy(cleaning_log_template_path, data_collection_path)
    file.rename(
      fs::path(data_collection_path, cleaning_log_template_name),
      fs::path(data_collection_path, paste0(period, "_cleaning_log.xlsx"))
    )
  } else {
    cli::cli_alert_danger("{data_collection_path} already exists !")
  }
}
