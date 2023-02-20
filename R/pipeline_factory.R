validate_period_label <- function(period) {
  regex <- r"(JMMI_\d{4}_\d{2})"
  assertthat::assert_that(
    assertthat::are_equal(
      stringr::str_extract(period, regex),
      period
    ),
    msg = glue::glue("{period} does not match the format 'JMMI_YEAR_MONTH'")
  )
}

pipeline <- function(period,
                     kobo_username = Sys.getenv("KOBO_USERNAME"),
                     kobo_password = Sys.getenv("KOBO_PASSWORD"),
                     uid,
                     base_path,
                     raw_data_age = as.difftime(8, units = "hours")) {
  base_target_name <- toupper(deparse(substitute(period)))
  validate_period_label(base_target_name)

  tar_name_raw <- paste0(base_target_name, "_raw")
  tar_name_cleaning_log_path <- paste0(base_target_name, "_cleaning_log_path")
  tar_name_data <- paste0(base_target_name, "_data")
  tar_name_report <- paste0(base_target_name, "_report")

  tar_name_cleanin_log_wb <- paste0(base_target_name, "_wb")
  tar_name_summary <- paste0(base_target_name, "_summary")
  tar_name_data_extract <- paste0(base_target_name, "_data_extract")
  tar_name_logbook <- paste0(base_target_name, "_logbook")
  tar_name_deletion_log <- paste0(base_target_name, "_deletion_log")


  tar_name_augmented_summary <- paste0(base_target_name, "_augmented_summary")
  tar_name_augmented_data_extract <- paste0(base_target_name, "_augmented_data_extract")
  tar_name_augmented_logbook <- paste0(base_target_name, "_augmented_logbook")
  tar_name_augmented_deletion_log <- paste0(base_target_name, "_augmented_deletion_log")

  augmented_targets <- list(
    tar_target_raw(
      name = tar_name_augmented_summary,
      command = substitute(
        summary,
        env = list(summary = as.symbol(tar_name_summary))
      )
    ),
    tar_target_raw(
      name = tar_name_augmented_data_extract,
      command = substitute(
        data_extract,
        env = list(data_extract = as.symbol(tar_name_data_extract))
      )
    ),
    tar_target_raw(
      name = tar_name_augmented_logbook,
      command = substitute(
        logbook,
        env = list(logbook = as.symbol(tar_name_logbook))
      )
    ),
    tar_target_raw(
      name = tar_name_augmented_deletion_log,
      command = substitute(
        deletion_log,
        env = list(deletion_log = as.symbol(tar_name_deletion_log))
      )
    )
  )

  sheets_targets <- list(
    tar_target_raw(
      name = tar_name_summary,
      command = substitute(readxl::read_excel(
        path,
        sheet = "00_Summary",
        skip = 10,
        na = c("", "NA")
      ), env = list(path = as.symbol(tar_name_cleaning_log_path)))
    ),
    tar_target_raw(
      name = tar_name_data_extract,
      command = substitute(readxl::read_excel(
        path,
        sheet = "01_data_extract",
        na = c("", "NA")
      ), env = list(path = as.symbol(tar_name_cleaning_log_path)))
    ),
    tar_target_raw(
      name = tar_name_logbook,
      command = substitute(readxl::read_excel(
        path,
        sheet = "02_Logbook",
        na = c("", "NA")
      ), env = list(path = as.symbol(tar_name_cleaning_log_path)))
    ),
    tar_target_raw(
      name = tar_name_deletion_log,
      command = substitute(readxl::read_excel(
        path,
        sheet = "03_deletion log",
        na = c("", "NA")
      ), env = list(path = as.symbol(tar_name_cleaning_log_path)))
    )
  )

  base_path <- fs::path_abs(base_path)
  cli::cli_alert_info(
    here::here(base_path, "report/report.qmd")
  )

  kobo_target <- tar_target_raw(
    name = paste0(base_target_name, "_raw"),
    command = substitute(
      quiveR::kobo_download_dataset(
        asset_uid = uid,
        user_name = user_name,
        user_password = user_password,
        file_path = fs::path(base_path, paste0(base_target_name, ".xlsx")),
        lang = "xls"
      ),
      env = list(
        uid = uid,
        user_name = kobo_username,
        user_password = kobo_password,
        base_path = base_path,
        base_target_name = base_target_name
      )
    ),
    cue = tarchetypes::tar_cue_age_raw(
      paste0(base_target_name, "_raw"),
      age = raw_data_age
    ),
    format = "file"
  )

  list(
    tar_target_raw(
      name = tar_name_raw,
      command = substitute(
        fs::path(dir_path, paste0(base_target_name, ".xlsx")),
        env = list(dir_path = base_path, base_target_name = base_target_name)
      ),
      format = "file"
    ),
    tar_target_raw(
      name = tar_name_cleaning_log_path,
      command = substitute(fs::path(
        base_path,
        paste0(base_target_name, "_cleaning_log.xlsx")
      )),
      format = "file"
    ),
    tar_target_raw(
      name = tar_name_data,
      command = substitute(
        readxl::read_excel(path, guess_max = 10000),
        env = list(
          path = as.symbol(tar_name_raw)
        )
      )
    ),
    # tar_target_raw(
    #   name = tar_name_data_extract,
    #   command = substitute(
    #     write_data_extract(raw_data, cleaning_log_path),
    #     env = list(
    #       raw_data = as.symbol(tar_name_data),
    #       cleaning_log_path = as.symbol(tar_name_cleaning_log_path)
    #     )
    #   )
    # ),
    tar_target_raw(
      name = tar_name_cleanin_log_wb,
      command = substitute(
        openxlsx::loadWorkbook(path),
        env = list(path = as.symbol(tar_name_cleaning_log_path))
      )
    ),
    tar_target_raw(
      name = "tar_complete_logbook",
      command = substitute(
        complete_logbook(raw_data, logbook),
        env = list(raw_data = as.symbol(tar_name_data), logbook = as.symbol(tar_name_logbook))
      )
    ),
    sheets_targets,
    augmented_targets
  )
}
