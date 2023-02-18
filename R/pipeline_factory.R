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

  list(
    tar_target_raw(
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
    ),
    tar_target_raw(
      name = paste0(base_target_name, "_cleaning_log_path"),
      command = substitute(fs::path(
        base_path,
        paste0(base_target_name, "_cleaning_log.xlsx")
      )),
      format = "file"
    ),
    tar_target_raw(
      name = paste0(base_target_name, "_data"),
      command = substitute(
        readxl::read_excel(path, guess_max = 10000),
        env = list(
          path = as.symbol(paste0(base_target_name, "_raw"))
        )
      )
    ),
    tar_target_raw(
      name = paste0(base_target_name, "_data_extract"),
      command = substitute(
        write_data_extract(raw_data, cleaning_log_path),
        env = list(
          raw_data = as.symbol(paste0(base_target_name, "_data")),
          cleaning_log_path = as.symbol(paste0(base_target_name, "_cleaning_log_path"))
        )
      ),
      format = "file"
    )
  )
}
