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

#' Download Data From KoBo ToolBox
#'
#' @param period a character vector in the format "JMMI_YEAR_MONTH"
#' @param uid the uid of the form
#' @param kobo_username KoBo Username, defaults to the environment variable KOBO_USERNAME
#' @param kobo_password  KoBo Password, defaults to the environment variable KOBO_PASSWORD
#' @param base_path path where the file will be downloaded
#' @param raw_data_age the duration after which the download will be refreshed
#'
#' @return
#' @export
kobo_download_target <- function(period,
                                 uid,
                                 kobo_username = Sys.getenv("KOBO_USERNAME"),
                                 kobo_password = Sys.getenv("KOBO_PASSWORD"),
                                 base_path = here::here(),
                                 raw_data_age = as.difftime(8, units = "hours")) {
  tar_target_raw(
    name = "raw_data_path",
    command = substitute(
      quiveR::kobo_download_dataset(
        asset_uid = uid,
        user_name = user_name,
        user_password = user_password,
        file_path = fs::path(base_path, paste0(period, "_raw_data", ".xlsx")),
        lang = "xls"
      ),
      env = list(
        uid = uid,
        user_name = kobo_username,
        user_password = kobo_password,
        base_path = base_path,
        period = period
      )
    ),
    cue = tarchetypes::tar_cue_age_raw(
      "raw_data_path",
      age = raw_data_age
    ),
    format = "file"
  )
}

#' Data Cleaning Pipeline
#'
#' @param period Period Name
#' @param base_path Path to the project
#'
#' @return
#' @export

pipeline <- function(period,
                     base_path) {
  base_target_name <- toupper(deparse(substitute(period)))
  # validate_period_label(base_target_name)

  tar_name_raw <- paste0("raw_data_path")
  tar_name_cleaning_log_path <- paste0("cleaning_log_path")
  tar_name_data <- paste0("data")
  # tar_name_report <- paste0("report")

  tar_name_cleanin_log_wb <- paste0("wb")
  tar_name_summary <- paste0("summary")
  tar_name_logbook <- paste0("logbook")
  tar_name_deletion_log <- paste0("deletion_log")


  tar_name_augmented_summary <- paste0("augmented_summary")
  tar_name_augmented_data_extract <- paste0("augmented_data_extract")
  tar_name_augmented_logbook <- paste0("augmented_logbook")
  tar_name_augmented_deletion_log <- paste0("augmented_deletion_log")
  tar_name_augmented_data <- paste0("augmented_data")

  tar_name_cleaning_log_output <- paste0("cleaning_log_output")

  augmented_targets <- list(
    tar_target_raw(
      name = tar_name_augmented_summary,
      command = substitute(
        augment_summary(
          raw_data,
          summary,
          augmented_logbook
        ),
        env = list(
          raw_data = as.symbol(tar_name_data),
          summary = as.symbol(tar_name_summary),
          augmented_logbook = as.symbol(tar_name_augmented_logbook)
        )
      )
    ),
    tar_target_raw(
      name = tar_name_augmented_data_extract,
      command = substitute(
        augment_data_extract(raw_data),
        env = list(
          raw_data = as.symbol(tar_name_data)
        )
      )
    ),
    tar_target_raw(
      name = tar_name_augmented_logbook,
      command = substitute(
        augment_logbook(raw_data, logbook),
        env = list(
          raw_data = as.symbol(tar_name_data),
          logbook = as.symbol(tar_name_logbook)
        )
      )
    ),
    tar_target_raw(
      name = tar_name_augmented_deletion_log,
      command = substitute(
        deletion_log,
        env = list(deletion_log = as.symbol(tar_name_deletion_log))
      )
    ),
    tar_target_raw(
      name = tar_name_augmented_data,
      command = substitute(
        augment_data(
          raw_data, augmented_logbook, augmented_deletion_log
        ),
        env = list(
          raw_data = as.symbol(tar_name_data),
          augmented_logbook = as.symbol(tar_name_augmented_logbook),
          augmented_deletion_log = as.symbol(tar_name_augmented_deletion_log)
        )
      )
    ),
    tar_target_raw(
      name = tar_name_cleaning_log_output,
      command = substitute(
        write_cleaning_log(
          wb,
          raw_data = raw_data,
          clean_data = clean_data,
          data_extract = data_extract,
          base_path = base_path,
          logbook = logbook,
          summary = summary
        ),
        env = list(
          wb = as.symbol(tar_name_cleanin_log_wb),
          raw_data = as.symbol(tar_name_data),
          clean_data = as.symbol(tar_name_augmented_data),
          data_extract = as.symbol(tar_name_augmented_data_extract),
          logbook = as.symbol(tar_name_augmented_logbook),
          summary = as.symbol(tar_name_augmented_summary),
          base_path = base_path
        )
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



  list(
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
        readxl::read_excel(path, guess_max = 10000) |> generate_enumerator_id(),
        env = list(
          path = as.symbol(tar_name_raw)
        )
      )
    ),
    tar_target_raw(
      name = tar_name_cleanin_log_wb,
      command = substitute(
        openxlsx::loadWorkbook(path),
        env = list(path = as.symbol(tar_name_cleaning_log_path))
      )
    ),
    sheets_targets,
    augmented_targets,
    tar_target_raw(
      "outliers_report",
      command = substitute(find_outliers(clean_data, logbook),
        env = list(
          clean_data = as.symbol(tar_name_augmented_data),
          logbook = as.symbol(tar_name_logbook)
        )
      ),
      packages = c("dplyr", "magrittr")
    )
    # tarchetypes::tar_quarto_raw(
    #   "report",
    #   path = fs::path(base_path, "report", "report.qmd"),
    #   execute_params = quote(list())
    # )
  )
}
