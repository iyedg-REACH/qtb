augment_logbook <- function(raw_data, logbook) {
  necessary_columns <- c(
    "uuid",
    "question.name",
    "Issue",
    "Type of Issue",
    "feedback",
    "old.value",
    "new.value"
  )


  necessary_logbook <- dplyr::select(
    logbook,
    dplyr::all_of(necessary_columns)
  )

  completion_columns <- c(
    "_uuid",
    "deviceid",
    "q_name"
  )

  completion_df <- dplyr::select(
    raw_data,
    dplyr::all_of(completion_columns)
  ) |>
    dplyr::mutate(changed = "Yes") |>
    dplyr::rename(
      `device ID` = deviceid,
      `Enumerator ID` = q_name
    )

  tidylog::left_join(necessary_logbook, completion_df, by = c("uuid" = "_uuid")) |>
    dplyr::select(
      "uuid",
      "Enumerator ID",
      "device ID",
      "question.name",
      "Issue",
      "Type of Issue",
      "feedback",
      "changed",
      "old.value",
      "new.value"
    )
}
