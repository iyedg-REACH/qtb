augment_logbook <- function(raw_data, logbook) {
  necessary_columns <- c(
    "uuid",
    "question.name",
    "Issue",
    "Type of Issue",
    "feedback",
    "changed",
    "new.value"
  )


  necessary_logbook <- dplyr::select(
    logbook,
    dplyr::all_of(necessary_columns)
  )

  completion_columns <- c(
    "_uuid",
    "deviceid",
    "enumerator_id"
  )

  completion_df <- raw_data |>
    tidyr::pivot_longer(
      cols = -completion_columns,
      names_to = "question.name",
      values_to = "old.value",
      values_transform = as.character
    ) |>
    dplyr::rename(
      `device ID` = deviceid,
      `Enumerator ID` = enumerator_id
    )



  tidylog::left_join(necessary_logbook, completion_df,
    by = c(
      "uuid" = "_uuid",
      "question.name" = "question.name"
    )
  ) |>
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
