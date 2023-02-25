#' Generate A Logbook from Raw Data
#'
#' @param raw_data  Data Frame of the Raw JMMI Survey Data
#' @param logbook State of the `02_Logook` Sheet before running the function
#'
#' @return Data Frame Listing the changes entered manually to the `02_Logook` Sheet
#' @export
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
      cols = -dplyr::all_of(completion_columns),
      names_to = "question.name",
      values_to = "old.value",
      values_transform = as.character
    ) |>
    dplyr::rename(
      `device ID` = .data[["deviceid"]],
      `Enumerator ID` = .data[["enumerator_id"]]
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
