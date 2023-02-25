#' Generate a Data Summary from Raw Data
#'
#' This function creates the summary from the raw data if it is empty to begin with.
#'
#' @param raw_data Data Frame of the Raw JMMI Survey Data
#' @param summary  The current state of the `00_Summary` Sheet in the Cleaning Log
#' @param augmented_logbook A Data Frame of the Augmented Logbook
#'
#' @return Data Frame Retaining summarising the operations of the cleaning log
#' @export
augment_summary <- function(raw_data,
                            summary,
                            augmented_logbook) {
  if (nrow(summary) == 0) {
    summary <- tibble::tibble(
      Variable = names(raw_data),
      Action = "Checked",
      Description = NA_character_
    )
  }

  count_affected_observations <- augmented_logbook |>
    dplyr::filter(tolower(.data[["changed"]]) == "yes") |>
    dplyr::count(.data[["question.name"]]) |>
    dplyr::rename(`Observations affected` = .data[["n"]])


  tidylog::left_join(summary, count_affected_observations,
    by = c("Variable" = "question.name")
  ) |>
    dplyr::mutate(
      `Observations affected` = tidyr::replace_na(.data[["Observations affected"]], 0),
      Action = dplyr::if_else(.data[["Observations affected"]] != 0,
        "Corrected",
        .data[["Action"]]
      )
    )
}
