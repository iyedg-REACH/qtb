augment_summary <- function(raw_data, summary, augmented_logbook) {
  if (nrow(summary) == 0) {
    summary <- tibble::tibble(
      Variable = names(raw_data),
      Action = "Checked",
      Description = NA_character_
    )
  }

  count_affected_observations <- augmented_logbook |>
    dplyr::filter(tolower(changed) == "yes") |>
    dplyr::count(question.name) |>
    dplyr::rename(`Observations affected` = n)


  tidylog::left_join(summary, count_affected_observations,
    by = c("Variable" = "question.name")
  ) |>
    dplyr::mutate(
      `Observations affected` = tidyr::replace_na(`Observations affected`, 0),
      Action = dplyr::if_else(`Observations affected` != 0,
        "Corrected",
        Action
      )
    )
}
