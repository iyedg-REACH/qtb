find_outliers <- function(df, logbook) {
  treated_observations <- dplyr::select(
    logbook,
    dplyr::all_of(c("uuid", "question.name"))
  ) |>
    distinct()


  outliers_report <- df |>
    dplyr::select(-dplyr::starts_with("_")) |>
    cleaninginspectoR::find_outliers() |>
    dplyr::select(variable, value)

  outlier_cols <- outliers_report |>
    dplyr::pull(variable) |>
    unique()

  df |>
    select(contains("uuid"), all_of(outlier_cols)) |>
    tidyr::pivot_longer(-contains("uuid"),
      names_to = "variable",
      values_transform = as.numeric
    ) |>
    dplyr::inner_join(outliers_report, by = c("variable", "value")) |>
    dplyr::filter(!`_uuid` %in% treated_observations) |>
    dplyr::anti_join(treated_observations, by = c(
      "_uuid" = "uuid",
      "variable" = "question.name"
    )) |>
    dplyr::distinct()
}
