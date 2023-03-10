#' Identify Outliers while ignoring those mentioned in a logbook
#'
#' @param df  Data Frame of the Raw JMMI Survey Data
#' @param logbook Data Frame of the `02_Logbook` sheet
#'
#' @return Data Frame
#' @export
find_outliers <- function(df, logbook) {
  treated_observations <- dplyr::select(
    logbook,
    dplyr::all_of(c("uuid", "question.name"))
  ) |>
    dplyr::distinct()


  outliers_report <- df |>
    dplyr::select(-dplyr::starts_with("_")) |>
    cleaninginspectoR::find_outliers() |>
    dplyr::select(.data[["variable"]], .data[["value"]])

  outlier_cols <- outliers_report |>
    dplyr::pull(.data[["variable"]]) |>
    unique()

  df |>
    select(dplyr::contains("uuid"), all_of(outlier_cols)) |>
    tidyr::pivot_longer(-dplyr::contains("uuid"),
      names_to = "variable",
      values_transform = as.numeric
    ) |>
    dplyr::inner_join(outliers_report, by = c("variable", "value")) |>
    dplyr::filter(!.data[["_uuid"]] %in% treated_observations) |>
    dplyr::anti_join(treated_observations, by = c(
      "_uuid" = "uuid",
      "variable" = "question.name"
    )) |>
    dplyr::distinct()
}
