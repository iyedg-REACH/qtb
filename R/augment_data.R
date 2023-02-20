augment_data <- function(raw_data, augmented_logbook, augmented_deletion_log) {
  logbook <- dplyr::select(
    augmented_logbook,
    all_of(c("uuid", "question.name", "new.value"))
  )

  deletion_log <- dplyr::select(
    augmented_deletion_log,
    all_of(c("uuid"))
  )

  # Keeping track of the original type of columns
  numeric_columns <- dplyr::select(
    raw_data,
    tidyselect::where(is.numeric)
  ) |> names()

  long_raw <- tidyr::pivot_longer(
    raw_data,
    cols = -c("_uuid"),
    values_transform = ~ as.character(.x),
    names_to = "question.name",
    values_to = "old.value"
  )


  to_update <- dplyr::inner_join(long_raw,
    logbook,
    by = c("_uuid" = "uuid", "question.name" = "question.name")
  ) |>
    dplyr::mutate(old.value = new.value) |>
    dplyr::select(-new.value)

  untouched <- dplyr::anti_join(long_raw,
    logbook,
    by = c("_uuid" = "uuid", "question.name" = "question.name")
  )

  dplyr::bind_rows(untouched, to_update) |>
    tidyr::pivot_wider(
      id_cols = "_uuid",
      names_from = "question.name",
      values_from = "old.value"
    ) |>
    tidylog::filter(!`_uuid` %in% deletion_log$uuid) |>
    tidylog::mutate(dplyr::across(dplyr::all_of(numeric_columns), as.numeric)) |>
    tidylog::mutate(dplyr::across(dplyr::ends_with("_price"), as.numeric)) |>
    tidylog::mutate(dplyr::across(dplyr::ends_with("_quantity2"), as.numeric)) |>
    dplyr::select(dplyr::any_of(names(raw_data))) # TODO: this may remove added columns
}
