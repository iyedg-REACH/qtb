augment_summary <- function(raw_data, summary) {

  if (nrow(summary) == 0) {
    summary <- tibble::tibble(
      Variable = names(raw_data),
      Action = "Checked",
      Description = NA_character_,
      `Observations affected` = NA_integer_
    )
  }

  return(summary)
}
