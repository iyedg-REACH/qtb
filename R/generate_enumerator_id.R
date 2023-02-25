#' Generate a Unique ID for each enumerator
#'
#' @param df Data Frame of the Raw JMMI Survey Data
#'
#' @return  Data Frame of Submissions UUIDs and Unique IDs for the corresponding enumerators
#' @export
generate_enumerator_id <- function(df) {
  ids_df <- df |>
    dplyr::select(.data[["q_orgname"]], .data[["deviceid"]]) |>
    dplyr::distinct() |>
    dplyr::arrange(.data[["q_orgname"]], .data[["deviceid"]]) |>
    dplyr::group_by(.data[["q_orgname"]]) |>
    dplyr::mutate(enum_n = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::mutate(enumerator_id = paste0(.data[["q_orgname"]], .data[["enum_n"]])) |>
    dplyr::select(-.data[["enum_n"]])

  df |>
    tidylog::left_join(ids_df, by = c("q_orgname", "deviceid"))
}
