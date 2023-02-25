#' Generate a Unique ID for each enumerator
#'
#' @param df Data Frame of the Raw JMMI Survey Data
#'
#' @return  Data Frame of Submissions UUIDs and Unique IDs for the corresponding enumerators
#' @export
#'
#' @examples
#' generate_enumerator_id(jmmi_2022_feb)
generate_enumerator_id <- function(df) {
  ids_df <- df |>
    dplyr::select(q_orgname, deviceid) |>
    dplyr::distinct() |>
    dplyr::arrange(q_orgname, deviceid) |>
    dplyr::group_by(q_orgname) |>
    dplyr::mutate(enum_n = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::mutate(enumerator_id = paste0(q_orgname, enum_n)) |>
    dplyr::select(-enum_n)

  df |>
    tidylog::left_join(ids_df, by = c("q_orgname", "deviceid"))
}
