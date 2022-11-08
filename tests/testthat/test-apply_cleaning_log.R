test_that("applying cleaning log with NA works", {
  df <- tibble::tribble(
    ~`_uuid`, ~name, ~value,
    "a", "string_to_na", "c",
    "a", "na_to_string", NA_character_,
    "a", "numeric_to_na", "1",
    "a", "na_to_numeric", NA_character_,
    "b", "string_to_na", "a",
    "b", "na_to_string", NA_character_,
    "b", "numeric_to_na", "0",
    "b", "na_to_numeric", NA_character_
  )

  cleaning_log <- tibble::tribble(
    ~`_uuid`, ~question.names, ~old.value, ~new.value,
    "a", "string_to_na", "c", NA_character_,
    "a", "na_to_string", NA_character_, "c",
    "a", "numeric_to_na", "1", NA_character_,
    "a", "na_to_numeric", NA_character_, "3"
  )

  expected_df <- tibble::tribble(
    ~`_uuid`, ~name, ~value,
    "a", "string_to_na", NA_character_,
    "a", "na_to_string", "c",
    "a", "numeric_to_na", NA_character_,
    "a", "na_to_numeric", "3",
    "b", "string_to_na", "a",
    "b", "na_to_string", NA_character_,
    "b", "numeric_to_na", "0",
    "b", "na_to_numeric", NA_character_
  )

  expect_equal(
    apply_cleaning_log(
      df,
      df_value_col = value,
      cleaning_log,
      cleaning_log_value_col = new.value,
      by = c("_uuid" = "_uuid", "name" = "question.names")
      ),
    expected_df
  )
})

# TODO: test for handling duplicates
# TODO: test for invalid columns in cleaning log
