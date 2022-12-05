URL <- "https://www.impact-repository.org/document/reach/cd36ef9f/reach_lby_dataset_joint_market_monitoring_initiative_jmmi_January_2022.xlsx"
jmmi_2022_jan_file <- tempfile(fileext = ".xlsx")

httr::GET(URL, httr::write_disk(jmmi_2022_jan_file, overwrite = TRUE))
jmmi_2022_jan <- openxlsx::read.xlsx(jmmi_2022_jan_file,
  sheet = "Clean Data",
  startRow = 2
) |>
  tibble::as_tibble()

usethis::use_data(jmmi_2022_jan, overwrite = TRUE)
