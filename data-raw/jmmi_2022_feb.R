## code to prepare `jmmi_2022_feb` dataset goes here

URL <- "https://www.impact-repository.org/document/reach/a5964fda/reach_lby_dataset_joint_market_monitoring_initiative_jmmi_February_2022.xlsx"
jmmi_2022_feb_file <- tempfile(fileext = ".xlsx")

httr::GET(URL, httr::write_disk(jmmi_2022_feb_file, overwrite = TRUE))
jmmi_2022_feb <- openxlsx::read.xlsx(jmmi_2022_feb_file,
  sheet = "Clean Data",
  startRow = 2
)

usethis::use_data(jmmi_2022_feb, overwrite = TRUE)
