#' MEB weights for the Libyan JMMI
#'
#' Weights of MEB items used for the cost of the MEB
#' in Libya
#'
#' \describe{
#'   \item{item}{Item label as per the standard KoBo tool used for data collection}
#'   \item{group}{The item group of the item}
#'   \item{label}{A readable label for the item, including the unit}
#'   \item{weight}{The weight contribution of the item towards the final cost of MEB}
#' }
#'
#' @docType data
#'
#' @keywords datasets
"meb_weights"


#' JMMI dataset for January 2022
#'
#' The clean dataset for January 2022 JMMI
#' @source \url{https://www.impact-repository.org/document/reach/cd36ef9f/reach_lby_dataset_joint_market_monitoring_initiative_jmmi_January_2022.xlsx}
#'
#' @docType data
#'
#' @keywords datasets
"jmmi_2022_jan"

#' JMMI dataset for February 2022
#'
#' The clean dataset for February 2022 JMMI
#' @source \url{https://www.impact-repository.org/document/reach/a5964fda/reach_lby_dataset_joint_market_monitoring_initiative_jmmi_February_2022.xlsx}
#'
#' @docType data
#'
#' @keywords datasets
"jmmi_2022_feb"

#' Libyan Municipalities
#'
#' A dataset containing the identifiers as well as
#' the Arabic and English names of Libyan Municipalities
#'
#' \describe{
#'   \item{municipality_id}{A unique identifier for the Municipality}
#'   \item{municipality_name_ar}{The Municipality's name in Arabic}
#'   \item{municipality_name_en}{The Municipality's name in English}
#'   \item{municipality_district_id}{The district to which the Municipality belongs}
#'   \item{municipality_region_id}{The region to which the Municipality belongs}
#' }
#' @source \url{reach}
"lby_municipalities"

#' Libyan Districts
#'
#' A dataset containing the identifiers as well as
#' the Arabic and English names of Libyan Districts
#'
#' \describe{
#'   \item{district_id}{A unique identifier for the District}
#'   \item{district_name_ar}{The Districts' name in Arabic}
#'   \item{district_name_en}{The Districts' name in English}
#'   \item{district_region_id}{The region to which the Districts belongs}
#' }
#' @source \url{reach}
"lby_districts"


#' Libyan Regions
#'
#' A dataset containing the identifiers as well as
#' the Arabic and English names of Libyan Regions
#'
#' \describe{
#'   \item{region_id}{A unique identifier for the Region}
#'   \item{region_name_ar}{The Region's name in Arabic}
#'   \item{region_name_en}{The Region's name in English}
#' }
#' @source \url{reach}
"lby_regions"



#' Tracked Municipalities
#'
#' The list of municipalities where data collection
#' usually occurs.
#'
#' \describe{
#'   \item{municipality_name_en}{The Municipality's name in English}
#'   \item{position}{A helper attribute indicating the position of the Municipality in table reports}
#' }
"tracked_municipalities"
