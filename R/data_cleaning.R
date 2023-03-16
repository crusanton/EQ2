utils::globalVariables(c("MONTH", "DAY", "DEATHS", "EQ_PRIMARY","ymd", "years", "YEAR", "days",
                         "DATE", "LATITUDE", "LONGITUDE","LOCATION_NAME", "layer"))


#' Remove country name from the LOCATION_NAME
#'
#' @param LOCATION_NAME single string that contains country and location
#'
#' @importFrom dplyr   "%>%"
#' @importFrom stringr str_to_title
#'
#' @return string with location only, country is removed
#'
#' @examples
#' \dontrun{
#' location_list <- c("MEXICO: SAN ANDRES TUXTLA, TUXTEPEC", "MEXICO: MEXICALI, BAJA CALIFORNIA", "MEXICO: ACAPULCO")
#' eq_location_clean(location_list)
#' }
#'
#' @export
eq_location_clean <- function(LOCATION_NAME){
  strsplit(LOCATION_NAME, split = ":")      %>%
  lapply(FUN = function(x){ x[length(x)]})  %>%
  unlist()                                  %>%
  trimws()                                  %>%
  stringr::str_to_title()
}  # eq_location_clean


#' Data Cleaning for everything else
#'
#' @param input_data raw earthquake data
#'
#' @importFrom dplyr     "%>%" mutate
#' @importFrom lubridate ymd years days
#'
#' @return cleaned data, new column for DATE, numeric for DEATHS, LATITUDE, LONGITUDE
#'
#' @examples
#' \dontrun{
#' eq_clean_data(eq_data)
#' }
#' @export
eq_clean_data <- function(input_data){
  input_data                                                     %>%
  dplyr::mutate(MONTH      = ifelse(is.na(MONTH),0, MONTH),
                DAY        = ifelse(is.na(DAY),0, DAY),
                DEATHS     = as.numeric(DEATHS),
                EQ_PRIMARY = as.numeric(EQ_PRIMARY))             %>%
  dplyr::mutate(DATE = ifelse(MONTH > 0,
                       ifelse(DAY   > 0,
                       lubridate::ymd("0000-01-01") + lubridate::years(YEAR)+months(MONTH-1) + lubridate::days(DAY-1),
                       lubridate::ymd("0000-01-01") + lubridate::years(YEAR)+months(MONTH-1) + lubridate::days(DAY)),
                       lubridate::ymd("0000-01-01") + lubridate::years(YEAR)+months(MONTH)   + lubridate::days(DAY)),
                DATE          = as.Date(DATE, origin = "1970-01-01"),  # requires origin, picked as default from R documentation
                LATITUDE      = as.numeric(LATITUDE),
                LONGITUDE     = as.numeric(LONGITUDE),
                LOCATION_NAME = eq_location_clean(LOCATION_NAME))
} #eq_clean_data
