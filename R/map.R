utils::globalVariables(c("MONTH", "DAY", "DEATHS", "EQ_PRIMARY","ymd", "years", "YEAR", "days",
                         "DATE", "LATITUDE", "LONGITUDE","LOCATION_NAME", "layer"))


#' Maps the epicenters
#'
#' @param input_data Earthquake data
#' @param annot_col  column to show (in this case, DATE)
#'
#' @importFrom dplyr "%>%"
#' @import leaflet
#'
#' @return Earthquake data Clean
#'
#' @examples
#' \dontrun{
#' file_name <- system.file("eq_data", "extdata/earthquakes.txt", package = "EQ")
#' eq_data   <- file_name                                                          %>%
#'              readr::read_delim(txt, delim = "\t")                               %>%
#'              eq_clean_data()                                                    %>%
#'              dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'              dplyr::mutate(popup_text = eq_create_label(.))                     %>%
#'              eq_map(annot_col = "popup_text")
#' }
#' @export
eq_map <- function(input_data, annot_col = "DATE"){
   print(leaflet()   %>%
         addTiles()  %>%
         addCircleMarkers(data   = input_data,
                          radius = ~ EQ_PRIMARY * 1.5,  # multiply by 1.5 to make the diff between large/small more pronounced on map
                          lng    = ~ LONGITUDE,
                          lat    = ~ LATITUDE,
                          popup  = ~ eval(expr = parse(text = annot_col))))
} # eq_map


#' Create label for HTML
#'
#' @param input_data Earthquake data
#'
#' @return Earthquake data Clean
#'
#' @examples
#' \dontrun{
#' file_name <- system.file("eq_data", "extdata/earthquakes.txt", package = "EQ")
#' eq_data   <- file_name                                                           %>%
#;              readr::read_delim(txt, delim = "\t")                                %>%
#'              eq_clean_data()                                                     %>%
#'              dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000)  %>%
#'              dplyr::mutate(popup_text = eq_create_label(.))                      %>%
#'              eq_map(annot_col = "popup_text")
#' }
#' @export
eq_create_label <- function(input_data){
  paste(ifelse(!is.na(input_data$LOCATION_NAME), paste("<b>Location:</b>",     input_data$LOCATION_NAME), ""),
        ifelse(!is.na(input_data$EQ_PRIMARY),    paste("<b>Magnitude:</b>",    input_data$EQ_PRIMARY),    ""),
        ifelse(!is.na(input_data$DEATHS),        paste("<b>Total deaths:</b>", input_data$DEATHS),        ""), sep = "<br/>")
} # eq_create_label

