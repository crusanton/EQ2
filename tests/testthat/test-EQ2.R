library(readr)
library(testthat)

test_that("main check", {

  test_that("files available", {
      testthat::expect_equal(list.files(system.file("extdata", package = "EQ2")),"earthquakes.txt")})

  file_name <- system.file("extdata", "earthquakes.txt", package="EQ2")
  raw_data   <- readr::read_delim(file_name, delim = "\t")
  clean_data <- EQ2::eq_clean_data(raw_data)

  # correct number of rows
  expect_equal(clean_data %>% nrow(), 6200)
  expect_equal(clean_data %>% dplyr::filter(COUNTRY == "ITALY") %>% nrow(), 326)
  expect_equal(clean_data %>% dplyr::filter(COUNTRY == "KENYA") %>% nrow(), 2)

  # check various values
  USA_magnitude   <- clean_data %>%
                     dplyr::filter(COUNTRY == "USA" & YEAR >= 2000)  %>%
                     dplyr::summarize(USA_mean = mean(EQ_PRIMARY)) %>% dplyr::pull(USA_mean)
  USA_magnitude = round(USA_magnitude, 2)
  expect_equal(USA_magnitude, 5.69)

  India_magnitude <- clean_data %>%
                     dplyr::filter(COUNTRY == "INDIA" & YEAR >= 2000) %>%
                     dplyr::summarize(India_mean = mean(EQ_PRIMARY)) %>% dplyr::pull(India_mean)
  India_magnitude = round(India_magnitude, 2)
  expect_equal(India_magnitude, 5.2)

  germany_houses <- clean_data %>%
                    dplyr::filter(COUNTRY == "GERMANY" & YEAR >= 2000) %>%
                    dplyr::summarize(Germany_mean = mean(FOCAL_DEPTH)) %>% dplyr::pull(Germany_mean)
  expect_equal(germany_houses, 9.5)

  # eq_clean_data
  expect_that(clean_data$DATE,      testthat::is_a('Date'))
  expect_that(clean_data$LATITUDE,  testthat::is_a('numeric'))
  expect_that(clean_data$LONGITUDE, testthat::is_a('numeric'))

  #  eq_location_clean
  expect_equal(eq_location_clean("GREECE:  THERA ISLAND (SANTORINI)"), "Thera Island (Santorini)")

  #  eq_map
  testthat::expect_that(clean_data %>%
                        dplyr::filter(COUNTRY == 'FRANCE' & lubridate::year(DATE) >= 2000) %>%
                        eq_map(annot_col = 'DATE'), testthat::is_a('leaflet'))

  # eq_create_label
  testthat::expect_that(eq_create_label(clean_data), testthat::is_a('character'))
  testthat::expect_that(clean_data %>%
                        dplyr::filter(COUNTRY == 'JAPAN' & lubridate::year(DATE) >= 1960) %>%
                        dplyr::mutate(popup_text = eq_create_label(.)) %>%
                        eq_map(annot_col = 'popup_text'), testthat::is_a('leaflet'))

})   # test_that("main check"...
