#' Calculate Rahu Kalam timing based on sunrise and sunset
#'
#' @name calculate_rahu_kalam
#' @param date Date for which Rahu Kalam is calculated.
#' @param lat Latitude of the location.
#' @param lon Longitude of the location.
#' @param tz Time zone.
#'
#' @return List with Rahu Kalam start and end times.
#'
#' @import suncalc
#' @import swephR
#' @importFrom lubridate as_datetime
#' @export


library(suncalc)

# Function to calculate Rahu Kalam using dynamic sunrise adjustments
calculate_rahu_kalam <- function(date, lat, lon, tz) {
  date <- as.Date(date)  # Ensure correct date format

  # Get sunrise time using the provided timezone
  sunlight_time <- getSunlightTimes(date = date, lat = lat, lon = lon, tz = tz)
  sunrise_time <- sunlight_time$sunrise

  # Compute total daylight duration (sunrise to sunset)
  sunset_time <- sunlight_time$sunset
  day_duration <- as.numeric(difftime(sunset_time, sunrise_time, units = "secs"))
  segment_duration <- day_duration / 8  # Divide daylight into 8 equal parts

  # Define Rahu Kalam slot based on the day of the week
  weekday <- weekdays(date)
  rahu_kalam_segment <- switch(weekday,
                               "Monday" = 2,   # 2nd segment
                               "Saturday" = 3, # 3rd segment
                               "Friday" = 4,   # 4th segment
                               "Wednesday" = 5,# 5th segment
                               "Thursday" = 6, # 6th segment
                               "Tuesday" = 7,  # 7th segment
                               "Sunday" = 8)   # 8th segment

  # Compute Rahu Kalam start and end time based on sunrise
  rahu_kalam_start <- sunrise_time + (rahu_kalam_segment - 1) * segment_duration
  rahu_kalam_end <- rahu_kalam_start + segment_duration

  return(list(date = date, start = rahu_kalam_start, end = rahu_kalam_end))
}

# Function to predict Rahu Kalam for the next 30 days
predict_rahu_kalam_month <- function(start_date, lat, lon, tz) {
  results <- list()
  current_date <- as.Date(start_date)

  for (i in 1:30) {  # Loop through 30 days
    rahu_kalam <- calculate_rahu_kalam(current_date, lat, lon, tz)
    results <- append(results, list(rahu_kalam))
    current_date <- current_date + 1  # Move to the next day
  }

  return(results)
}
