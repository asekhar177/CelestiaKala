# ---- Package Imports ----
#' @import suncalc
#' @import swephR
#' @importFrom lubridate as_datetime
#'
# ---- calculate_rahu_kalam ----
#' Calculate Rahu Kalam timing based on sunrise and sunset
#'
#' Computes the Rahu Kalam period for a given date, location, and timezone.
#'
#' @param date A `Date` object representing the day for which to calculate Rahu Kalam.
#' @param lat Latitude in decimal degrees (e.g., 28.6139 for Delhi).
#' @param lon Longitude in decimal degrees (e.g., 77.2090 for Delhi).
#' @param tz Timezone as a string (e.g., "Asia/Kolkata").
#'
#' @return A list with the date, start, and end time of Rahu Kalam.
#'
#' @examples
#' calculate_rahu_kalam(Sys.Date(), lat = -33.87, lon = 151.21, tz = "Australia/Sydney")
#'
#' @export
calculate_rahu_kalam <- function(date, lat, lon, tz) {
  date <- as.Date(date)
  sunlight_time <- getSunlightTimes(date = date, lat = lat, lon = lon, tz = tz)
  sunrise_time <- sunlight_time$sunrise
  sunset_time <- sunlight_time$sunset
  day_duration <- as.numeric(difftime(sunset_time, sunrise_time, units = "secs"))
  segment_duration <- day_duration / 8

  weekday <- weekdays(date)
  rahu_kalam_segment <- switch(weekday,
                               "Monday" = 2,
                               "Saturday" = 3,
                               "Friday" = 4,
                               "Wednesday" = 5,
                               "Thursday" = 6,
                               "Tuesday" = 7,
                               "Sunday" = 8)

  rahu_kalam_start <- sunrise_time + (rahu_kalam_segment - 1) * segment_duration
  rahu_kalam_end <- rahu_kalam_start + segment_duration

  return(list(date = date, start = rahu_kalam_start, end = rahu_kalam_end))
}

# ---- predict_rahu_kalam_month ----
#' Predict Rahu Kalam for the next 30 days
#'
#' Generates Rahu Kalam timings for a 30-day window starting from a given date.
#'
#' @param start_date A `Date` object representing the first day to begin predictions.
#' @param lat Latitude in decimal degrees.
#' @param lon Longitude in decimal degrees.
#' @param tz Timezone as a string (e.g., "Asia/Kolkata").
#'
#' @return A list of Rahu Kalam time periods for the next 30 days.
#'
#' @examples
#' predict_rahu_kalam_month(Sys.Date(), lat = -33.87, lon = 151.21, tz = "Australia/Sydney")
#'
#' @export
predict_rahu_kalam_month <- function(start_date, lat, lon, tz) {
  results <- list()
  current_date <- as.Date(start_date)

  for (i in 1:30) {
    rahu_kalam <- calculate_rahu_kalam(current_date, lat, lon, tz)
    results[[i]] <- rahu_kalam
    current_date <- current_date + 1
  }

  return(results)
}
