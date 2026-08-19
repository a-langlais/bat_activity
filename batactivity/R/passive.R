#' Analyze passive bat activity by place and species
#'
#' Computes contact counts, positive nights, positive hours, positive minutes,
#' and CPN/CPH summaries for standardized passive survey data.
#'
#' @param data A standard passive survey data frame.
#' @param nights Number of recording nights represented by the dataset.
#' @param record_time Character vector of recording start and end times in
#'   `HH:MM` format.
#'
#' @return A data frame of passive activity indicators by `Place` and `Id`.
#' @export
species_place_activity <- function(data, nights = 1, record_time = c("22:00", "06:00")) {
  if (!is.data.frame(data)) {
    stop("data must be a data.frame.", call. = FALSE)
  }
  if (!is.numeric(nights) || length(nights) != 1 || is.na(nights) || nights <= 0) {
    stop("nights must be a positive number.", call. = FALSE)
  }
  required_columns <- c(
    "Place", "Id", "Night_Date", "Date_Time", "Year", "Month",
    "Week", "Day", "Time", "Hour", "Minute"
  )
  require_columns(data, required_columns, "data")
  if (nrow(data) == 0) {
    stop("data must contain at least one row.", call. = FALSE)
  }

  night_duration <- night_duration_hours(record_time)
  data$Night_Date <- as.Date(data$Night_Date)
  data$Date_Time <- as.POSIXct(data$Date_Time, tz = "Europe/Paris")
  data$Hour <- as.numeric(data$Hour)
  data$Minute <- as.numeric(data$Minute)

  if (any(is.na(data$Night_Date))) {
    stop("Night_Date contains values that cannot be parsed as dates.", call. = FALSE)
  }
  if (any(is.na(data$Date_Time))) {
    stop("Date_Time contains values that cannot be parsed as datetimes.", call. = FALSE)
  }

  keys <- paste(data$Place, data$Id, sep = "\r")
  grouped <- split(data, keys, drop = TRUE)

  rows <- lapply(grouped, function(group) {
    night_counts <- table(group$Night_Date)
    hour_key <- paste(group$Night_Date, group$Hour)
    hour_counts <- table(hour_key)
    minute_key <- paste(group$Night_Date, group$Hour, group$Minute)

    data.frame(
      Place = as.character(group$Place[[1]]),
      Id = as.character(group$Id[[1]]),
      contacts = nrow(group),
      night_positive = length(unique(group$Night_Date)),
      hour_positive = length(unique(hour_key)),
      minute_positive = length(unique(minute_key)),
      min_CPN = min(night_counts),
      mean_CPN = mean(night_counts),
      sd_CPN = stats::sd(as.numeric(night_counts)),
      max_CPN = max(night_counts),
      min_CPH = min(hour_counts),
      mean_CPH = mean(hour_counts) / (nights * night_duration),
      sd_CPH = stats::sd(as.numeric(hour_counts)) / (nights * night_duration),
      max_CPH = max(hour_counts),
      stringsAsFactors = FALSE
    )
  })

  results <- do.call(rbind, rows)
  rownames(results) <- NULL
  results
}
