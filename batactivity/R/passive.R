#' Analyze passive bat activity by place and species
#'
#' Zero-contact nights are included in CPN and CPH metrics. With fixed recording
#' hours, contacts outside that window are excluded. With solar offsets,
#' ephemerides are used only to calculate each night's CPH duration.
#'
#' @param data A data frame with Place, Id, Night_Date, and Date_Time columns.
#' @param city Nearest city for solar calculations.
#' @param record_time Start and end times in HH:MM for fixed-time calculations.
#' @param sun_offsets Optional named offsets, in minutes, before sunset and after sunrise.
#'   In this mode, all contacts are retained; the resulting solar window is used
#'   only as the denominator of the CPH indicators.
#' @param timezone IANA time zone used to interpret recording times and
#'   ephemerides. Defaults to French time (\code{Europe/Paris}).
#'
#' @return A data frame of passive activity indicators by Place and Id.
#' @export
species_place_activity <- function(data,
                                   city = "Paris",
                                   record_time = c("08:00", "22:00"),
                                   sun_offsets = NULL,
                                   timezone = "Europe/Paris") {
  if (!is.data.frame(data)) stop("data must be a data.frame.", call. = FALSE)
  require_columns(data, c("Place", "Id", "Night_Date", "Date_Time"), "data")
  if (nrow(data) == 0) stop("data must contain at least one row.", call. = FALSE)

  cities <- data.frame(
    city = c("Paris", "Lyon", "Marseille", "Toulouse", "Bordeaux", "Brest", "Strasbourg"),
    latitude = c(48.8566, 45.7640, 43.2965, 43.6047, 44.8378, 48.3904, 48.5734),
    longitude = c(2.3522, 4.8357, 5.3698, 1.4442, -0.5792, -4.4861, 7.7521),
    stringsAsFactors = FALSE
  )
  if (!is.character(city) || length(city) != 1 || !city %in% cities$city) {
    stop("city must be one of: ", paste(cities$city, collapse = ", "), call. = FALSE)
  }
  if (!is.character(timezone) || length(timezone) != 1 || !timezone %in% OlsonNames()) {
    stop("timezone must be a valid IANA time zone, such as Europe/Paris.", call. = FALSE)
  }

  data$Night_Date <- as.Date(data$Night_Date)
  date_time_values <- trimws(as.character(data$Date_Time))
  date_only <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", date_time_values)
  date_time_values[date_only] <- paste(date_time_values[date_only], "00:00:00")
  data$Date_Time <- parse_datetime(date_time_values, timezone = timezone)
  if (any(is.na(data$Night_Date))) stop("Night_Date contains values that cannot be parsed as dates.", call. = FALSE)
  if (any(is.na(data$Date_Time))) stop("Date_Time contains values that cannot be parsed as datetimes.", call. = FALSE)

  all_nights <- seq(min(data$Night_Date), max(data$Night_Date), by = "day")
  if (is.null(sun_offsets)) {
    start_time <- as.POSIXct(paste(all_nights, record_time[[1]]), tz = timezone)
    end_time <- as.POSIXct(paste(all_nights + 1, record_time[[2]]), tz = timezone)
    windows <- data.frame(
      Night_Date = all_nights,
      start_time = start_time,
      end_time = end_time,
      duration_hours = as.numeric(difftime(end_time, start_time, units = "hours"))
    )
  } else {
    needed <- c("before_sunset", "after_sunrise")
    if (!is.numeric(sun_offsets) || !all(needed %in% names(sun_offsets)) ||
        any(is.na(sun_offsets[needed])) || any(sun_offsets[needed] < 0)) {
      stop("sun_offsets must provide non-negative before_sunset and after_sunrise values in minutes.", call. = FALSE)
    }
    if (!requireNamespace("suncalc", quietly = TRUE)) {
      stop("sun_offsets requires the optional suncalc package.", call. = FALSE)
    }
    coordinates <- cities[cities$city == city, , drop = FALSE]
    windows <- do.call(rbind, lapply(all_nights, function(night) {
      sunset <- as.POSIXct(
        suncalc::getSunlightTimes(night, coordinates$latitude, coordinates$longitude, keep = "sunset")$sunset,
        tz = timezone
      )
      sunrise <- as.POSIXct(
        suncalc::getSunlightTimes(night + 1, coordinates$latitude, coordinates$longitude, keep = "sunrise")$sunrise,
        tz = timezone
      )
      start_time <- sunset - abs(sun_offsets[["before_sunset"]]) * 60
      end_time <- sunrise + abs(sun_offsets[["after_sunrise"]]) * 60
      data.frame(Night_Date = night, start_time = start_time, end_time = end_time,
                 duration_hours = as.numeric(difftime(end_time, start_time, units = "hours")))
    }))
  }

  if (is.null(sun_offsets)) {
    window_index <- match(as.character(data$Night_Date), as.character(windows$Night_Date))
    keep <- data$Date_Time >= windows$start_time[window_index] &
      data$Date_Time <= windows$end_time[window_index]
    data <- data[keep, , drop = FALSE]
  }
  output_names <- c("Place", "Id", "contacts", "night_positive", "hour_positive", "minute_positive",
                    "min_CPN", "mean_CPN", "sd_CPN", "max_CPN", "min_CPH", "mean_CPH", "sd_CPH", "max_CPH")
  if (nrow(data) == 0) {
    return(as.data.frame(setNames(replicate(length(output_names), numeric(), simplify = FALSE), output_names)))
  }

  data$Hour <- as.integer(format(data$Date_Time, "%H", tz = timezone))
  data$Minute <- as.integer(format(data$Date_Time, "%M", tz = timezone))
  contacts <- stats::aggregate(
    rep(1L, nrow(data)),
    list(Place = as.character(data$Place), Id = as.character(data$Id), Night_Date = data$Night_Date),
    sum
  )
  names(contacts)[[4]] <- "n_contacts"
  full_data <- merge(
    expand.grid(Place = unique(as.character(data$Place)), Id = unique(as.character(data$Id)),
                Night_Date = all_nights, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE),
    contacts, by = c("Place", "Id", "Night_Date"), all.x = TRUE, sort = FALSE
  )
  full_data$n_contacts[is.na(full_data$n_contacts)] <- 0L
  window_index <- match(as.character(full_data$Night_Date), as.character(windows$Night_Date))
  full_data$contacts_per_hour <- full_data$n_contacts / windows$duration_hours[window_index]

  key <- function(place, id) paste(as.character(place), as.character(id), sep = "\r")
  positive_groups <- split(data, key(data$Place, data$Id), drop = TRUE)
  positives <- do.call(rbind, lapply(positive_groups, function(group) {
    data.frame(
      key = key(group$Place[[1]], group$Id[[1]]),
      hour_positive = length(unique(paste(group$Night_Date, group$Hour))),
      minute_positive = length(unique(paste(group$Night_Date, group$Hour, group$Minute)))
    )
  }))

  groups <- split(full_data, key(full_data$Place, full_data$Id), drop = TRUE)
  result <- do.call(rbind, lapply(groups, function(group) {
    current_key <- key(group$Place[[1]], group$Id[[1]])
    positive <- positives[positives$key == current_key, , drop = FALSE]
    data.frame(
      Place = as.character(group$Place[[1]]), Id = as.character(group$Id[[1]]),
      contacts = sum(group$n_contacts), night_positive = sum(group$n_contacts > 0),
      hour_positive = positive$hour_positive[[1]], minute_positive = positive$minute_positive[[1]],
      min_CPN = min(group$n_contacts), mean_CPN = mean(group$n_contacts),
      sd_CPN = stats::sd(group$n_contacts), max_CPN = max(group$n_contacts),
      min_CPH = min(group$contacts_per_hour), mean_CPH = mean(group$contacts_per_hour),
      sd_CPH = stats::sd(group$contacts_per_hour), max_CPH = max(group$contacts_per_hour),
      stringsAsFactors = FALSE
    )
  }))
  rownames(result) <- NULL
  result
}
