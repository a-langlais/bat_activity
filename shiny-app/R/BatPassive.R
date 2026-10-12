# Passive activity indicators used by the Shiny application.
#
# This implementation intentionally follows batactivity::species_place_activity:
# solar ephemerides determine the CPH denominator only; they do not discard
# contacts. Keeping the calculation here dependency-free makes the application
# usable without installing the package.

BatPassive <- function(data,
                       city = "Paris",
                       record_time = c("08:00", "22:00"),
                       sun_offsets = NULL,
                       timezone = "Europe/Paris") {
  required_columns <- c("Place", "Id", "Night_Date", "Date_Time")
  missing_columns <- setdiff(required_columns, names(data))
  if (!is.data.frame(data) || length(missing_columns) > 0) {
    stop("data must be a data frame containing: ", paste(required_columns, collapse = ", "), call. = FALSE)
  }
  if (nrow(data) == 0) stop("data must contain at least one row.", call. = FALSE)
  if (!is.character(timezone) || length(timezone) != 1 || !timezone %in% OlsonNames()) {
    stop("timezone must be a valid IANA time zone, such as Europe/Paris.", call. = FALSE)
  }

  cities <- data.frame(
    city = c("Paris", "Lyon", "Marseille", "Toulouse", "Bordeaux", "Brest", "Strasbourg"),
    latitude = c(48.8566, 45.7640, 43.2965, 43.6047, 44.8378, 48.3904, 48.5734),
    longitude = c(2.3522, 4.8357, 5.3698, 1.4442, -0.5792, -4.4861, 7.7521),
    stringsAsFactors = FALSE
  )
  if (!is.character(city) || length(city) != 1 || !city %in% cities$city) {
    stop("city must be one of: ", paste(cities$city, collapse = ", "), call. = FALSE)
  }

  parse_datetime_app <- function(x) {
    if (inherits(x, "POSIXct")) return(as.POSIXct(x, tz = timezone))
    values <- trimws(as.character(x))
    values <- sub("\\s+(UTC|GMT|CEST|CET)$", "", values, ignore.case = TRUE)
    result <- rep(as.POSIXct(NA, tz = timezone), length(values))
    numeric_value <- suppressWarnings(as.numeric(gsub(",", ".", values)))
    numeric_datetime <- is.na(result) & !is.na(numeric_value) & numeric_value > 20000
    result[numeric_datetime] <- as.POSIXct(
      (numeric_value[numeric_datetime] - 25569) * 86400,
      origin = "1970-01-01",
      tz = timezone
    )
    formats <- c(
      "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
      "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M",
      "%d-%m-%Y %H:%M:%S", "%d-%m-%Y %H:%M"
    )
    for (format in formats) {
      missing <- is.na(result) & !is.na(values) & values != ""
      if (!any(missing)) break
      result[missing] <- as.POSIXct(strptime(values[missing], format = format, tz = timezone))
    }
    result
  }

  data$Night_Date <- as.Date(data$Night_Date)
  data$Date_Time <- parse_datetime_app(data$Date_Time)
  if (any(is.na(data$Night_Date)) || any(is.na(data$Date_Time))) {
    stop("Night_Date or Date_Time contains unparsable values.", call. = FALSE)
  }

  all_nights <- seq(min(data$Night_Date), max(data$Night_Date), by = "day")
  if (is.null(sun_offsets)) {
    if (!is.character(record_time) || length(record_time) != 2 ||
        any(is.na(strptime(record_time, format = "%H:%M", tz = "UTC")))) {
      stop("record_time must contain two values in HH:MM format.", call. = FALSE)
    }
    start_time <- as.POSIXct(paste(all_nights, record_time[[1]]), tz = timezone)
    end_time <- as.POSIXct(paste(all_nights + 1, record_time[[2]]), tz = timezone)
    windows <- data.frame(
      Night_Date = all_nights,
      start_time = start_time,
      end_time = end_time,
      duration_hours = as.numeric(difftime(end_time, start_time, units = "hours"))
    )
  } else {
    required_offsets <- c("before_sunset", "after_sunrise")
    if (!is.numeric(sun_offsets) || !all(required_offsets %in% names(sun_offsets)) ||
        any(is.na(sun_offsets[required_offsets])) || any(sun_offsets[required_offsets] < 0)) {
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
      start_time <- sunset - sun_offsets[["before_sunset"]] * 60
      end_time <- sunrise + sun_offsets[["after_sunrise"]] * 60
      data.frame(
        Night_Date = night,
        start_time = start_time,
        end_time = end_time,
        duration_hours = as.numeric(difftime(end_time, start_time, units = "hours"))
      )
    }))
  }

  if (is.null(sun_offsets)) {
    window_index <- match(as.character(data$Night_Date), as.character(windows$Night_Date))
    data <- data[data$Date_Time >= windows$start_time[window_index] &
                   data$Date_Time <= windows$end_time[window_index], , drop = FALSE]
  }

  output_names <- c(
    "Place", "Id", "contacts", "night_positive", "hour_positive", "minute_positive",
    "min_CPN", "mean_CPN", "sd_CPN", "max_CPN", "min_CPH", "mean_CPH", "sd_CPH", "max_CPH"
  )
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

  group_key <- function(place, id) paste(as.character(place), as.character(id), sep = "\r")
  positive_groups <- split(data, group_key(data$Place, data$Id), drop = TRUE)
  positives <- do.call(rbind, lapply(positive_groups, function(group) {
    data.frame(
      key = group_key(group$Place[[1]], group$Id[[1]]),
      hour_positive = length(unique(paste(group$Night_Date, group$Hour))),
      minute_positive = length(unique(paste(group$Night_Date, group$Hour, group$Minute)))
    )
  }))

  result_groups <- split(full_data, group_key(full_data$Place, full_data$Id), drop = TRUE)
  result <- do.call(rbind, lapply(result_groups, function(group) {
    current_key <- group_key(group$Place[[1]], group$Id[[1]])
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
