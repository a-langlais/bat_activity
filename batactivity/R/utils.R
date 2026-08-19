#' @noRd
pick_column <- function(data, candidates) {
  matched <- candidates[candidates %in% names(data)]
  if (length(matched) == 0) {
    return(NULL)
  }
  matched[[1]]
}

#' @noRd
require_columns <- function(data, columns, data_name = "data") {
  missing_columns <- setdiff(columns, names(data))
  if (length(missing_columns) > 0) {
    stop(
      data_name, " is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' @noRd
coalesce_text <- function(...) {
  values <- list(...)
  values <- Filter(Negate(is.null), values)
  if (length(values) == 0) {
    return(NULL)
  }

  out <- as.character(values[[1]])
  out[out == ""] <- NA_character_
  if (length(values) > 1) {
    for (value in values[-1]) {
      value <- as.character(value)
      value[value == ""] <- NA_character_
      out[is.na(out)] <- value[is.na(out)]
    }
  }
  out
}

#' @noRd
parse_datetime <- function(x, timezone = "Europe/Paris") {
  if (inherits(x, "POSIXct")) {
    return(as.POSIXct(x, tz = timezone))
  }

  x <- trimws(as.character(x))
  x <- sub("\\s+(UTC|GMT|CEST|CET)$", "", x, ignore.case = TRUE)
  parsed <- rep(as.POSIXct(NA, tz = timezone), length(x))

  numeric_value <- suppressWarnings(as.numeric(gsub(",", ".", x)))
  numeric_datetime <- is.na(parsed) & !is.na(numeric_value) & numeric_value > 20000
  parsed[numeric_datetime] <- as.POSIXct(
    (numeric_value[numeric_datetime] - 25569) * 86400,
    origin = "1970-01-01",
    tz = timezone
  )

  formats <- c(
    "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
    "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M",
    "%d-%m-%Y %H:%M:%S", "%d-%m-%Y %H:%M",
    "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M"
  )
  for (format in formats) {
    missing <- is.na(parsed) & !is.na(x) & x != ""
    if (!any(missing)) {
      break
    }
    parsed[missing] <- as.POSIXct(strptime(x[missing], format = format, tz = timezone))
  }

  parsed
}

#' @noRd
parse_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }

  x <- trimws(as.character(x))
  parsed <- rep(as.Date(NA), length(x))
  formats <- c("%Y-%m-%d", "%Y/%m/%d", "%d-%m-%Y", "%d/%m/%Y")
  for (format in formats) {
    missing <- is.na(parsed) & !is.na(x) & x != ""
    if (!any(missing)) {
      break
    }
    parsed[missing] <- as.Date(x[missing], format = format)
  }
  parsed
}

#' @noRd
ceiling_datetime <- function(x, minutes = 10) {
  seconds <- minutes * 60
  as.POSIXct(
    ceiling(as.numeric(x) / seconds) * seconds,
    origin = "1970-01-01",
    tz = attr(x, "tzone")
  )
}

#' @noRd
parse_record_time <- function(record_time) {
  if (!is.character(record_time) || length(record_time) != 2) {
    stop("record_time must be a character vector like c('22:00', '06:00').", call. = FALSE)
  }

  parsed <- strptime(record_time, format = "%H:%M", tz = "UTC")
  if (any(is.na(parsed))) {
    stop("record_time values must use the HH:MM format.", call. = FALSE)
  }

  as.numeric(format(parsed, "%H")) + as.numeric(format(parsed, "%M")) / 60
}

#' @noRd
night_duration_hours <- function(record_time) {
  parsed <- parse_record_time(record_time)
  start_time <- parsed[[1]]
  end_time <- parsed[[2]]

  if (end_time > start_time) {
    end_time - start_time
  } else {
    24 - start_time + end_time
  }
}
