#' Prepare data for threshold modelling
#'
#' Joins standardized bat contacts and weather data at a common time step and
#' creates the binary `is_contacted` response used by threshold models.
#'
#' @param data A standard bat contact data frame with `Date_Time` and `Id`.
#' @param weather A weather data frame with `Date_Time` and weather variables.
#' @param variables Character vector of weather variables to keep.
#' @param dates Optional vector of start and end dates.
#' @param night_hours Numeric vector of night start and end hours.
#' @param timezone Timezone used to parse datetimes.
#'
#' @return A merged data frame ready for threshold modelling.
#' @export
prepare_threshold_data <- function(data,
                                   weather,
                                   variables,
                                   dates = NULL,
                                   night_hours = c(20, 6),
                                   timezone = "Europe/Paris") {
  if (!is.data.frame(data)) {
    stop("data must be a data.frame.", call. = FALSE)
  }
  if (!is.data.frame(weather)) {
    stop("weather must be a data.frame.", call. = FALSE)
  }
  require_columns(data, c("Date_Time", "Id"), "data")
  require_columns(weather, c("Date_Time", variables), "weather")

  if (!is.character(variables) || length(variables) == 0) {
    stop("variables must be a non-empty character vector.", call. = FALSE)
  }
  if (!is.numeric(night_hours) || length(night_hours) != 2) {
    stop("night_hours must be a numeric vector of length 2.", call. = FALSE)
  }
  if (!is.null(dates) && length(dates) != 2) {
    stop("dates must be NULL or a vector of length 2.", call. = FALSE)
  }

  data$Date_Time <- ceiling_datetime(parse_datetime(data$Date_Time, timezone), minutes = 10)
  weather$Date_Time <- parse_datetime(weather$Date_Time, timezone)

  if (any(is.na(data$Date_Time)) || any(is.na(weather$Date_Time))) {
    stop("Date_Time contains values that cannot be parsed.", call. = FALSE)
  }

  if (!is.null(dates)) {
    parsed_dates <- parse_date(dates)
    if (any(is.na(parsed_dates))) {
      stop("dates contains values that cannot be parsed.", call. = FALSE)
    }
    start_date <- as.POSIXct(parsed_dates[[1]], tz = timezone)
    end_date <- as.POSIXct(parsed_dates[[2]] + 1, tz = timezone) - 1
    data <- data[data$Date_Time >= start_date & data$Date_Time <= end_date, , drop = FALSE]
    weather <- weather[weather$Date_Time >= start_date & weather$Date_Time <= end_date, , drop = FALSE]
  }

  hour_value <- as.numeric(format(weather$Date_Time, "%H"))
  if (night_hours[[1]] <= night_hours[[2]]) {
    keep_night <- hour_value >= night_hours[[1]] & hour_value < night_hours[[2]]
  } else {
    keep_night <- hour_value >= night_hours[[1]] | hour_value < night_hours[[2]]
  }
  weather <- weather[keep_night, , drop = FALSE]

  merged <- merge(data, weather, by = "Date_Time", all = TRUE)
  if (nrow(merged) == 0) {
    stop("Merging data and weather produced an empty data.frame.", call. = FALSE)
  }

  merged$is_contacted <- ifelse(!is.na(merged$Id), 1, 0)
  merged
}

#' Calculate weather thresholds for bat activity
#'
#' Fits a logistic regression linking bat contact presence to weather variables
#' and returns model coefficients with requested weather percentiles.
#'
#' @param data A standard bat contact data frame.
#' @param weather A weather data frame.
#' @param variables Character vector of weather variables to model.
#' @param dates Optional vector of start and end dates.
#' @param percent Percentile to calculate for each variable.
#' @param make_plot Logical. Whether to also print diagnostic plots.
#' @param night_hours Numeric vector of night start and end hours.
#' @param timezone Timezone used to parse datetimes.
#' @param return_model Logical. Whether to return model and prepared data.
#'
#' @return A data frame of model coefficients and percentiles, or a list when
#'   `return_model = TRUE`.
#' @export
calculate_threshold <- function(data,
                                weather,
                                variables,
                                dates = NULL,
                                percent = 95,
                                make_plot = FALSE,
                                night_hours = c(20, 6),
                                timezone = "Europe/Paris",
                                return_model = FALSE) {
  if (!is.numeric(percent) || length(percent) != 1 || is.na(percent) || percent < 0 || percent > 100) {
    stop("percent must be a number between 0 and 100.", call. = FALSE)
  }

  threshold_data <- prepare_threshold_data(
    data = data,
    weather = weather,
    variables = variables,
    dates = dates,
    night_hours = night_hours,
    timezone = timezone
  )

  percentiles <- do.call(rbind, lapply(variables, function(variable) {
    quantiles <- stats::quantile(
      threshold_data[[variable]],
      probs = c(percent / 100, 1 - percent / 100),
      na.rm = TRUE,
      names = FALSE
    )
    data.frame(
      variable = variable,
      percentile = quantiles[[1]],
      inverse_percentile = quantiles[[2]],
      stringsAsFactors = FALSE
    )
  }))

  model_formula <- stats::as.formula(paste("is_contacted ~", paste(variables, collapse = " + ")))
  model <- stats::glm(model_formula, data = threshold_data, family = stats::binomial(link = "logit"))
  coefficients <- summary(model)$coefficients
  if (any(is.na(coefficients))) {
    warning("Some model coefficients are NA; check data completeness and variable collinearity.", call. = FALSE)
  }

  results <- data.frame(
    variable = rownames(coefficients),
    coefficient = coefficients[, 1],
    std_error = coefficients[, 2],
    z_value = coefficients[, 3],
    p_value = coefficients[, 4],
    stringsAsFactors = FALSE
  )
  results <- merge(results, percentiles, by = "variable", all.x = TRUE)

  if (isTRUE(make_plot)) {
    plot_threshold(threshold_data, variables)
  }

  if (isTRUE(return_model)) {
    return(list(results = results, model = model, data = threshold_data))
  }

  results
}

#' Plot threshold diagnostics
#'
#' Creates one logistic diagnostic plot per weather variable.
#'
#' @param threshold_data Data prepared by `prepare_threshold_data()`.
#' @param variables Character vector of variables to plot.
#'
#' @return Invisibly returns a named list of ggplot objects.
#' @export
plot_threshold <- function(threshold_data, variables) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required to plot threshold diagnostics.", call. = FALSE)
  }
  require_columns(threshold_data, c("is_contacted", variables), "threshold_data")

  plots <- lapply(variables, function(variable) {
    plot_data <- data.frame(
      value = threshold_data[[variable]],
      is_contacted = threshold_data$is_contacted
    )
    ggplot2::ggplot(plot_data, ggplot2::aes_string(x = "value", y = "is_contacted")) +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::geom_smooth(
        method = "glm",
        method.args = list(family = stats::binomial()),
        se = FALSE,
        color = "red"
      ) +
      ggplot2::labs(
        title = paste("Bat contacts vs", variable),
        x = variable,
        y = "Contact probability"
      ) +
      ggplot2::theme_minimal()
  })
  names(plots) <- variables
  print(plots)
  invisible(plots)
}
