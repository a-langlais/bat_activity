#' Backward-compatible wrapper for `standardize_table()`
#'
#' @param table A data frame to standardize.
#' @param sftw Character string identifying the input software.
#' @param output_file Optional path where the standardized table is written.
#' @param write_file Logical. Whether to write the standardized table to
#'   `output_file`.
#' @param sep Field separator used when writing `output_file`.
#' @param dec Decimal separator used when writing `output_file`.
#'
#' @return A data frame in BatActivity standard format.
#' @export
TableFormatage <- function(table,
                           sftw = "Tadarida",
                           output_file = NULL,
                           write_file = !is.null(output_file),
                           sep = ";",
                           dec = ",") {
  standardize_table(
    data = table,
    software = sftw,
    output_file = output_file,
    write_file = write_file,
    sep = sep,
    dec = dec
  )
}

#' Backward-compatible wrapper for `bat_active()`
#'
#' @param table A standard active survey data frame.
#' @param duration Listening duration in minutes for each point.
#' @param npoint Number of sampled points.
#'
#' @return A data frame of active survey indicators.
#' @export
BatActive <- function(table, duration, npoint) {
  bat_active(data = table, duration = duration, npoint = npoint)
}

#' Backward-compatible wrapper for `species_place_activity()`
#'
#' @inheritParams species_place_activity
#'
#' @return A data frame of passive activity indicators.
#' @export
SpeciesPlaceActivity <- function(data, nights = 1, record_time = c("22:00", "06:00")) {
  species_place_activity(data = data, nights = nights, record_time = record_time)
}

#' Backward-compatible wrapper for `calculate_threshold()`
#'
#' @param data A standard bat contact data frame.
#' @param meteo A weather data frame.
#' @param dates Optional vector of start and end dates.
#' @param var Character vector of weather variables.
#' @param percent Percentile to calculate for each variable.
#' @param plot Logical. Whether to print diagnostic plots.
#'
#' @return A data frame of threshold model results.
#' @export
CalculateThreshold <- function(data, meteo, dates = NULL, var, percent = 95, plot = FALSE) {
  calculate_threshold(
    data = data,
    weather = meteo,
    variables = var,
    dates = dates,
    percent = percent,
    make_plot = plot
  )
}

#' Backward-compatible wrapper for `rename_audio_files()`
#'
#' @param list Character vector of audio file paths.
#' @param dry_run Logical. When `TRUE`, return the rename plan without changing
#'   files.
#' @param output_dir Optional directory for renamed files. Defaults to each
#'   file's current directory.
#'
#' @return A data frame with old paths, new paths, and rename status.
#' @export
list.renamer <- function(list, dry_run = TRUE, output_dir = NULL) {
  rename_audio_files(files = list, output_dir = output_dir, dry_run = dry_run)
}

#' Backward-compatible wrapper for microphone test plotting
#'
#' @param file_path Path to the microphone test CSV file.
#' @param interactive Logical. Whether to open a file chooser when `file_path`
#'   is missing.
#'
#' @return A ggplot object.
#' @export
print_Signal <- function(file_path, interactive = FALSE) {
  if (missing(file_path)) {
    if (!isTRUE(interactive)) {
      stop("file_path must be provided. Use interactive = TRUE to open a file chooser.", call. = FALSE)
    }
    file_path <- file.choose()
  }

  test <- read_microphone_test(file_path)
  graph <- plot_microphone_test(test)
  message("Microphone test plot produced: ", test$title)
  graph
}
