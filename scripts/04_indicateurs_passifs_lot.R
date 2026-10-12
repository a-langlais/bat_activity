#!/usr/bin/env Rscript

# Calculate passive activity indicators using the same methodology as Shiny.
#
# Example:
#   Rscript scripts/04_indicateurs_passifs_lot.R --horaires 20:00 07:00 --dossier data

usage <- function() {
  cat(
    "Usage:\n",
    "  Rscript scripts/04_indicateurs_passifs_lot.R [--ville VILLE] [--fuseau TZ] [--horaires HH:MM HH:MM] <fichier1.csv> [fichier2.csv ...]\n",
    "  Rscript scripts/04_indicateurs_passifs_lot.R [--ville VILLE] [--fuseau TZ] [--horaires HH:MM HH:MM] --dossier <dossier>\n",
    "  Rscript scripts/04_indicateurs_passifs_lot.R [--ville VILLE] [--fuseau TZ] --ephemerides MIN_AVANT_COUCHER MIN_APRES_LEVER --dossier <dossier>\n\n",
    "Inputs must be standardized tables ending in _clean_format.csv.\n",
    "Each result is written next to its source file, with the suffix _raw_results.\n",
    sep = ""
  )
}

detect_separator <- function(file) {
  first_line <- readLines(file, n = 1, warn = FALSE, encoding = "UTF-8")
  if (length(first_line) == 0) {
    stop("The file is empty.", call. = FALSE)
  }

  separators <- c(";", ",", "\t")
  counts <- vapply(
    separators,
    function(separator) lengths(regmatches(first_line, gregexpr(separator, first_line, fixed = TRUE))),
    integer(1)
  )
  if (max(counts) == 0) {
    stop("No CSV separator (;, , or tab) could be detected.", call. = FALSE)
  }

  separators[[which.max(counts)]]
}

read_standard_table <- function(file) {
  utils::read.table(
    file = file,
    header = TRUE,
    sep = detect_separator(file),
    quote = "\"",
    comment.char = "",
    fill = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) == 0 || arguments[[1]] %in% c("-h", "--help")) {
  usage()
  quit(status = if (length(arguments) == 0) 1 else 0)
}

city <- "Paris"
timezone <- "Europe/Paris"
record_time <- c("08:00", "22:00")
sun_offsets <- NULL
fixed_time_supplied <- FALSE
solar_time_supplied <- FALSE
input_files <- character()

while (length(arguments) > 0) {
  argument <- arguments[[1]]

  if (argument %in% c("--ville", "--city")) {
    if (length(arguments) < 2) {
      usage()
      stop(argument, " must be followed by a city name.", call. = FALSE)
    }
    city <- arguments[[2]]
    arguments <- arguments[-c(1, 2)]
  } else if (argument %in% c("--fuseau", "--timezone")) {
    if (length(arguments) < 2) {
      usage()
      stop(argument, " must be followed by an IANA time zone, such as Europe/Paris.", call. = FALSE)
    }
    timezone <- arguments[[2]]
    arguments <- arguments[-c(1, 2)]
  } else if (argument %in% c("--horaires", "--record-time")) {
    if (length(arguments) < 3) {
      usage()
      stop(argument, " must be followed by a start and end time.", call. = FALSE)
    }
    record_time <- arguments[2:3]
    fixed_time_supplied <- TRUE
    arguments <- arguments[-c(1, 2, 3)]
  } else if (argument %in% c("--ephemerides", "--sun-offsets")) {
    if (length(arguments) < 3) {
      usage()
      stop(argument, " must be followed by two offsets in minutes.", call. = FALSE)
    }
    offsets <- suppressWarnings(as.numeric(arguments[2:3]))
    if (any(is.na(offsets)) || any(offsets < 0)) {
      stop(argument, " values must be positive minutes.", call. = FALSE)
    }
    sun_offsets <- c(before_sunset = offsets[[1]], after_sunrise = offsets[[2]])
    solar_time_supplied <- TRUE
    arguments <- arguments[-c(1, 2, 3)]
  } else if (argument == "--dossier") {
    if (length(arguments) < 2 || !dir.exists(arguments[[2]])) {
      usage()
      stop("--dossier must be followed by an existing directory.", call. = FALSE)
    }
    folder_files <- list.files(
      arguments[[2]],
      pattern = "_clean_format\\.csv$",
      full.names = TRUE,
      ignore.case = TRUE
    )
    input_files <- c(input_files, folder_files)
    arguments <- arguments[-c(1, 2)]
  } else {
    input_files <- c(input_files, argument)
    arguments <- arguments[-1]
  }
}

input_files <- unique(input_files)
if (length(input_files) == 0) {
  usage()
  stop("No standardized input CSV file was found.", call. = FALSE)
}
if (fixed_time_supplied && solar_time_supplied) {
  stop("Use either --horaires or --ephemerides, not both.", call. = FALSE)
}
supported_cities <- c("Paris", "Lyon", "Marseille", "Toulouse", "Bordeaux", "Brest", "Strasbourg")
if (!city %in% supported_cities) {
  stop(
    "--ville must be one of: ",
    paste(supported_cities, collapse = ", "),
    call. = FALSE
  )
}
if (!timezone %in% OlsonNames()) {
  stop("--fuseau must be a valid IANA time zone, such as Europe/Paris.", call. = FALSE)
}

source(file.path("batactivity", "R", "utils.R"))
source(file.path("batactivity", "R", "passive.R"))

failures <- character()
for (input_file in input_files) {
  if (!file.exists(input_file)) {
    failures <- c(failures, paste0(input_file, ": file not found"))
    next
  }

  input_stem <- tools::file_path_sans_ext(basename(input_file))
  output_stem <- sub("_clean_format$", "_raw_results", input_stem)
  if (identical(output_stem, input_stem)) {
    failures <- c(failures, paste0(input_file, ": filename must end in _clean_format.csv"))
    next
  }
  output_file <- file.path(dirname(input_file), paste0(output_stem, ".csv"))

  result <- tryCatch(
    {
      standard_data <- read_standard_table(input_file)
      species_place_activity(
        standard_data,
        city = city,
        record_time = record_time,
        sun_offsets = sun_offsets,
        timezone = timezone
      )
    },
    error = function(error) error
  )

  if (inherits(result, "error")) {
    failures <- c(failures, paste0(input_file, ": ", conditionMessage(result)))
  } else {
    utils::write.table(
      result,
      file = output_file,
      row.names = FALSE,
      col.names = TRUE,
      sep = ";",
      dec = ","
    )
    message("Passive indicators: ", output_file, " (", nrow(result), " rows)")
  }
}

if (length(failures) > 0) {
  message("\nFiles not processed:")
  message(paste0("- ", failures, collapse = "\n"))
  quit(status = 1)
}
