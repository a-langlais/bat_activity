#!/usr/bin/env Rscript

# Standardise several SonoChiro or Tadarida exports in one command.
#
# Example:
#   Rscript scripts/03_standardiser_lot.R SonoChiro data/nuit_1.csv data/nuit_2.csv

usage <- function() {
  cat(
    "Usage:\n",
    "  Rscript scripts/03_standardiser_lot.R <SonoChiro|Tadarida> <fichier1.csv> [fichier2.csv ...]\n\n",
    "  Rscript scripts/03_standardiser_lot.R <SonoChiro|Tadarida> --dossier <dossier>\n\n",
    "Each result is written next to its source file, with the suffix _format.\n",
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

read_export <- function(file) {
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

software_values <- c("Tadarida", "SonoChiro")
software <- software_values[tolower(software_values) == tolower(arguments[[1]])]
if (length(software) == 0) {
  usage()
  stop("The first argument must be SonoChiro or Tadarida.", call. = FALSE)
}

input_arguments <- arguments[-1]
if (identical(input_arguments[[1]], "--dossier")) {
  if (length(input_arguments) != 2 || !dir.exists(input_arguments[[2]])) {
    usage()
    stop("--dossier must be followed by an existing directory.", call. = FALSE)
  }

  input_files <- list.files(
    input_arguments[[2]],
    pattern = "_clean\\.csv$",
    full.names = TRUE,
    ignore.case = TRUE
  )
} else {
  input_files <- input_arguments
}

if (length(input_files) == 0) {
  usage()
  stop("No input CSV file was found.", call. = FALSE)
}

source(file.path("batactivity", "R", "utils.R"))
source(file.path("batactivity", "R", "standardize.R"))

failures <- character()
for (input_file in input_files) {
  if (!file.exists(input_file)) {
    failures <- c(failures, paste0(input_file, ": file not found"))
    next
  }

  output_file <- file.path(
    dirname(input_file),
    paste0(tools::file_path_sans_ext(basename(input_file)), "_format.csv")
  )

  result <- tryCatch(
    {
      raw_data <- read_export(input_file)
      standardize_table(
        raw_data,
        software = software,
        output_file = output_file
      )
    },
    error = function(error) error
  )

  if (inherits(result, "error")) {
    failures <- c(failures, paste0(input_file, ": ", conditionMessage(result)))
  } else {
    message("Standardized: ", output_file, " (", nrow(result), " rows)")
  }
}

if (length(failures) > 0) {
  message("\nFiles not standardized:")
  message(paste0("- ", failures, collapse = "\n"))
  quit(status = 1)
}
