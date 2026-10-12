#!/usr/bin/env Rscript

# Apply species-specific detectability coefficients to passive indicators.
#
# Example:
#   Rscript scripts/05_correction_detectabilite.R --milieu ouvert --dossier data

usage <- function() {
  cat(
    "Usage:\n",
    "  Rscript scripts/05_correction_detectabilite.R --milieu <ouvert|ferme> <fichier1.csv> [fichier2.csv ...]\n",
    "  Rscript scripts/05_correction_detectabilite.R --milieu <ouvert|ferme> --dossier <dossier>\n\n",
    "Inputs must be passive indicator files ending in _raw_results.csv.\n",
    "The corrected file preserves the exact same columns as its input.\n",
    sep = ""
  )
}

detect_separator <- function(file) {
  first_line <- readLines(file, n = 1, warn = FALSE, encoding = "UTF-8")
  if (length(first_line) == 0) stop("The file is empty.", call. = FALSE)

  separators <- c(";", ",", "\t")
  counts <- vapply(
    separators,
    function(separator) lengths(regmatches(first_line, gregexpr(separator, first_line, fixed = TRUE))),
    integer(1)
  )
  if (max(counts) == 0) stop("No CSV separator (;, , or tab) could be detected.", call. = FALSE)
  separators[[which.max(counts)]]
}

read_indicators <- function(file) {
  separator <- detect_separator(file)
  utils::read.table(
    file = file,
    header = TRUE,
    sep = separator,
    dec = if (separator == ";") "," else ".",
    quote = "\"",
    comment.char = "",
    fill = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

coefficients <- data.frame(
  Id = c(
    "Barbar", "Eptser", "Minsch", "Myoalc", "Myobec", "Myodau", "Myoema",
    "Myomyo", "Myomys", "Myonat", "Nyclei", "Nycnoc", "Pipkuh", "Pipnat",
    "Pippip", "Pippyg", "Pleaur", "Pleaus", "Rhifer", "Rhihip"
  ),
  ouvert = c(
    1.70, 0.71, 0.83, 2.50, 1.70, 1.20, 2.50, 1.20, 2.50, 1.70,
    0.31, 0.25, 0.83, 0.83, 0.83, 1.00, 0.71, 0.71, 2.50, 5.00
  ),
  ferme = c(
    1.70, 0.83, 1.20, 2.50, 2.50, 1.20, 3.10, 1.70, 2.50, 3.10,
    0.31, 0.25, 1.00, 1.00, 1.00, 1.20, 5.00, 5.00, 2.50, 5.00
  ),
  stringsAsFactors = FALSE
)

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) == 0 || arguments[[1]] %in% c("-h", "--help")) {
  usage()
  quit(status = if (length(arguments) == 0) 1 else 0)
}

milieu <- NULL
input_files <- character()
while (length(arguments) > 0) {
  argument <- arguments[[1]]

  if (argument == "--milieu") {
    if (length(arguments) < 2) {
      usage()
      stop("--milieu must be followed by ouvert or ferme.", call. = FALSE)
    }
    milieu <- tolower(arguments[[2]])
    arguments <- arguments[-c(1, 2)]
  } else if (argument == "--dossier") {
    if (length(arguments) < 2 || !dir.exists(arguments[[2]])) {
      usage()
      stop("--dossier must be followed by an existing directory.", call. = FALSE)
    }
    input_files <- c(
      input_files,
      list.files(
        arguments[[2]],
        pattern = "_raw_results\\.csv$",
        full.names = TRUE,
        ignore.case = TRUE
      )
    )
    arguments <- arguments[-c(1, 2)]
  } else {
    input_files <- c(input_files, argument)
    arguments <- arguments[-1]
  }
}

if (is.null(milieu) || !milieu %in% c("ouvert", "ferme")) {
  usage()
  stop("--milieu must be ouvert or ferme.", call. = FALSE)
}
input_files <- unique(input_files)
if (length(input_files) == 0) {
  usage()
  stop("No passive indicator file was found.", call. = FALSE)
}

quantitative_indicators <- c(
  "contacts", "min_CPN", "mean_CPN", "sd_CPN", "max_CPN",
  "min_CPH", "mean_CPH", "sd_CPH", "max_CPH"
)
failures <- character()
for (input_file in input_files) {
  if (!file.exists(input_file)) {
    failures <- c(failures, paste0(input_file, ": file not found"))
    next
  }

  result <- tryCatch({
    indicators <- read_indicators(input_file)
    if (!"Id" %in% names(indicators)) {
      stop("The file is missing the Id column.", call. = FALSE)
    }

    indicator_columns <- intersect(quantitative_indicators, names(indicators))
    if (length(indicator_columns) == 0) {
      stop("No quantitative passive indicator column was found.", call. = FALSE)
    }

    taxon_id <- trimws(as.character(indicators$Id))
    coefficient <- coefficients[[milieu]][match(taxon_id, coefficients$Id)]
    missing_taxa <- unique(indicators$Id[is.na(coefficient)])
    correction_factor <- coefficient
    correction_factor[is.na(correction_factor)] <- 1

    for (column in indicator_columns) {
      indicators[[column]] <- as.numeric(indicators[[column]]) * correction_factor
    }
    attr(indicators, "missing_taxa") <- missing_taxa

    indicators
  }, error = function(error) error)

  if (inherits(result, "error")) {
    failures <- c(failures, paste0(input_file, ": ", conditionMessage(result)))
    next
  }

  input_stem <- tools::file_path_sans_ext(basename(input_file))
  output_stem <- sub("_raw_results$", "_corrected_results", input_stem)
  if (identical(output_stem, input_stem)) {
    failures <- c(failures, paste0(input_file, ": filename must end in _raw_results.csv"))
    next
  }
  output_file <- file.path(dirname(input_file), paste0(output_stem, ".csv"))
  utils::write.table(
    result,
    file = output_file,
    row.names = FALSE,
    col.names = TRUE,
    sep = ";",
    dec = ","
  )
  missing_taxa <- attr(result, "missing_taxa")
  if (length(missing_taxa) > 0) {
    warning(
      "No detectability coefficient for ", basename(input_file), ": ",
      paste(missing_taxa, collapse = ", "),
      ". Their values were retained without correction.",
      call. = FALSE
    )
  }
  message("Corrected indicators: ", output_file)
}

if (length(failures) > 0) {
  message("\nFiles not processed:")
  message(paste0("- ", failures, collapse = "\n"))
  quit(status = 1)
}
