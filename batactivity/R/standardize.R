#' Standardize bat acoustic output tables
#'
#' Converts SonoChiro or Tadarida exports to the BatActivity standard table
#' format used by the analysis functions.
#'
#' @param data A data frame containing a SonoChiro or Tadarida export.
#' @param software Character string. One of `"Tadarida"` or `"SonoChiro"`.
#' @param output_file Optional path where the standardized table is written.
#' @param write_file Logical. Whether to write the standardized table to
#'   `output_file`.
#' @param sep Field separator used when writing `output_file`.
#' @param dec Decimal separator used when writing `output_file`.
#'
#' @return A data frame in BatActivity standard format.
#' @export
standardize_table <- function(data,
                              software = c("Tadarida", "SonoChiro"),
                              output_file = NULL,
                              write_file = !is.null(output_file),
                              sep = ";",
                              dec = ",") {
  if (!is.data.frame(data)) {
    stop("data must be a data.frame.", call. = FALSE)
  }

  software <- match.arg(software)

  if (software == "Tadarida") {
    file_col <- pick_column(data, c("nom.du.fichier", "nom du fichier", "File", "file"))
    observer_col <- pick_column(data, c("observateur_taxon", "observateur.taxon"))
    validator_col <- pick_column(data, c("validateur_taxon", "validateur.taxon"))
    tadarida_col <- pick_column(data, c("tadarida_taxon", "tadarida.taxon"))

    if (is.null(file_col)) {
      stop("No Tadarida file column found.", call. = FALSE)
    }

    File <- as.character(data[[file_col]])
    Place <- sub(
      "^.*-([^-_]+-[^-_]+)_[0-9]{8}_[0-9]{6}.*$",
      "\\1",
      File,
      perl = TRUE
    )
    unmatched_place <- Place == File
    Place[unmatched_place] <- substr(File[unmatched_place], 25, 29)
    Id <- coalesce_text(
      if (!is.null(validator_col)) data[[validator_col]],
      if (!is.null(observer_col)) data[[observer_col]],
      if (!is.null(tadarida_col)) data[[tadarida_col]]
    )
    if (is.null(Id)) {
      stop("No Tadarida species column found.", call. = FALSE)
    }

  } else {
    require_columns(data, c("File", "Id"), "data")

    File <- as.character(data$File)
    Place <- as.character(substr(File, 1, 5))
    Id <- as.character(data$Id)
  }

  timestamp <- regmatches(
    File,
    regexpr("_[0-9]{8}_[0-9]{6}", File, perl = TRUE)
  )
  if (any(timestamp == "")) {
    stop(
      "At least one recording file name does not contain a _YYYYMMDD_HHMMSS timestamp.",
      call. = FALSE
    )
  }

  Year <- as.character(substr(timestamp, 2, 5))
  Month <- as.character(substr(timestamp, 6, 7))
  Day <- as.character(substr(timestamp, 8, 9))
  Hour <- as.character(substr(timestamp, 11, 12))
  Minute <- as.character(substr(timestamp, 13, 14))

  Date <- as.Date(paste(Year, Month, Day, sep = "-"), format = "%Y-%m-%d")
  if (any(is.na(Date))) {
    stop("At least one date could not be parsed from file names.", call. = FALSE)
  }

  Time <- paste(Hour, Minute, sep = ":")
  Date_Time <- as.POSIXct(
    paste(Date, Time),
    format = "%Y-%m-%d %H:%M",
    tz = "Europe/Paris"
  )
  Night_Date <- ifelse(as.numeric(Hour) < 10, Date - 1, Date)
  Night_Date <- as.Date(Night_Date, origin = "1970-01-01")
  Week <- strftime(Night_Date, format = "%V")

  standard_data <- data.frame(
    File = File,
    Place = Place,
    Id = Id,
    Night_Date = Night_Date,
    Date_Time = Date_Time,
    Date = Date,
    Year = Year,
    Month = Month,
    Week = Week,
    Day = Day,
    Time = Time,
    Hour = Hour,
    Minute = Minute,
    stringsAsFactors = FALSE
  )

  if (isTRUE(write_file)) {
    if (is.null(output_file) || !nzchar(output_file)) {
      stop("output_file must be provided when write_file is TRUE.", call. = FALSE)
    }
    utils::write.table(
      standard_data,
      file = output_file,
      row.names = FALSE,
      col.names = TRUE,
      sep = sep,
      dec = dec
    )
  }

  standard_data
}
