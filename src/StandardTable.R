# =======================================================================================
# Titre: Standardisation des tableaux chiroptérologiques
# Description: Convertit des exports SonoChiro ou Tadarida en table standard BatActivity.
#
# Auteur: Alexandre LANGLAIS
# Date: 2026/05/28
# Version: 2.0
# GitHub : https://github.com/a-langlais/bat_activity
# =======================================================================================

pick_column <- function(table, candidates) {
  matched <- candidates[candidates %in% names(table)]
  if (length(matched) == 0) {
    return(NULL)
  }
  matched[[1]]
}

first_non_empty <- function(...) {
  values <- list(...)
  values <- lapply(values, function(x) {
    if (is.null(x)) {
      return(NULL)
    }
    x[x == ""] <- NA
    x
  })
  values <- Filter(Negate(is.null), values)
  if (length(values) == 0) {
    return(NULL)
  }

  result <- values[[1]]
  if (length(values) > 1) {
    for (value in values[-1]) {
      result <- ifelse(is.na(result), value, result)
    }
  }
  result
}

extract_datetime_parts <- function(file) {
  match <- regexpr("[0-9]{8}_[0-9]{6}", file)
  date_time <- rep(NA_character_, length(file))
  date_time[match > 0] <- regmatches(file, match)

  if (any(is.na(date_time))) {
    stop("Impossible d'extraire la date et l'heure depuis au moins un nom de fichier.")
  }

  data.frame(
    Year = substr(date_time, 1, 4),
    Month = substr(date_time, 5, 6),
    Day = substr(date_time, 7, 8),
    Hour = substr(date_time, 10, 11),
    Minute = substr(date_time, 12, 13),
    stringsAsFactors = FALSE
  )
}

extract_place_from_file <- function(file) {
  place <- sub("_[0-9]{8}_[0-9]{6}.*$", "", file)
  place <- sub("\\.\\w+$", "", place)
  place
}

extract_tadarida_place <- function(file) {
  place <- sub("_[0-9]{8}_[0-9]{6}(_[0-9]+)?$", "", file, perl = TRUE)
  place <- sub("^Car[0-9]+-[0-9]{4}-[^-]+-[^-]+-", "", place, perl = TRUE)
  place[place == ""] <- NA_character_
  place
}

StandardTable <- function(table,
                          sftw = c("Tadarida", "SonoChiro"),
                          table_name = NULL,
                          write_file = !is.null(table_name)) {
  if (!is.data.frame(table)) {
    stop("L'argument 'table' doit être un data.frame.")
  }

  sftw <- match.arg(sftw)

  if (sftw == "Tadarida") {
    file_col <- pick_column(table, c("nom du fichier", "nom.du.fichier", "File", "file"))
    if (is.null(file_col)) {
      stop("Aucune colonne de fichier reconnue pour Tadarida.")
    }

    File <- sub("\\.\\w+$", "", table[[file_col]])
    Place <- extract_tadarida_place(File)

    observer_col <- pick_column(table, c("observateur_taxon", "observateur.taxon"))
    validator_col <- pick_column(table, c("validateur_taxon", "validateur.taxon"))
    tadarida_col <- pick_column(table, c("tadarida_taxon", "tadarida.taxon"))

    Id <- first_non_empty(
      if (!is.null(validator_col)) table[[validator_col]],
      if (!is.null(observer_col)) table[[observer_col]],
      if (!is.null(tadarida_col)) table[[tadarida_col]]
    )
    if (is.null(Id)) {
      stop("Aucune colonne d'espèce reconnue pour Tadarida.")
    }
  }

  if (sftw == "SonoChiro") {
    file_col <- pick_column(table, c("File", "file", "File_name", "File.name", "nom du fichier", "nom.du.fichier"))
    if (is.null(file_col)) {
      stop("Aucune colonne de fichier reconnue pour SonoChiro.")
    }

    File <- sub("\\.\\w+$", "", table[[file_col]])

    place_col <- pick_column(table, c("Place", "Site", "Point", "station"))
    Place <- if (!is.null(place_col)) table[[place_col]] else extract_place_from_file(File)

    id_col <- pick_column(table, c("Id", "ID", "Species", "Taxon", "Espèce", "Espece"))
    if (is.null(id_col)) {
      stop("Aucune colonne d'espèce reconnue pour SonoChiro.")
    }
    Id <- table[[id_col]]
  }

  parts <- extract_datetime_parts(File)

  Date <- as.Date(
    paste(parts$Year, parts$Month, parts$Day, sep = "-"),
    format = "%Y-%m-%d"
  )
  Time <- paste(parts$Hour, parts$Minute, sep = ":")
  Date_Time <- as.POSIXct(
    paste(Date, Time),
    format = "%Y-%m-%d %H:%M",
    tz = "Europe/Paris"
  )
  Night_Date <- ifelse(as.numeric(parts$Hour) < 10, Date - 1, Date)
  Night_Date <- as.Date(Night_Date, origin = "1970-01-01")
  Week <- strftime(Night_Date, format = "%V")

  table_standard <- data.frame(
    File = File,
    Place = Place,
    Id = Id,
    Night_Date = Night_Date,
    Date_Time = format(Date_Time, "%Y-%m-%d %H:%M"),
    Date = Date,
    Year = parts$Year,
    Month = parts$Month,
    Week = Week,
    Day = parts$Day,
    Time = Time,
    Hour = parts$Hour,
    Minute = parts$Minute,
    stringsAsFactors = FALSE
  )

  if (write_file) {
    write.table(table_standard, file = table_name, row.names = FALSE, col.names = TRUE, sep = ";", dec = ",")
  }

  table_standard
}

# Ancien nom conservé pour compatibilité avec les scripts et le README existants.
TableFormatage <- StandardTable
