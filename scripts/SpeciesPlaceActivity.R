# =======================================================================================
# Titre: Analyse de l'Activit� Chiropt�rologique par site et par esp�ce
# Description: Ce script R est con�u pour analyser l'activit� chiropt�rologique en
#              calculant des indicateurs sp�cifiques tels que le nombre d'esp�ces
#              d�tect�es, le nombre total de contacts, la fr�quence des contacts par heure
#              et par nuit.
#
# Auteur: Alexandre LANGLAIS
# Date: 2024/05/03
# Version: 1.1
# GitHub : https://github.com/a-langlais/bat_activity
# D�pendances: dplyr, lubridate
#
# Instructions: Ce script d�finit une fonction SpeciesPlaceActivity qui prend en entr�e une
#               table de donn�es sur les observations chiropt�rologiques, le nombre de nuits
#               enregistr�es et l'heure de d�part et de fin des enregistrements. Le tableau
#               doit d'abord �tre standardis� via la fonction pr�vue � cet effet.
#
#               ex : SpeciesPlaceActivity(data = MaTable, nights = 1, record_time = c("22:00", "06:00"))
# =======================================================================================

SpeciesPlaceActivity <- function(data, nights = 1, record_time = c("22:00", "06:00")) {
  
  library(dplyr)
  library(lubridate)
  
  # V�rifier la pr�sence des colonnes n�cessaires
  required_columns <- c("Place", "Id", "Night_Date", "Date_Time", "Year", "Month", "Week", "Day", "Time", "Hour", "Minute")
  missing_columns <- required_columns[!required_columns %in% names(data)]
  if (length(missing_columns) > 0) {
    stop("Les colonnes suivantes sont manquantes dans le dataframe: ", paste(missing_columns, collapse = ", "), ".")
  }
  if (nights <= 0) {
    stop("Le nombre de nuits doit �tre > 0")
  }
  
  # Pr�paration des donn�es
  data <- data %>%
    mutate(Night_Date = as.Date(Night_Date),
           Date_Time = as.POSIXct(Date_Time),
           Hour = as.numeric(Hour),
           Minute = as.numeric(Minute))
  
  # Calculer la dur�e de la nuit en heures
  start_time <- as.numeric(strptime(record_time[1], format = "%H:%M")) 
  end_time <- as.numeric(strptime(record_time[2], format = "%H:%M"))
  night_duration <- ifelse(end_time > start_time, end_time - start_time, 24 * 3600 - (start_time - end_time)) / 3600
  
  # Regrouper les donn�es par site et esp�ce
  results <- data %>%
    group_by(Place, Id) %>%
    summarise(
      contacts = n(),
      night_positive = n_distinct(Night_Date),
      hour_positive = length(unique(paste(Night_Date, Hour))),
      minute_positive = length(unique(paste(Night_Date, Hour, Minute))),
      # Calcul des indices CPN
      min_CPN = min(table(Night_Date)),
      mean_CPN = mean(table(Night_Date)),
      sd_CPN = sd(table(Night_Date)),
      max_CPN = max(table(Night_Date)),
      # Calcul des indices CPH
      min_CPH = min(table(paste(Night_Date, Hour))),
      mean_CPH = mean(table(paste(Night_Date, Hour))) / (nights * night_duration),
      sd_CPH = sd(table(paste(Night_Date, Hour))) / (nights * night_duration),
      max_CPH = max(table(paste(Night_Date, Hour))),
      .groups = 'drop'
    )
  
  return(results)
}
