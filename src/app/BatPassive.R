# =======================================================================================
# Titre: Fonction BatPassive - Analyse des données d'activité chiroptérologique passive
# Description:  Fonction R destinée à analyser les données d'enregistrement passif d'activité
#               chiroptérologique. Elle traite un jeu de données contenant les enregistrements horaires,
#               calcule des indicateurs d'activité nocturne (contacts, occurrences par heure, etc.) 
#               en prenant en compte la localisation géographique et la période d'enregistrement.
#
# Auteur: Alexandre LANGLAIS
# Date: 2025/06/15
# Version: 1.0
# GitHub : https://github.com/a-langlais/bat_activity
# Dépendances: dplyr, lubridate, suncalc, tibble
#
# Paramètres:
#   - data : data.frame ou tibble contenant les données d'activité avec colonnes obligatoires 
#            (Place, Id, Night_Date, Date_Time)
#   - city : caractère, nom de la ville pour les coordonnées géographiques (par défaut "Paris")
#   - record_time : vecteur de 2 chaînes "HH:MM" définissant la plage horaire d'enregistrement 
#                   fixe si aucun offset n'est donné (par défaut c("22:00", "06:00"))
#   - sun_offsets : liste ou vecteur nommé avec décalages en minutes (before_sunset, after_sunrise) 
#                   pour calculer dynamiquement la période nocturne selon le coucher et lever du soleil
#
# =======================================================================================

BatPassive <- function(data, 
                       city = "Paris",
                       record_time = c("22:00", "06:00"), 
                       sun_offsets = NULL) {
  
  library(dplyr)      # 1.1.4
  library(lubridate)  # 1.9.4
  library(suncalc)    # 0.5.1
  library(tibble)     # 3.2.1
  
  # Table interne des villes avec coordonnées
  cities_coords <- tibble(
    Place = c("Paris", "Lyon", "Marseille", "Toulouse", "Bordeaux", "Brest", "Strasbourg"),
    Latitude = c(48.8566, 45.7640, 43.2965, 43.6047, 44.8378, 48.3904, 48.5734),
    Longitude = c(2.3522, 4.8357, 5.3698, 1.4442, -0.5792, -4.4861, 7.7521),
  )
  
  # Vérifier si la ville choisie est dans la table
  if (!(city %in% cities_coords$Place)) {
    stop("La ville choisie n'est pas dans la liste des villes supportées (Paris, Lyon, Marseille, Toulouse, Bordeaux, Brest et Strasbourg")
  }
  
  # Extraire coordonnées de la ville choisie
  coords <- cities_coords %>% filter(Place == city)
  lat <- coords$Latitude
  lon <- coords$Longitude
  
  # Calcul des variables temporelles
  if(!all(c("Year", "Month", "Week", "Day", "Time", "Hour", "Minute") %in% names(data))) {
    data <- data %>%
      dplyr::mutate(
        Year = year(Date_Time),
        Month = month(Date_Time),
        Week = isoweek(Date_Time),
        Day = day(Date_Time),
        Time = format(Date_Time, format = "%H:%M"),
        Hour = hour(Date_Time),
        Minute = minute(Date_Time)
      )
  }
  
  # Colonnes requises dans data
  required_columns <- c("Place", "Id", "Night_Date", "Date_Time")
  missing_columns <- required_columns[!required_columns %in% names(data)]
  if(length(missing_columns) > 0) {
    stop("Colonnes manquantes dans data: ", paste(missing_columns, collapse = ", "), ".")
  }
  
  # Préparer les dates et heures
  data <- data %>%
    dplyr::mutate(
      Night_Date = as.Date(Night_Date),
      Date_Time = as.POSIXct(Date_Time, tz = "UTC"),
      Hour = as.numeric(Hour),
      Minute = as.numeric(Minute)
    )
  
  if (!is.null(sun_offsets)) {
    # Mode éphémérides avec offsets (minutes)
    unique_nights <- sort(unique(data$Night_Date))
    times_list <- lapply(unique_nights, function(nd) {
      # Obtenir sunrise et sunset pour la date et la suivante
      sun_times_today <- getSunlightTimes(date = nd, lat = lat, lon = lon, keep = c("sunset"))
      sun_times_tomorrow <- getSunlightTimes(date = nd + 1, lat = lat, lon = lon, keep = c("sunrise"))
      
      start_time <- sun_times_today$sunset - minutes(abs(sun_offsets["before_sunset"]))
      end_time <- sun_times_tomorrow$sunrise + minutes(abs(sun_offsets["after_sunrise"]))
      duration_hours <- as.numeric(difftime(end_time, start_time, units = "hours"))
      
      tibble(Night_Date = nd, start_time = start_time, end_time = end_time, duration_hours = duration_hours)
    })
    times_df <- bind_rows(times_list)
    
    data <- data %>%
      dplyr::left_join(times_df, by = "Night_Date")
    
  } else {
    # Mode fixe (heures données)
    start_time_num <- as.numeric(strptime(record_time[1], format = "%H:%M"))
    end_time_num <- as.numeric(strptime(record_time[2], format = "%H:%M"))
    
    night_duration <- ifelse(end_time_num > start_time_num,
                             (end_time_num - start_time_num) / 3600,
                             (24 * 3600 - (start_time_num - end_time_num)) / 3600)
    
    data <- data %>%
      dplyr::mutate(
        start_time = as.POSIXct(paste(Night_Date, record_time[1]), tz = "UTC"),
        end_time = as.POSIXct(paste(Night_Date + 1, record_time[2]), tz = "UTC"),
        duration_hours = night_duration
      )
  }
  
  # Nombre de nuits (calcul automatique)
  nights <- length(unique(data$Night_Date))
  
  results <- data %>%
    group_by(Place, Id) %>%
    summarise(
      contacts = n(),
      night_positive = n_distinct(Night_Date),
      hour_positive = length(unique(paste(Night_Date, Hour))),
      minute_positive = length(unique(paste(Night_Date, Hour, Minute))),
      min_CPN = min(table(Night_Date)),
      mean_CPN = mean(table(Night_Date)),
      sd_CPN = sd(table(Night_Date)),
      max_CPN = max(table(Night_Date)),
      min_CPH = min(table(paste(Night_Date, Hour))),
      mean_CPH = mean(table(paste(Night_Date, Hour))) / mean(duration_hours),
      sd_CPH = sd(table(paste(Night_Date, Hour))) / mean(duration_hours),
      max_CPH = max(table(paste(Night_Date, Hour))),
      .groups = "drop"
    )
  
  return(results)
}
