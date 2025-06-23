# =======================================================================================
# Titre: Fonction BatPassive - Analyse des données d'activité chiroptérologique passive
# Description:  Fonction R destinée à analyser les données d'enregistrement passif d'activité
#               chiroptérologique. Elle traite un jeu de données contenant les enregistrements horaires,
#               calcule des indicateurs d'activité nocturne (contacts, occurrences par heure, etc.) 
#               en prenant en compte la localisation géographique et la période d'enregistrement.
#
# Auteur: Alexandre LANGLAIS
# Date: 2025/06/23
# Version: 1.1
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
    Longitude = c(2.3522, 4.8357, 5.3698, 1.4442, -0.5792, -4.4861, 7.7521)
  )
  
  # Vérifier si la ville choisie est dans la table
  if (!(city %in% cities_coords$Place)) {
    stop("La ville choisie n'est pas dans la liste des villes supportées (Paris, Lyon, Marseille, Toulouse, Bordeaux, Brest et Strasbourg")
  }
  
  # Extraire coordonnées de la ville choisie
  coords <- filter(cities_coords, Place == city)
  lat <- coords$Latitude
  lon <- coords$Longitude
  
  # Colonnes temporelles
  data <- data %>%
    mutate(
      Night_Date = as.Date(Night_Date),
      Date_Time = as.POSIXct(Date_Time, tz = "UTC"),
      Hour = hour(Date_Time),
      Minute = minute(Date_Time)
    )
  
  # Liste complète des nuits
  all_nights <- seq(min(data$Night_Date), max(data$Night_Date), by = "day")
  
  # Calcul des durées de nuit
  if (!is.null(sun_offsets)) {
    times_df <- lapply(all_nights, function(nd) {
      s1 <- getSunlightTimes(date = nd, lat = lat, lon = lon, keep = "sunset")
      s2 <- getSunlightTimes(date = nd + 1, lat = lat, lon = lon, keep = "sunrise")
      start <- s1$sunset - minutes(abs(sun_offsets["before_sunset"]))
      end <- s2$sunrise + minutes(abs(sun_offsets["after_sunrise"]))
      tibble(Night_Date = nd, start_time = start, end_time = end,
             duration_hours = as.numeric(difftime(end, start, units = "hours")))
    }) %>% bind_rows()
  } else {
    # Mode fixe
    start_time_num <- as.numeric(strptime(record_time[1], format = "%H:%M"))
    end_time_num <- as.numeric(strptime(record_time[2], format = "%H:%M"))
    fixed_duration <- ifelse(end_time_num > start_time_num,
                             (end_time_num - start_time_num) / 3600,
                             (24 * 3600 - (start_time_num - end_time_num)) / 3600)
    
    times_df <- tibble(
      Night_Date = all_nights,
      start_time = as.POSIXct(paste(all_nights, record_time[1]), tz = "UTC"),
      end_time = as.POSIXct(paste(all_nights + 1, record_time[2]), tz = "UTC"),
      duration_hours = fixed_duration
    )
  }
  
  # Intégrer la durée de nuit à chaque observation
  data <- data %>%
    left_join(times_df, by = "Night_Date") %>%
    filter(Date_Time >= start_time & Date_Time <= end_time)
  
  # Heures et minutes positives
  hour_minute_summary <- data %>%
    mutate(
      hour_id = paste(Night_Date, Hour),
      minute_id = paste(Night_Date, Hour, Minute)
    ) %>%
    group_by(Place, Id) %>%
    summarise(
      hour_positive = n_distinct(hour_id),
      minute_positive = n_distinct(minute_id),
      .groups = "drop"
    )
  
  # Contacts par nuit
  contacts_per_night <- data %>%
    group_by(Place, Id, Night_Date) %>%
    summarise(n_contacts = n(), .groups = "drop")
  
  # Grille complète Place x Id x Night_Date
  full_grid <- expand.grid(
    Place = unique(data$Place),
    Id = unique(data$Id),
    Night_Date = all_nights
  )
  
  # Ajouter les zéros et durées
  full_data <- full_grid %>%
    left_join(contacts_per_night, by = c("Place", "Id", "Night_Date")) %>%
    left_join(times_df, by = "Night_Date") %>%
    mutate(
      n_contacts = replace_na(n_contacts, 0),
      contacts_per_hour = n_contacts / duration_hours
    )
  
  # Résumé final
  results <- full_data %>%
    group_by(Place, Id) %>%
    summarise(
      contacts = sum(n_contacts),
      night_positive = sum(n_contacts > 0),
      min_CPN = min(n_contacts),
      mean_CPN = mean(n_contacts),
      sd_CPN = sd(n_contacts),
      max_CPN = max(n_contacts),
      min_CPH = min(contacts_per_hour),
      mean_CPH = mean(contacts_per_hour),
      sd_CPH = sd(contacts_per_hour),
      max_CPH = max(contacts_per_hour),
      .groups = "drop"
    ) %>%
    left_join(hour_minute_summary, by = c("Place", "Id")) %>%
    mutate(
      hour_positive = replace_na(hour_positive, 0),
      minute_positive = replace_na(minute_positive, 0)
    ) %>%
    select(
      Place, Id,
      contacts,
      night_positive,
      hour_positive,
      minute_positive,
      min_CPN,
      mean_CPN,
      sd_CPN,
      max_CPN,
      min_CPH,
      mean_CPH,
      sd_CPH,
      max_CPH
    )
  
  
  return(results)
}
