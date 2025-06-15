# =======================================================================================
# Titre: Analyse de l'Activité Chiroptérologique
# Description: Ce script R est conçu pour analyser l'activité chiroptérologique en
#              calculant des indicateurs spécifiques tels que le nombre d'espéces
#              détectées, le nombre total de contacts, la fréquence des contacts par heure,
#              et les pourcentages d'activité sonar, sociale et de chasse.
#
# Auteur: Alexandre LANGLAIS
# Date: 2025/06/14
# Version: 1.2
# GitHub : https://github.com/a-langlais/bat_activity
# Dépendances: dplyr
#
# Instructions: Ce script définit une fonction BatActive qui prend en entrée une
#               table de données sur les observations chiroptérologiques, une durée
#               d'observation (en minutes) et le nombre de points d'observation.
#               La fonction retourne un tableau récapitulatif de l'activité.
# =======================================================================================


BatActive <- function(table, duration, npoint) {
  
  library(dplyr)  # 1.1.4
  
  # Vérification des types d'entrées
  if (!is.data.frame(table)) {stop("L'argument 'table' doit Dtre un dataframe.")}
  if (!is.numeric(duration) || duration <= 0) {stop("L'argument 'duration' doit être un nombre positif.")}
  if (!is.numeric(npoint) || npoint < 1 || round(npoint) != npoint) {stop("L'argument 'npoint' doit être un entier positif.")}
  if (!"Place" %in% names(table) || !"Id" %in% names(table) || !"Activity" %in% names(table)) {stop("Le dataframe 'table' doit contenir les colonnes 'Place', 'Id', et 'Activity'.")}
  
  # Calculs par point
  active_table <- table %>%
    group_by(Place) %>%
    summarise(
      Point = unique(Place),
      n_sp = n_distinct(Id),
      contacts = n(),
      CPHe = (n() * 60) / duration,
      c_sonar = sum(Activity == "Transit", na.rm = TRUE) / n() * 100,
      c_social = sum(Activity == "Social", na.rm = TRUE) / n() * 100,
      c_feeding = sum(Activity == "Chasse", na.rm = TRUE) / n() * 100,
      .groups = "drop"
    ) %>%
    select(Point, n_sp, contacts, CPHe, c_sonar, c_social, c_feeding)
  
  # Résumé global si besoin
  if (npoint > 1) {
    total_contacts <- nrow(table)
    
    global_summary <- table %>%
      summarise(
        Point = "All",
        n_sp = n_distinct(Id),
        contacts = total_contacts,
        CPHe = (total_contacts * 60) / duration / npoint,
        c_sonar = sum(Activity == "Transit", na.rm = TRUE) / total_contacts * 100,
        c_social = sum(Activity == "Social", na.rm = TRUE) / total_contacts * 100,
        c_feeding = sum(Activity == "Chasse", na.rm = TRUE) / total_contacts * 100
      )
    
    active_table <- bind_rows(active_table, global_summary)
  }
  
  # Nettoyage NA
  active_table <- active_table %>%
    mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)),
           across(where(is.character), ~ifelse(is.na(.), "", .)))
  
  return(active_table)
}
#

