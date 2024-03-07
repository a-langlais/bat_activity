# =======================================================================================
# Titre: Analyse de l'Activité Chiroptérologique
# Description: Ce script R est conçu pour analyser l'activité chiroptérologique en
#              calculant des indicateurs spécifiques tels que le nombre d'espèces
#              détectées, le nombre total de contacts, la fréquence des contacts par heure,
#              et les pourcentages d'activité sonar, sociale et de chasse.
#
# Auteur: Alexandre LANGLAIS
# Date: 2024/05/03
# Version: 1.1
# GitHub : https://github.com/a-langlais/bat_activity
# Dépendances: dplyr
#
# Instructions: Ce script définit une fonction BatActive qui prend en entrée une
#               table de données sur les observations chiroptérologiques, une durée
#               d'observation (en minutes) et le nombre de points d'observation.
#               La fonction retourne un tableau récapitulatif de l'activité.
# =======================================================================================

# ======== PACKAGES ==============
library(dplyr)

BatActive <- function(table, duration, npoint) {
  
  # Vérification des types d'entrées
  if (!is.data.frame(table)) {
    stop("L'argument 'table' doit être un dataframe.")
  }
  
  if (!is.numeric(duration) || duration <= 0) {
    stop("L'argument 'duration' doit être un nombre positif.")
  }
  
  if (!is.numeric(npoint) || npoint < 1 || round(npoint) != npoint) {
    stop("L'argument 'npoint' doit être un entier positif.")
  }
  
  if (!"Place" %in% names(table) || !"Id" %in% names(table) || !"Activity" %in% names(table)) {
    stop("Le dataframe 'table' doit contenir les colonnes 'Place', 'Id', et 'Activity'.")
  }
  
  # Calcul par point avec dplyr
  active_table <- table %>%
    group_by(Place) %>%
    summarise(
      Point = as.character(unique(Place)),
      Point = unique(Place),
      n_sp = n_distinct(Id),
      contacts = n(),
      CPHe = (contacts * 60) / duration,
      c_sonar = sum(Activity == "Sonar") / n() * 100,
      c_social = sum(Activity == "Social") / n() * 100,
      c_feeding = sum(Activity == "Feed") / n() * 100
    ) %>%
    ungroup()
  
  # Ajout du résumé global si nécessaire
  if (npoint > 1) {
    global_summary <- summarise(table,
                                Point = "All",
                                n_sp = n_distinct(Id),
                                contacts = n(),
                                CPHe = sum((contacts * 60) / duration) / npoint,
                                c_sonar = sum(Activity == "Sonar") / n() * 100,
                                c_social = sum(Activity == "Social") / n() * 100,
                                c_feeding = sum(Activity == "Feed") / n() * 100,
                                .groups = 'drop' # Évite de garder les groupes après summarise()
    )
    active_table <- bind_rows(active_table, global_summary)
  }
  
  # Remplacement des NA par 0 pour les colonnes numériques et par "" pour les caractères
  active_table <- active_table %>%
    mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)),
           across(where(is.character), ~ifelse(is.na(.), "", .)))
  
  return(active_table)
}
#

