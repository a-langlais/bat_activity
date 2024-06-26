# =======================================================================================
# Titre: Analyse de l'Activit� Chiropt�rologique
# Description: Ce script R est con�u pour analyser l'activit� chiropt�rologique en
#              calculant des indicateurs sp�cifiques tels que le nombre d'esp�ces
#              d�tect�es, le nombre total de contacts, la fr�quence des contacts par heure,
#              et les pourcentages d'activit� sonar, sociale et de chasse.
#
# Auteur: Alexandre LANGLAIS
# Date: 2024/05/03
# Version: 1.1
# GitHub : https://github.com/a-langlais/bat_activity
# D�pendances: dplyr
#
# Instructions: Ce script d�finit une fonction BatActive qui prend en entr�e une
#               table de donn�es sur les observations chiropt�rologiques, une dur�e
#               d'observation (en minutes) et le nombre de points d'observation.
#               La fonction retourne un tableau r�capitulatif de l'activit�.
# =======================================================================================


BatActive <- function(table, duration, npoint) {
  
  library(dplyr)
  
  # V�rification des types d'entr�es
  if (!is.data.frame(table)) {
    stop("L'argument 'table' doit �tre un dataframe.")
  }
  
  if (!is.numeric(duration) || duration <= 0) {
    stop("L'argument 'duration' doit �tre un nombre positif.")
  }
  
  if (!is.numeric(npoint) || npoint < 1 || round(npoint) != npoint) {
    stop("L'argument 'npoint' doit �tre un entier positif.")
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
  
  # Ajout du r�sum� global si n�cessaire
  if (npoint > 1) {
    global_summary <- summarise(table,
                                Point = "All",
                                n_sp = n_distinct(Id),
                                contacts = n(),
                                CPHe = sum((contacts * 60) / duration) / npoint,
                                c_sonar = sum(Activity == "Sonar") / n() * 100,
                                c_social = sum(Activity == "Social") / n() * 100,
                                c_feeding = sum(Activity == "Feed") / n() * 100,
                                .groups = 'drop' # �vite de garder les groupes apr�s summarise()
    )
    active_table <- bind_rows(active_table, global_summary)
  }
  
  # Remplacement des NA par 0 pour les colonnes num�riques et par "" pour les caract�res
  active_table <- active_table %>%
    mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)),
           across(where(is.character), ~ifelse(is.na(.), "", .)))
  
  return(active_table)
}
#

