# =======================================================================================
# Titre: Calcul des métriques de bridage optimales pour un projet éolien
# Description: Ce script R permet de réaliser une analyse multivariée des des données abiotiques
#              en fonction des contacts de chauves-souris afin de calculer les mesures de bridage
#              les plus adaptées.
# Auteur: Alexandre LANGLAIS
# Date: 2024/04/01
# Version: 1.1
# GitHub : https://github.com/a-langlais/bat_activity
# Dépendances: ggplot2, lubridate
# =======================================================================================

calculate_seuils <- function(contacts, meteo, dates = NULL, variables, percent = 95, plot = FALSE) {
  library(lubridate)
  library(ggplot2)
  
  # Gestion des erreurs
  if(!is.null(dates) && length(dates) != 2) {
    stop("L'argument 'dates' doit être un vecteur de longueur 2.")
  }
  
  if(!is.numeric(pourcentage) || pourcentage < 0 || pourcentage > 100) {
    stop("L'argument 'pourcentage' doit être un nombre entre 0 et 100.")
  }
  
  if(!all(variables %in% names(meteo))) {
    stop("Toutes les 'variables' doivent être présentes dans 'meteo'.")
  }
  
  if(any(sapply(c(contacts$Date_Time, meteo$Date_Time), function(dt) is.na(dmy_hm(dt)[1])))) {
    stop("Une ou plusieurs dates ne sont pas dans le format attendu ou ne peuvent pas être converties.")
  }
  
  if(nrow(donneesFusionnees) == 0) {
    stop("La fusion des données a produit un ensemble de données vide. Vérifiez les plages de dates et les formats.")
  }
  
  if(any(is.na(coefficients))) {
    warning("Des coefficients du modèle sont NA, ce qui peut indiquer un problème avec les données ou le modèle.")
  }
  
  if(plot) {
    missing_vars <- !variables %in% names(donneesFusionnees)
    if(any(missing_vars)) {
      warning("Les variables suivantes manquent dans les données fusionnées et seront ignorées dans le graphique :", 
              paste(variables[missing_vars], collapse = ", "))
      plot <- FALSE # Désactiver le plot si les données nécessaires manquent
    }
  }
  
  # Convertir les colonnes de date en POSIXct
  contacts$Date_Time <- dmy_hm(contacts$Date_Time)
  meteo$Date_Time <- dmy_hm(meteo$Date_Time)
  contacts$Date_Time <- ceiling_date(contacts$Date_Time, unit = "10 minutes")
  
  # Filtrer les données si des dates sont fournies
  if (!is.null(dates) && length(dates) == 2) {
    date_start <- dmy(dates[1])
    date_end <- dmy(dates[2])
    contacts <- contacts[contacts$Date_Time >= date_start & contacts$Date_Time <= date_end, ]
    meteo <- meteo[meteo$Date_Time >= date_start & meteo$Date_Time <= date_end, ]
  }
  
  # Garder la période nocturne
  meteo <- meteo[hour(meteo$Date_Time) >= 20 | hour(meteo$Date_Time) < 6, ]
  
  # Fusionner les données sur la base de la date et de l'heure
  data_merged <- merge(contacts, meteo, by = "Date_Time", all = TRUE)
  
  # Créer la colonne 'is_contacted' pour indiquer la présence ou l'absence de contact
  data_merged$is_contacted <- ifelse(!is.na(data_merged$Id), 1, 0)
  
  # Calcul des percentiles pour les variables spécifiées
  percentiles_list <- sapply(variables, function(var) {
    # Calcul des quantiles pour le percent spécifié et son inverse
    probs <- c(percent / 100, 1 - (percent / 100))
    quantiles <- quantile(data_merged[[var]], probs = probs, na.rm = TRUE)
    
    # Retourne une liste contenant les deux quantiles pour chaque variable
    list(
      Percentile = quantiles[1],
      InversePercentile = quantiles[2]
    )}, simplify = FALSE)
  
  # Transformer la liste en dataframe pour une manipulation facile
  percentiles_df <- do.call(rbind, lapply(names(percentiles_list), function(var) {
    data.frame(
      Variable = var,
      Percentile = percentiles_list[[var]]$Percentile,
      InversePercentile = percentiles_list[[var]]$InversePercentile
    )
  }))
  
  gc()
  # Construction du modèle de régression logistique
  model <- as.formula(paste("is_contacted ~", paste(variables, collapse = " + ")))
  modele <- glm(model, data = data_merged, family = binomial(link = "logit"))
  gc()
  
  # Extraction des métriques du modèle
  coefficients <- summary(modele)$coefficients
  resultats <- data.frame(
    Variable = rownames(coefficients),
    Coefficient = coefficients[,1],
    ErreurStandard = coefficients[,2],
    zValue = coefficients[,3],
    PValue = coefficients[,4]
  )
  
  # Ajout des percentiles
  resultats <- merge(resultats, percentiles_df, by = "Variable", all.x = TRUE)
  
  # Générer les graphiques si demandé
  if (plot) {
    for (var in variables) {
      p <- ggplot(data_merged, aes_string(x = var, y = "is_contacted")) +
        geom_point(alpha = 0.5) +
        geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "red") +
        labs(title = paste("Contact des chauves-souris vs", var),
             x = var,
             y = "Probabilité de contact") +
        theme_minimal()
      print(p)
    }
  }
  
  return(resultats)
}
