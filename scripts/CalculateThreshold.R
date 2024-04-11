# =======================================================================================
# Titre: Calcul des m�triques de bridage optimales pour un projet �olien
# Description: Ce script R permet de r�aliser une analyse multivari�e des des donn�es abiotiques
#              en fonction des data de chauves-souris afin de calculer les mesures de bridage
#              les plus adapt�es.
# Auteur: Alexandre LANGLAIS
# Date: 2024/04/01
# Version: 1.1
# GitHub : https://github.com/a-langlais/bat_activity
# D�pendances: ggplot2, lubridate
# =======================================================================================

CalculateThreshold <- function(data, meteo, dates = NULL, var, percent = 95, plot = FALSE) {
  library(lubridate)
  library(ggplot2)
  
  # Gestion des erreurs
  if(!is.null(dates) && length(dates) != 2) {
    stop("L'argument 'dates' doit �tre un vecteur de longueur 2.")
  }
  
  if(!is.numeric(percent) || percent < 0 || percent > 100) {
    stop("L'argument 'percent' doit �tre un nombre entre 0 et 100.")
  }
  
  if(!all(var %in% names(meteo))) {
    stop("Toutes les 'var' doivent �tre pr�sentes dans 'meteo'.")
  }
  
  if(any(sapply(c(data$Date_Time, meteo$Date_Time), function(dt) is.na(dmy_hm(dt)[1])))) {
    stop("Une ou plusieurs dates ne sont pas dans le format attendu ou ne peuvent pas �tre converties.")
  }
  
  if(nrow(data_merged) == 0) {
    stop("La fusion des donn�es a produit un ensemble de donn�es vide. V�rifiez les plages de dates et les formats.")
  }
  
  if(any(is.na(coefficients))) {
    warning("Des coefficients du mod�le sont NA, ce qui peut indiquer un probl�me avec les donn�es ou le mod�le.")
  }
  
  if(plot) {
    missing_vars <- !var %in% names(data_merged)
    if(any(missing_vars)) {
      warning("Les var suivantes manquent dans les donn�es fusionn�es et seront ignor�es dans le graphique :", 
              paste(var[missing_vars], collapse = ", "))
      plot <- FALSE # D�sactiver le plot si les donn�es n�cessaires manquent
    }
  }
  
  # Convertir les colonnes de date en POSIXct
  data$Date_Time <- dmy_hm(data$Date_Time)
  meteo$Date_Time <- dmy_hm(meteo$Date_Time)
  data$Date_Time <- ceiling_date(data$Date_Time, unit = "10 minutes")
  
  # Filtrer les donn�es si des dates sont fournies
  if (!is.null(dates) && length(dates) == 2) {
    date_start <- dmy(dates[1])
    date_end <- dmy(dates[2])
    data <- data[data$Date_Time >= date_start & data$Date_Time <= date_end, ]
    meteo <- meteo[meteo$Date_Time >= date_start & meteo$Date_Time <= date_end, ]
  }
  
  # Garder la p�riode nocturne
  meteo <- meteo[hour(meteo$Date_Time) >= 20 | hour(meteo$Date_Time) < 6, ]
  
  # Fusionner les donn�es sur la base de la date et de l'heure
  data_merged <- merge(data, meteo, by = "Date_Time", all = TRUE)
  
  # Cr�er la colonne 'is_contacted' pour indiquer la pr�sence ou l'absence de contact
  data_merged$is_contacted <- ifelse(!is.na(data_merged$Id), 1, 0)
  
  # Calcul des percentiles pour les var sp�cifi�es
  percentiles_list <- sapply(var, function(var) {
    # Calcul des quantiles pour le percent sp�cifi� et son inverse
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
  # Construction du mod�le de r�gression logistique
  model <- as.formula(paste("is_contacted ~", paste(var, collapse = " + ")))
  modele <- glm(model, data = data_merged, family = binomial(link = "logit"))
  gc()
  
  # Extraction des m�triques du mod�le
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
  
  # G�n�rer les graphiques si demand�
  if (plot) {
    for (var in var) {
      p <- ggplot(data_merged, aes_string(x = var, y = "is_contacted")) +
        geom_point(alpha = 0.5) +
        geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "red") +
        labs(title = paste("Contact des chauves-souris vs", var),
             x = var,
             y = "Probabilit� de contact") +
        theme_minimal()
      print(p)
    }
  }
  
  return(resultats)
}