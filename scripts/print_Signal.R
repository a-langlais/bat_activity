# =======================================================================================
# Titre: Génération d'un graphique des courbes de réponses du test étendu des TeensyRecorders
# Description: Ce script R est conçu pour générer automatiquement un graphique du signal
#               suite à un test étendu sur les détecteurs TeensyRecorders.
#
# Auteur: Alexandre LANGLAIS
# Date: 2024/03/29
# Version: 1
# GitHub : https://github.com/a-langlais/bat_activity
# Dépendances: ggplot2
#
# Instructions: Ce script définit une fonction print_Signal qui ne prend aucun argument.
#               Une fenêtre de sélection permet à l'utilisateur de choisir son fichier *.csv'
# =======================================================================================

print_Signal <- function(){
  
  choose <- file.choose()
  
  # Gestion des erreurs
  tryCatch({
    df <- read.csv(choose, header = FALSE)
  }, error = function(e) {
    print("Erreur lors de la lecture du fichier. Assurez-vous que le fichier est accessible et au bon format.")
    return()
  })
  
  df <- read.csv(choose, header = FALSE)
  
  if (nrow(df) > 6){
    print("Votre tableau semble avoir plusieurs tests micros. Seul le dernier test est affiché.")
    df <- df[(nrow(df)-5):nrow(df), ]
  }
  
  # Récupération du titre et de la qualité du micro
  title <- paste(df[1,1], "du", df[1,2], "à", df[1,3])
  quality <- paste("Qualité: ", df[1,4])
  df <- df[-1,]
  
  # Preprocessing du tableau
  df <- t(df)
  rownames(df) <- NULL
  colnames(df) <- c("Canaux", "Signal", "Gabarit Min", "Gabarit Max", "Silence")
  df <- df[-1, ]
  
  df <- as.data.frame(df)
  df <- as.data.frame(lapply(df, as.numeric))
  
  # Création du graphique
  library(ggplot2)
  
  ggplot(df, aes(x = Canaux)) +
    geom_line(aes(y = Signal, color = "Signal")) +
    geom_line(aes(y = Gabarit.Min, color = "Gabarit Min")) +
    geom_line(aes(y = Gabarit.Max, color = "Gabarit Max")) +
    geom_line(aes(y = Silence, color = "Silence")) +  
    ggtitle(title, quality) +
    labs(x = "Canaux (kHz)", y = "Niveaux (dB)", color = "Courbes") +
    scale_color_manual(values = c("Signal" = "blue", "Gabarit Min" = "red", "Gabarit Max" = "green", "Silence" = "black")) +
    theme_classic()
  
  # Message de sortie
  print(paste("Graphique du", title, "produit avec succès."))
}
