# =======================================================================================
# Titre: Vérification et Installation des Packages R avec Versions Spécifiques
# Description: Ce script R vérifie la présence de packages nécessaires au projet,
#              ainsi que leurs versions spécifiques. Si un package est absent ou si
#              la version installée diffère de celle requise, le script procède à son
#              installation ou à sa mise à jour. Il charge ensuite tous les packages.
#
# Auteur: Alexandre LANGLAIS
# Date: 2025/06/16
# Version: 1.0
# GitHub : https://github.com/a-langlais/bat_activity
# Dépendances: aucune
#
# Instructions: Exécuter ce script avant toute analyse ou déploiement pour s'assurer
#               que l'environnement contient les bonnes versions des bibliothèques.
#               Adapté pour des workflows reproductibles, notamment avec Shiny.
# =======================================================================================

# Liste des packages et versions requises
required_packages <- list(
  shiny     = "1.10.0",   # interface web
  readr     = "2.1.5",    # lecture rapide de fichiers
  dplyr     = "1.1.4",    # manipulation de données
  here      = "1.0.1",    # gestion des chemins relatifs
  plotly    = "4.10.4",   # graphiques interactifs
  lubridate = "1.9.4",    # gestion des dates
  suncalc   = "0.5.1",    # calcul des données solaires
  tibble    = "3.2.1"     # tableaux allégés
)

# Vérification et installation/mise à jour
for (pkg in names(required_packages)) {
  required_version <- required_packages[[pkg]]
  
  # Vérifie si le package est installé
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("🔧 Installation de ", pkg, " (version requise : ", required_version, ")")
    install.packages(pkg)
  } else {
    # Vérifie la version installée
    installed_version <- as.character(packageVersion(pkg))
    
    if (installed_version != required_version) {
      message("⚠️  ", pkg, " : version ", installed_version,
              " installée, version ", required_version, " requise → mise à jour")
      install.packages(pkg)
    } else {
      message("✅ ", pkg, " : version correcte (", installed_version, ")")
    }
  }
}

# Chargement explicite des packages après vérification
invisible(lapply(names(required_packages), library, character.only = TRUE))
