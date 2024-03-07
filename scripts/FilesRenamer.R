# =======================================================================================
# Titre: Programme de renommage de masse des fichiers sons
# Description: Ce script R est conçu pour renommer les fichiers sons d'un répértoire en fonction
#              de leurs métadonnées (date et heure de création).
#
# Auteur: Alexandre LANGLAIS
# Date: 2019/03/25
# Version: 1
# GitHub : https://github.com/a-langlais/bat_activity
# Dépendances: aucune
#
# Instructions: Il suffit de saison le chemin d'accès du répértoire en question avec 
#               `setwd(...)` puis appliquer la fonction.
#
#               ex : setwd() # répértoire du script
#                    files <- list.files(pattern = ".wav", ignore.case = TRUE)
#                    list.renamer(files)
# =======================================================================================

#
list.renamer <- function(list){
  newnames <- file.info(list)$mtime
  newnames <- as.character(newnames)
  newnames <- gsub(':', '', newnames)
  newnames <- gsub('-', '', newnames)
  newnames <- gsub(' ', '_', newnames)
  file.rename(list, paste0(newnames, ".wav"))
}



