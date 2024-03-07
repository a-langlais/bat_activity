# =======================================================================================
# Titre: Analyse de l'Activit� Chiropt�rologique
# Description: Ce script R permet de retourner un tableau d'analyse au format standard 
#              compatible avec les autres fonctions BatActivity � partir d'une sortie 
#              SonoChiro ou Tadarida (en anglais).
#
# Auteur: Alexandre LANGLAIS
# Date: 2022/10/03
# Version: 1.1
# GitHub : https://github.com/a-langlais/bat_activity
# D�pendances: aucune
#
# Instructions: Ce script d�finit une fonction TableFormatage qui retourne un tableau 
#               standard.
# =======================================================================================

TableFormatage <- function(table, sftw = "Tadarida"){
  #  
  if(sftw == "Tadarida"){
    File <- table$nom.du.fichier
    Place <- as.character(substr(table$File, 25, 29))
    Year <- as.character(substr(table$File, 31, 34))
    Month <- as.character(substr(table$File, 35, 36))
    Day <- as.character(substr(table$File, 37, 38))
    Hour <- as.character(substr(table$File, 40, 41))
    Minute <- as.character(substr(table$File, 42, 43))
    Id <- ifelse(table$observateur_taxon == "", table$tadarida_taxon, table$observateur_taxon)
  }
  #
  if(sftw == "SonoChiro"){
    File <- table$File
    Place <- as.character(substr(table$File, 1, 5))
    Year <- as.character(substr(table$File, 7, 10))
    Month <- as.character(substr(table$File, 11, 12))
    Day <- as.character(substr(table$File, 13, 14))
    Hour <- as.character(substr(table$File, 16, 17))
    Minute <- as.character(substr(table$File, 18, 19))
    Id <- table$Id
  }
  #
  Night_Date <- ""
  Night_Date <- as.POSIXct(Night_Date, format = "%Y-%m-%d")
  Week <- ""
  #
  Date <- as.POSIXct(format(paste(Year, "-", Month, "-", Day, sep = ""), format = "%Y-%m-%d"))
  Time <- paste(Hour, ":", Minute, sep = "")
  Date_Time <- paste(Date, Time)
  Date_Time <- format(Date_Time, format = "%Y-%m-%d %H:%M")
  table.temp <- data.frame(File, Place, Id, Night_Date, Date_Time, Date, Year, Month, Week, Day, Time, Hour, Minute)
  #
  table.temp$Night_Date <- ifelse(as.numeric(table.temp$Hour) < 10, 
                                  table.temp$Date - 86400, 
                                  table.temp$Date)
  table.temp$Night_Date <- as.POSIXct(table.temp$Night_Date, format = "%Y-%m-%d", origin = "1970-01-01")
  #
  table.temp$Week <- strftime(table.temp$Night_Date, format = "%V")
  #
  write.table(table.temp, file = "BatTable.csv", row.names = FALSE, col.names = TRUE, sep = ";", dec = ",")
  return(table.temp)
}