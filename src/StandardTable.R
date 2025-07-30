# =======================================================================================
# Titre: Analyse de l'Activité Chiroptérologique
# Description: Ce script R permet de retourner un tableau d'analyse au format standard 
#              compatible avec les autres fonctions BatActivity à partir d'une sortie 
#              SonoChiro ou Tadarida (en anglais).
#
# Auteur: Alexandre LANGLAIS
# Date: 2025/07/30
# Version: 1.2
# GitHub : https://github.com/a-langlais/bat_activity
# Dépendances: aucune
#
# Instructions: Ce script définit une fonction TableFormatage qui retourne un tableau 
#               standard.
# =======================================================================================

TableFormatage <- function(table, sftw = "Tadarida"){
  #  
  if(sftw == "Tadarida"){
    File <- table$nom.du.fichier
    Place <- substr(File, 25, nchar(File)-20)
    Id <- ifelse(table$observateur_taxon == "", table$tadarida_taxon, table$observateur_taxon)
    
    DateTime <- substr(File, nchar(File)-18, nchar(File)-1)
    Year <- substr(DateTime, 1, 4)
    Month <- substr(DateTime, 5, 6)
    Day <- substr(DateTime, 7, 8)
    Hour <- substr(DateTime, 10, 11)
    Minute <- substr(DateTime, 12, 13)
  }
  #
  if(sftw == "SonoChiro"){
    File <- table$File
    Place <- substr(File, 1, nchar(File)-20)
    Id <- table$Id
    
    DateTime <- substr(File, nchar(File)-18, nchar(File)-1)
    Year <- substr(DateTime, 1, 4)
    Month <- substr(DateTime, 5, 6)
    Day <- substr(DateTime, 7, 8)
    Hour <- substr(DateTime, 10, 11)
    Minute <- substr(DateTime, 12, 13)
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
