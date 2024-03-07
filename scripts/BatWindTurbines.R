# =======================================================================================
# Titre: Bridage éolien multivarié pour les chauves-souris
# Description: Ce script R permet d'afficher un graphique de l'activité chiroptérologique 
#              en fonction des valeurs abiotiques (température, vitesse du vent) et 
#              détermine par KDE les seuils les plus performants pour réaliser un bridage 
#              éolien couvrant 95% minimum de l'activité
#
# Auteur: Alexandre LANGLAIS
# Date: 2020/11/02
# Version: 1
# GitHub : https://github.com/a-langlais/bat_activity
# D�pendances: dplyr, ggplot2, ks, lubridate
# =======================================================================================

library(ggplot2)
library(ks)
library(lubridate)
library(dplyr)

results$Date <- dmy(results$Date)
results$Night_Date <- dmy(results$Night_Date)
results$Time <- format(results$Time, format = "%H:%M")
meteo$Date.Time <- dmy_hm(meteo$Date.Time)

# Creation colonne Date_Time
results$Date_Time <- paste(results$Date, results$Time)
results$Date_Time <- format(results$Date_Time, format = "%Y-%m-%d %H:%M")
results$Date_Time <- as.POSIXct(results$Date_Time)
results$Date.Time <- ceiling_date(results$Date_Time, unit = "10 minutes")

table <- results[which(results$Height == "0"),] # tous les enregistrements en hauteur
table <- results[which(results$Height == "0" & 
                         results$Night_Date >= "2018-06-15" & results$Night_Date <= "2018-09-15"),]  # changer les dates selon besoin
                        # enregistrements en hauteur lors de la période estivale 

table <- table[, c("Id","Date.Time")]

table <- merge(table, meteo[,c(1, 6, 7)], by = "Date.Time")
colnames(table) <- c("Date.Time", "Id", "Speed","Temp")
table <- na.omit(table)
summary(table)

kd <- kde(table[, c(3,4)], compute.cont = TRUE, adj.positive = TRUE)


# =======================
# =======================

contour <- with(kd, contourLines(x = eval.points[[1]], y = eval.points[[2]], z = estimate, levels = cont["5%"])[[1]])
contour <- data.frame(contour)

IC <- "5%"
Max_windspeed <- max(contour$x) # x maximum
Min_temp <- ifelse(min(contour$y) < "0", 0, min(contour$y)) # y minimum
avoid_contacts <- nrow(table[table$Speed <= Max_windspeed & table$Temp >= Min_temp,])
avoid_percent <- (avoid_contacts/nrow(table))*100
speed_restriction <- data.frame(IC, Max_windspeed, Min_temp, avoid_contacts, avoid_percent)

for (i in 2:99){
  IC <- paste(i, "%", sep = "")
  contour <- with(kd, contourLines(x = eval.points[[1]], y = eval.points[[2]], z = estimate, levels = cont[IC])[[1]])
  contour <- data.frame(contour)
  #
  Max_windspeed <- max(contour$x)
  Min_temp <- ifelse(min(contour$y) < "0", 0, min(contour$y))
  avoid_contacts <- nrow(table[table$Speed <= max(contour$x) & table$Temp >= min(contour$y),])
  avoid_percent <- avoid_contacts/nrow(table)*100
  row.temp <- data.frame(IC, Max_windspeed, Min_temp, avoid_contacts, avoid_percent) 
  speed_restriction <- bind_rows(speed_restriction, row.temp)
}
rm(Min_temp, Max_windspeed, IC, avoid_contacts, avoid_percent, i, row.temp, contour, kd)
write.table(speed_restriction, "Bridage-2d.csv", row.names = FALSE, col.names = TRUE, dec = ",", sep = ";")

# =======================
# =======================

# Intervalles minimum pour eviter 95% des contacts les plus probables
#
kd <- kde(table[, c(3,4)], compute.cont = TRUE, adj.positive = TRUE)
contour <- with(kd, contourLines(x = eval.points[[1]], y = eval.points[[2]], z = estimate, levels = cont["5%"])[[1]])
contour <- data.frame(contour)

Max_windspeed <- max(contour$x) # x maximum
Min_temp <- ifelse(min(contour$y) < "0", 0, min(contour$y)) # y minimum
nrow(table[table$Speed <= max(contour$x) & table$Temp >= min(contour$y),]) # nombre de contacts evites
nrow(table[table$Speed <= max(contour$x) & table$Temp >= min(contour$y),])/nrow(table)*100 # pourcentage total evite

# representation graphique des intervalles
#
IntGraph <- ggplot(data = table, aes(Speed, Temp))
IntGraph <- IntGraph + geom_point(alpha = 0.2, colour = "black")
IntGraph <- IntGraph + geom_point(data = , alpha = 0.2, colour = "red")
# IntGraph <- IntGraph + geom_path(aes(x, y), data = contour, colour = "blue")
# IntGraph <- IntGraph + stat_density2d(aes(fill = ..density..), geom = "tile", contour = FALSE, n = 200)
#
IntGraph <- IntGraph + geom_hline(yintercept = Min_temp, data = contour, linetype = "dashed", colour = "red")
IntGraph <- IntGraph + annotate("text", x = min(table$Speed), y = min(contour$y), label = round(Min_temp, digits = 4), colour = "red", vjust = -0.5)
IntGraph <- IntGraph + geom_vline(xintercept = Max_windspeed, data = contour, linetype = "dashed",  colour = "red")
IntGraph <- IntGraph + annotate("text", x = max(contour$x), y = max(table$Temp), label = round(Max_windspeed, digits = 4), colour = "red", hjust = -0.1, vjust = 1)
#
IntGraph <- IntGraph + annotate("rect", xmin = 0, xmax = Max_windspeed, ymin = Min_temp, ymax = Inf, alpha = 0.2, fill = "blue")

IntGraph <- IntGraph + theme_bw()
# IntGraph <- IntGraph + theme(legend.position = "none", panel.background = element_blank())
IntGraph

