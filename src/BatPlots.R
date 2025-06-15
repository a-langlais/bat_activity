# =======================================================================================
# Titre: Visualisation graphique des résultats
# Description:  Ces fonctions permettent d'afficher des résultats sous forme graphique
#               intéractifs directement dans l'application.
#
# Auteur: Alexandre LANGLAIS
# Date: 2025/06/15
# Version: 1.0
# GitHub : https://github.com/a-langlais/bat_activity
# Dépendances: plotly, dplyr, lubridate
#
# Instructions: Ce script définit plusieurs fonctions pour afficher la synthèse des 
#               résultats dans l'application :
#                 - Un piechart pour la répartition de l'activité
#                 - un barplot pour la réparition des contacts par espèce
#                 - un scatterplot pour l'activité phénologique
# =======================================================================================

# Import des packages
library(plotly)     # 4.10.4
library(dplyr)      # 1.1.4
library(lubridate)  # 1.9.4

# Piechart des proportions d'activité
plot_behavior_pie <- function(indicateurs_df) {
  all_row <- indicateurs_df[indicateurs_df$Point == "All", ]
  
  data_pie <- data.frame(
    Behavior = c("Transit", "Social", "Chasse"),
    Percentage = c(all_row$c_sonar, all_row$c_social, all_row$c_feeding)
  )
  
  plot_ly(
    data_pie,
    labels = ~Behavior,
    values = ~Percentage,
    type = 'pie',
    hole = 0.4,
    textinfo = 'label+percent',
    insidetextorientation = 'radial',
    marker = list(colors = RColorBrewer::brewer.pal(3, "Set2"),
                  line = list(color = '#FFFFFF', width = 2))
  ) %>%
    layout(
      title = list(text = "Répartition des comportements (All)", x = 0.5),
      # legend = list(title = list(text = '<b>Comportement</b>'))
      showlegend = FALSE
    )
}

# Barplot des contacts par espèce
plot_species_bar <- function(data_df) {
  count_by_species <- data_df %>%
    group_by(Id) %>%
    summarise(contacts = n(), .groups = "drop") %>%
    arrange(contacts)
  
  plot_ly(
    count_by_species,
    x = ~contacts,
    y = ~reorder(Id, contacts),
    type = 'bar',
    orientation = 'h',
    marker = list(
      color = ~contacts,
      colorscale = 'Plasma',
      line = list(color = 'rgba(50, 50, 50, 0.8)', width = 1)
    ),
    text = ~contacts,
    textposition = 'outside',
    hoverinfo = 'text',
    hovertext = ~paste("Espèce:", Id, "<br>Contacts:", contacts)
  ) %>%
    layout(
      title = list(text = "Nombre de contacts par espèce", x = 0.5),
      xaxis = list(title = "Nombre de contacts", zeroline = FALSE),
      yaxis = list(title = "Espèce", automargin = TRUE),
      margin = list(l = 120)
    )
}

# Scatterplot
plot_passive_activity <- function(data, col_id) {
  # Extraire date et heure numérique continue
  data <- data %>%
    mutate(
      Date = as.Date(Date_Time),
      Heure_num = hour(Date_Time) + minute(Date_Time)/60 + second(Date_Time)/3600,
      # Décaler les heures avant 18h pour avoir un continuum 18h-18h+24h
      Heure_num = ifelse(Heure_num < 18, Heure_num + 24, Heure_num)
    )
  
  # Définir ticks et labels pour l'axe y (heure)
  y_ticks <- seq(18, 42, by = 2)
  y_labels <- sapply(y_ticks, function(h) {
    heure <- floor(h) %% 24
    sprintf("%02d:00", heure)
  })
  
  # Définir ticks x : dates uniques triées
  x_ticks <- sort(unique(data$Date))
  
  # Labels x : format "janvier 2018" en français
  # Pour forcer la locale en français temporairement
  old_locale <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "fr_FR.UTF-8")  # ou "French_France" sur Windows
  
  x_labels <- format(x_ticks, "%B %Y")
  
  # Remettre locale par défaut
  Sys.setlocale("LC_TIME", old_locale)
  
  # Créer la palette de couleur
  n_colors <- length(unique(data[[col_id]]))
  palette_colors <- grDevices::rainbow(n_colors)
  
  # Créer le scatter plot avec axes inversés
  p <- plot_ly(
    data = data,
    x = ~Date,
    y = ~Heure_num,
    type = "scatter",
    mode = "markers",
    color = ~get(col_id),
    colors = palette_colors,
    marker = list(size = 8),
    text = ~paste0(col_id, ": ", get(col_id),
                   "<br>Date: ", Date,
                   "<br>Heure: ", format(Date_Time, "%H:%M")),
    hoverinfo = "text",
    height = 600
  ) %>%
    layout(
      title = "Activité enregistrée en fonction du temps",
      xaxis = list(
        title = "Date",
        type = "category"
      ),
      yaxis = list(
        title = "Heure",
        tickvals = y_ticks,
        ticktext = y_labels,
        range = c(17, 31),
        zeroline = FALSE
      ),
      legend = list(title = list(text = paste0("<b>", col_id, "</b>")))
    )
  
  return(p)
}
