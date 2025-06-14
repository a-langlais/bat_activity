# =======================================================================================
# Titre: Visualisation graphique des résultats
# Description:  Ces fonctions permettent d'afficher des résultats sous forme graphique
#               intéractifs directement dans l'application.
#
# Auteur: Alexandre LANGLAIS
# Date: 2025/06/14
# Version: 1.0
# GitHub : https://github.com/a-langlais/bat_activity
# Dépendances: plotly, dplyr
#
# Instructions: Ce script définit plusieurs fonctions pour afficher la synthèse des 
#               résultats dans l'application :
#                 - Un piechart pour la répartition de l'activité
#                 - un barplot pour la réparition des contacts par espèce
# =======================================================================================

# Import des packages
library(plotly)
library(dplyr)

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