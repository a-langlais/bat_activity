# =======================================================================================
# Titre: Visualisation graphique des resultats
# Description: Fonctions plotly utilisees dans l'application Shiny BatActivity.
#
# Auteur: Alexandre LANGLAIS
# Date: 2026/05/28
# Version: 1.1
# GitHub : https://github.com/a-langlais/bat_activity
# =======================================================================================

library(plotly)     # 4.10.4
library(dplyr)      # 1.1.4
library(lubridate)  # 1.9.4
library(suncalc)    # 0.5.1
library(tibble)     # 3.2.1

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
    type = "pie",
    hole = 0.4,
    textinfo = "label+percent",
    insidetextorientation = "radial",
    marker = list(
      colors = RColorBrewer::brewer.pal(3, "Set2"),
      line = list(color = "#FFFFFF", width = 2)
    )
  ) %>%
    layout(
      #title = list(text = "Répartition des comportements", x = 0.5),
      showlegend = FALSE
    )
}

plot_species_bar <- function(data_df) {
  count_by_species <- data_df %>%
    group_by(Id) %>%
    summarise(contacts = n(), .groups = "drop") %>%
    arrange(contacts)

  plot_ly(
    count_by_species,
    x = ~contacts,
    y = ~reorder(Id, contacts),
    type = "bar",
    orientation = "h",
    marker = list(
      color = ~contacts,
      colorscale = "Plasma",
      line = list(color = "rgba(50, 50, 50, 0.8)", width = 1)
    ),
    text = ~contacts,
    textposition = "outside",
    hoverinfo = "text",
    hovertext = ~paste("Espece:", Id, "<br>Contacts:", contacts)
  ) %>%
    layout(
      #title = list(text = "Nombre de contacts par espèce", x = 0.5),
      xaxis = list(title = "Nombre de contacts", zeroline = FALSE),
      yaxis = list(title = "Espece", automargin = TRUE),
      margin = list(l = 120)
    )
}

plot_passive_activity <- function(data, col_id, city = "Paris") {
  cities_coords <- tibble(
    Place = c("Paris", "Lyon", "Marseille", "Toulouse", "Bordeaux", "Brest", "Strasbourg"),
    Latitude = c(48.8566, 45.7640, 43.2965, 43.6047, 44.8378, 48.3904, 48.5734),
    Longitude = c(2.3522, 4.8357, 5.3698, 1.4442, -0.5792, -4.4861, 7.7521)
  )

  if (!(city %in% cities_coords$Place)) {
    city <- "Paris"
  }

  coords <- filter(cities_coords, Place == city)

  to_night_hour <- function(datetime) {
    hour_value <- hour(datetime) + minute(datetime) / 60 + second(datetime) / 3600
    ifelse(hour_value < 18, hour_value + 24, hour_value)
  }

  data <- data %>%
    mutate(
      Date_Time = as.POSIXct(Date_Time, tz = "UTC"),
      Date = as.Date(Date_Time),
      Heure_num = to_night_hour(Date_Time)
    ) %>%
    arrange(Date)

  y_ticks <- seq(18, 42, by = 2)
  y_labels <- sapply(y_ticks, function(h) {
    heure <- floor(h) %% 24
    sprintf("%02d:00", heure)
  })

  x_ticks <- sort(unique(data$Date))
  sun_times <- getSunlightTimes(
    date = x_ticks,
    lat = coords$Latitude,
    lon = coords$Longitude,
    keep = c("sunrise", "sunset"),
    tz = "UTC"
  )

  sun_df <- sun_times %>%
    mutate(
      Date = as.Date(date),
      Sunrise_num = to_night_hour(sunrise),
      Sunset_num = to_night_hour(sunset)
    )

  n_colors <- length(unique(data[[col_id]]))
  palette_colors <- grDevices::rainbow(n_colors)

  plot_ly(height = 800) %>%
    add_markers(
      data = data,
      x = ~Date,
      y = ~Heure_num,
      color = ~get(col_id),
      colors = palette_colors,
      marker = list(size = 8),
      text = ~paste0(
        col_id, ": ", get(col_id),
        "<br>Date: ", format(Date, "%Y-%m-%d"),
        "<br>Heure: ", format(Date_Time, "%H:%M")
      ),
      hoverinfo = "text"
    ) %>%
    add_lines(
      data = sun_df,
      x = ~Date,
      y = ~Sunrise_num,
      name = "Lever du soleil",
      line = list(color = "#d9a441", width = 2),
      text = ~paste0(
        "Lever du soleil<br>Date: ", format(Date, "%Y-%m-%d"),
        "<br>Heure: ", format(sunrise, "%H:%M")
      ),
      hoverinfo = "text",
      inherit = FALSE
    ) %>%
    add_lines(
      data = sun_df,
      x = ~Date,
      y = ~Sunset_num,
      name = "Coucher du soleil",
      line = list(color = "#315b4f", width = 2),
      text = ~paste0(
        "Coucher du soleil<br>Date: ", format(Date, "%Y-%m-%d"),
        "<br>Heure: ", format(sunset, "%H:%M")
      ),
      hoverinfo = "text",
      inherit = FALSE
    ) %>%
    layout(
      title = "Phénologie des contacts",
      xaxis = list(title = "Date", type = "date"),
      yaxis = list(
        title = "Heure",
        tickvals = y_ticks,
        ticktext = y_labels,
        range = c(17, 31),
        zeroline = FALSE
      ),
      legend = list(title = list(text = paste0("<b>", col_id, "</b>")))
    )
}
