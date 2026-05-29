# =======================================================================================
# Titre: Phénogramme sur données de point passif
# Description:  Fonction ggplot2 pour visualiser un phénogramme de toutes les espèces ou 
#               d'une seule espèce d'un suivi longue durée. Sous forme d'un nuage de points
#               ou d'une carte de chaleur basée sur la densité des contacts.
#               Basé sur les tableaux de contacts standardisés (BatActivity).
# Date : 2026-05-29
# Auteur : Alexandre LANGLAIS
# =======================================================================================

# Packages nécessaires
library(readr)      # 2.1.5
library(dplyr)      # 1.1.4
library(lubridate)  # 1.9.4
library(ggplot2)    # 3.5.2
library(suncalc)    # 0.5.1
library(tibble)     # 3.2.1

# Définition de la fonction Phenogram(file_path, species, plot_type, city, output_file)
# Prends en arguments : 
#     - le chemin du csv (fichier standardisé), 
#     - l'espèce souhaitée (ou "All" pour toutes), 
#     - la nature du graphique (nuage de points ou carte de chaleur), 
#     - la ville la plus proche parmi Paris, Lyon, Marseille, Toulouse, Bordeaux, Brest et Strasbourg,
#     - le chemin du dossier de sortie pour le ou les graphiques

Phenogram <- function(file_path, species = "all", plot_type = c("points", "heatmap"), city = "Paris", output_file = NULL){

  # Paramètres de base
  plot_type <- match.arg(plot_type)
  local_timezone <- "Europe/Paris"
  night_start_hour <- 16
  plot_y_min <- 17
  plot_y_max <- 33

  # Coordonnées des principales villes
  cities_coords <- tibble::tibble(
    Place = c("Paris", "Lyon", "Marseille", "Toulouse", "Bordeaux", "Brest", "Strasbourg"),
    Latitude = c(48.8566, 45.7640, 43.2965, 43.6047, 44.8378, 48.3904, 48.5734),
    Longitude = c(2.3522, 4.8357, 5.3698, 1.4442, -0.5792, -4.4861, 7.7521)
  )
  
  if (!(city %in% cities_coords$Place)) {
    stop("La ville choisie n'est pas disponible : ", city, call. = FALSE)
  }

  # Gestion du Night_Date pour éviter l'overlap
  to_night_hour <- function(datetime) {
    hour_value <- lubridate::hour(datetime) +
      lubridate::minute(datetime) / 60 +
      lubridate::second(datetime) / 3600
    ifelse(hour_value < night_start_hour, hour_value + 24, hour_value)
  }

  parse_local_datetime <- function(x) {
    x <- trimws(as.character(x))
    x <- sub("\\s+(UTC|GMT|CEST|CET)$", "", x, ignore.case = TRUE)
    lubridate::parse_date_time(
      x,
      orders = c("ymd HMS", "ymd HM", "ymd_HMS", "ymd_HM", "dmy HMS", "dmy HM"),
      tz = local_timezone
    )
  }

  # Formatage des mois en lettres et vérification des colonnes
  format_day_month <- function(x) {
    months_fr <- c(
      "Janvier", "Fevrier", "Mars", "Avril", "Mai", "Juin",
      "Juillet", "Aout", "Septembre", "Octobre", "Novembre", "Decembre"
    )
    dates <- as.Date(x, origin = "1970-01-01")
    paste(as.integer(format(dates, "%d")), months_fr[as.integer(format(dates, "%m"))])
  }

  # Lecture du CSV et vérifications
  data <- readr::read_delim(file_path, delim = ";", col_types = readr::cols(.default = readr::col_character()),
                            locale = readr::locale(encoding = "UTF-8"), show_col_types = FALSE)

  required_columns <- c("Place", "Id", "Night_Date", "Date_Time")
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0) {
    stop("Colonne(s) manquante(s) : ", paste(missing_columns, collapse = ", "), call. = FALSE)
  }

  # Prétraitement des données et vérifications
  data_plot <- data |>
    dplyr::mutate(
      Date_Time = parse_local_datetime(.data$Date_Time),
      Night_Date = as.Date(.data$Night_Date),
      Date = as.Date(.data$Date_Time),
      Date_num = as.numeric(.data$Date),
      Heure_num = to_night_hour(.data$Date_Time)
    ) |>
    dplyr::filter(!is.na(.data$Date_Time), !is.na(.data$Id), !is.na(.data$Date))

  if (!identical(species, "all")) {
    if (!(species %in% data_plot$Id)) {
      stop("Espece absente du fichier : ", species, call. = FALSE)
    }
    data_plot <- dplyr::filter(data_plot, .data$Id == species)
  }

  if (nrow(data_plot) == 0) {
    stop("Aucune donnee valide a afficher.", call. = FALSE)
  }

  coords <- dplyr::filter(cities_coords, .data$Place == city)
  y_ticks <- seq(18, 30, by = 2)
  y_labels <- sprintf("%02dh", y_ticks %% 24)
  x_breaks <- pretty(data_plot$Date_num, n = 7)

  # Calcul des éphémérides
  sun_df <- suncalc::getSunlightTimes(
    date = sort(unique(data_plot$Date)),
    lat = coords$Latitude,
    lon = coords$Longitude,
    keep = c("sunrise", "sunset"),
    tz = local_timezone
  ) |>
    dplyr::mutate(
      Date = as.Date(.data$date),
      Date_num = as.numeric(.data$Date),
      Sunrise_num = to_night_hour(.data$sunrise),
      Sunset_num = to_night_hour(.data$sunset)
    )

  # Création des visualisations
  phenogram <- ggplot2::ggplot(data_plot, ggplot2::aes(x = .data$Date_num, y = .data$Heure_num))

  # plot_type = "heatmap"
  # Nuage de points avec superposition d'une carte de chaleur basée sur la densité des points
  if (plot_type == "heatmap") {
    can_compute_density <- nrow(data_plot) >= 3 &&
      dplyr::n_distinct(data_plot$Date_num) >= 2 &&
      dplyr::n_distinct(data_plot$Heure_num) >= 2

    if (can_compute_density) {
      phenogram <- phenogram +
        ggplot2::geom_point(color = "#f4bf00", size = 1.5, alpha = 0.85) +
        ggplot2::stat_density_2d(
          ggplot2::aes(fill = ggplot2::after_stat(level)),
          geom = "polygon",
          contour = TRUE,
          bins = 9,
          alpha = 0.95
        ) +
        ggplot2::scale_fill_gradientn(
          colours = c("yellow", "orange", "orangered", "red"),
          values = c(0, 0.25, 0.5, 0.75, 1),
          name = "level",
          breaks = function(x) range(x, finite = TRUE),
          labels = c("Minimum", "Maximum"),
          guide = ggplot2::guide_colorbar(
            title.position = "top",
            barheight = ggplot2::unit(70, "pt")
          )
        )
    } else {
      phenogram <- phenogram +
        ggplot2::geom_point(color = "#f4bf00", size = 1.8, alpha = 0.85) +
        ggplot2::labs(subtitle = "Densité non calculée : nombre de points insuffisant")
    }
  }

  # plot_type = "points"
  # Nuage de points seul, avec une couleur par espèce
  if (plot_type == "points" && identical(species, "all")) {
    phenogram <- phenogram +
      ggplot2::geom_point(ggplot2::aes(color = .data$Id), size = 1.8, alpha = 0.75) +
      ggplot2::guides(color = ggplot2::guide_legend(title = "Id", override.aes = list(size = 3, alpha = 1)))
  }

  if (plot_type == "points" && !identical(species, "all")) {
    phenogram <- phenogram +
      ggplot2::geom_point(color = "#f4bf00", size = 1.8, alpha = 0.75)
  }

  # Ajout des courbes du Lever et du Coucher du soleil
  phenogram <- phenogram +
    ggplot2::geom_line(data = sun_df,
      ggplot2::aes(x = .data$Date_num, y = .data$Sunrise_num), inherit.aes = FALSE, color = "blue", linewidth = 0.8) +
    ggplot2::geom_line(data = sun_df,
      ggplot2::aes(x = .data$Date_num, y = .data$Sunset_num), inherit.aes = FALSE, color = "blue", linewidth = 0.8) +
    ggplot2::annotate(
      "text",
      x = max(sun_df$Date_num, na.rm = TRUE),
      y = tail(sun_df$Sunrise_num, 1),
      label = "Lever du soleil",
      hjust = 1,
      vjust = -0.6,
      color = "blue",
      fontface = "bold",
      size = 3.2
    ) +
    ggplot2::annotate(
      "text",
      x = max(sun_df$Date_num, na.rm = TRUE),
      y = tail(sun_df$Sunset_num, 1),
      label = "Coucher du soleil",
      hjust = 1,
      vjust = 1.4,
      color = "blue",
      fontface = "bold",
      size = 3.2
    ) +
    
    # Paramètres esthétiques du graphique
    ggplot2::scale_x_continuous(name = "Jour de l'annee", breaks = x_breaks, labels = format_day_month) +
    ggplot2::scale_y_continuous(name = "Heure de la nuit", breaks = y_ticks, labels = y_labels, 
                                expand = ggplot2::expansion(mult = c(0.01, 0.03))) +
    ggplot2::labs(title = NULL) +
    ggplot2::coord_cartesian(ylim = c(plot_y_min, plot_y_max)) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "#e7e7e7", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_line(color = "#f2f2f2", linewidth = 0.2),
      legend.position = "right",
      axis.title = ggplot2::element_text(face = "bold")
    )

  # Enregistrement du graphique
  if (!is.null(output_file)) {
    dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
    ggplot2::ggsave(output_file, phenogram, width = 12, height = 8, dpi = 300)
  }

  phenogram
}

# ===============================================================
# ===============================================================

# Un graphique avec toutes les espèces :
Phenogram("data/passive_standard.csv", species = "all", plot_type = "points", city = "Bordeaux")
Phenogram("data/passive_standard.csv", species = "all", plot_type = "heatmap", city = "Bordeaux")

# Un graphique par espèce :
data <- read.csv2("data/passive_standard.csv", stringsAsFactors = FALSE)
species_list <- sort(unique(data$Id))
for (sp in species_list) {
  # Petit nettoyage pour éviter les slashs dans les noms de fichiers
  file_name <- gsub("[^A-Za-z0-9_-]", "_", sp)
  
  Phenogram(
    file_path = "data/passive_standard.csv",
    species = sp,
    plot_type = "heatmap",
    city = "Bordeaux",
    output_file = paste0("output/phenogram_", file_name, ".png")
  )
}
