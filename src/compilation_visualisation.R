# =======================================================================================
# Titre: Compilation d'exemples de visualisation à partir des tableaux standards
# Description: Graphiques reproductibles à partir du format standard.
#
# Utilisation:
#   1. Exécuter les sections 1 a 4 pour préparer les données et les fonctions.
#   2. Sélectionner le bloc du graphique souhaite, export inclus.
#   3. Exécuter ce bloc: la figure est enregistrée dans output.
#
# Auteur : Alexandre LANGLAIS
# Date : 01/06/2026
# =======================================================================================

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

# ---------------------------------------------------------------------------------------
# 1. Parametres
# ---------------------------------------------------------------------------------------

input_file <- "data/multisite_standard.csv"
output_dir <- "output"

# Les heures sont affichées sur une nuit continue: 20:00, 22:00, ..., 06:00.
night_start <- 17
night_end <- 31
profile_bin_width <- 0.5
hourly_profile_bin_width <- 1
distribution_bin_width <- 0.25
species_distribution_bin_width <- 5 / 60

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------------------
# 2. Style graphique modifiable
# ---------------------------------------------------------------------------------------

bat_ink <- "#24332E"
bat_green <- "#315B4F"
bat_mist <- "#E7ECE8"
bat_gold <- "#D2A44E"
heat_colors <- c("#FFFFFF", "#FEE08B", "#F46D43", "#B2182B")

theme_bat <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", color = bat_ink, size = rel(1.15)),
      plot.subtitle = element_text(color = "#61706B", margin = margin(b = 8)),
      axis.title = element_text(color = bat_ink),
      axis.text = element_text(color = "#52605C"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = bat_mist, linewidth = 0.35),
      strip.text = element_text(face = "bold", color = bat_ink),
      strip.background = element_rect(fill = "#F3F6F4", color = NA),
      legend.title = element_text(face = "bold"),
      plot.caption = element_text(color = "#75817D", hjust = 0)
    )
}

to_night_hour <- function(hour, minute = 0) {
  decimal_hour <- hour + minute / 60
  if_else(decimal_hour < 12, decimal_hour + 24, decimal_hour)
}

format_night_hour <- function(x) {
  sprintf("%02d:00", as.integer(x) %% 24)
}

scale_x_night <- function(step = 2) {
  breaks <- seq(night_start, night_end, by = step)
  scale_x_continuous(
    breaks = breaks,
    labels = format_night_hour(breaks),
    expand = expansion(mult = c(0.01, 0.01))
  )
}

coord_x_night <- function() {
  coord_cartesian(xlim = c(night_start, night_end), expand = FALSE)
}

species_palette <- function(species) {
  colors <- grDevices::hcl.colors(length(species), palette = "Dark 3")
  setNames(colors, species)
}

save_plot <- function(plot, filename, width, height) {
  output_file <- file.path(output_dir, paste0(filename, ".png"))
  ggsave(
    filename = output_file,
    plot = plot,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
  message("Figure exportee: ", output_file)
  invisible(plot)
}

# ---------------------------------------------------------------------------------------
# 3. Import et préparation des données
# ---------------------------------------------------------------------------------------

df <- read_delim(input_file, delim = ";", show_col_types = FALSE) %>%
  transmute(
    Place = as.factor(Place),
    Id = as.factor(Id),
    Night_Date = as.Date(Night_Date),
    Hour = as.integer(Hour),
    Minute = as.integer(Minute),
    night_hour = to_night_hour(Hour, Minute)
  ) %>%
  filter(
    !is.na(Id),
    !is.na(Place),
    !is.na(Night_Date),
    between(night_hour, night_start, night_end)
  )

if (nrow(df) == 0) {
  stop("Aucun contact exploitable dans la plage horaire nocturne configurée.")
}

species <- levels(droplevels(df$Id))
species_colors <- species_palette(species)
nights <- sort(unique(df$Night_Date))

# ---------------------------------------------------------------------------------------
# 4. Tables d'analyse
# ---------------------------------------------------------------------------------------

summarise_distribution <- function(data, by_species = FALSE) {
  distribution <- data %>%
    mutate(
      night_bin = floor(night_hour / distribution_bin_width) * distribution_bin_width,
      bin_center = night_bin + distribution_bin_width / 2
    )

  if (by_species) {
    distribution %>%
      count(Night_Date, Id, bin_center, name = "activity") %>%
      complete(
        Night_Date = nights,
        Id = species,
        bin_center = seq(
          night_start + distribution_bin_width / 2,
          night_end - distribution_bin_width / 2,
          by = distribution_bin_width
        ),
        fill = list(activity = 0)
      ) %>%
      group_by(Id, bin_center) %>%
      summarise(
        mean_activity = mean(activity),
        sd_activity = sd(activity),
        .groups = "drop"
      )
  } else {
    distribution %>%
      count(Night_Date, bin_center, name = "activity") %>%
      complete(
        Night_Date = nights,
        bin_center = seq(
          night_start + distribution_bin_width / 2,
          night_end - distribution_bin_width / 2,
          by = distribution_bin_width
        ),
        fill = list(activity = 0)
      ) %>%
      group_by(bin_center) %>%
      summarise(
        mean_activity = mean(activity),
        sd_activity = sd(activity),
        .groups = "drop"
      )
  }
}

summarise_species_profiles <- function(data, bin_width) {
  data %>%
    mutate(night_bin = floor(night_hour / bin_width) * bin_width) %>%
    count(Night_Date, Id, night_bin, name = "activity") %>%
    complete(
      Night_Date = nights,
      Id = species,
      night_bin = seq(night_start, night_end - bin_width, by = bin_width),
      fill = list(activity = 0)
    ) %>%
    group_by(Id, night_bin) %>%
    summarise(
      mean_activity = mean(activity),
      sd_activity = sd(activity),
      .groups = "drop"
    )
}

make_community_matrix <- function(data) {
  expected_sites <- sort(unique(as.character(data$Place)))

  community_df <- data %>%
    count(Place, Id, name = "activity") %>%
    pivot_wider(names_from = Id, values_from = activity, values_fill = 0) %>%
    as.data.frame()

  if (nrow(community_df) != length(expected_sites)) {
    stop("La matrice des communautés doit contenir exactement une ligne par site.")
  }

  community_df <- community_df[match(expected_sites, as.character(community_df$Place)), , drop = FALSE]
  rownames(community_df) <- as.character(community_df$Place)

  community_matrix <- as.matrix(
    community_df[, setdiff(names(community_df), "Place"), drop = FALSE]
  )

  if (!identical(rownames(community_matrix), expected_sites)) {
    stop("Les lignes de la matrice des communautés ne correspondent pas aux sites attendus.")
  }

  community_matrix
}

bray_curtis_dist <- function(community_matrix) {
  site_count <- nrow(community_matrix)
  site_names <- rownames(community_matrix)
  distances <- matrix(
    0,
    nrow = site_count,
    ncol = site_count,
    dimnames = list(site_names, site_names)
  )

  for (i in seq_len(site_count)) {
    for (j in seq_len(site_count)) {
      denominator <- sum(community_matrix[i, ] + community_matrix[j, ])
      distances[i, j] <- if (denominator == 0) {
        0
      } else {
        sum(abs(community_matrix[i, ] - community_matrix[j, ])) / denominator
      }
    }
  }

  stats::as.dist(distances)
}

cluster_communities <- function(community_matrix) {
  if (nrow(community_matrix) < 2) {
    return(NULL)
  }

  stats::hclust(bray_curtis_dist(community_matrix), method = "average")
}

# ---------------------------------------------------------------------------------------
# 5. Figures de synthese
# ---------------------------------------------------------------------------------------

export_activite_par_espece <- function() {
  species_activity <- df %>%
    count(Night_Date, Id, name = "activity") %>%
    complete(Night_Date = nights, Id = species, fill = list(activity = 0)) %>%
    group_by(Id) %>%
    summarise(
      mean_activity = mean(activity),
      sd_activity = sd(activity),
      .groups = "drop"
    ) %>%
    arrange(mean_activity)

  plot <- ggplot(
    species_activity,
    aes(x = mean_activity, y = reorder(Id, mean_activity))
  ) +
    geom_errorbar(
      aes(
        xmin = pmax(mean_activity - sd_activity, 0),
        xmax = mean_activity + sd_activity
      ),
      width = 0.22,
      color = bat_gold
    ) +
    geom_col(fill = bat_green, width = 0.72) +
    geom_text(
      aes(label = scales::number(mean_activity, accuracy = 0.1)),
      hjust = -0.15,
      color = bat_ink,
      size = 3
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(
      title = "Activité acoustique moyenne par espèce",
      x = "Contacts par nuit (moyen)",
      y = NULL
    ) +
    theme_bat()

  save_plot(plot, "01_activite_par_espece", width = 8, height = 6.5)
}

# Export individuel:
export_activite_par_espece()

export_composition_par_site <- function() {
  site_species <- df %>%
    count(Place, Id, name = "activity") %>%
    group_by(Place) %>%
    mutate(proportion = activity / sum(activity)) %>%
    ungroup()

  plot <- ggplot(site_species, aes(x = Place, y = proportion, fill = Id)) +
    geom_col(width = 0.68, color = "white", linewidth = 0.2) +
    scale_fill_manual(values = species_colors) +
    scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.02))) +
    labs(
      title = "Composition spécifique par site",
      x = NULL,
      y = "Part des contacts",
      fill = "Espèce"
    ) +
    theme_bat() +
    theme(legend.position = "right")

  save_plot(plot, "02_composition_par_site", width = 8, height = 5)
}

# Export individuel:
export_composition_par_site()

export_activite_au_cours_de_la_nuit <- function() {
  night_distribution <- summarise_distribution(df)

  plot <- ggplot(night_distribution, aes(x = bin_center, y = mean_activity)) +
    geom_col(
      width = distribution_bin_width,
      fill = bat_green,
      color = "white",
      linewidth = 0.15
    ) +
    geom_errorbar(
      aes(
        ymin = pmax(mean_activity - sd_activity, 0),
        ymax = mean_activity + sd_activity
      ),
      width = distribution_bin_width * 0.55,
      color = bat_gold,
      linewidth = 0.45
    ) +
    scale_x_night() +
    coord_x_night() +
    labs(
      title = "Activité moyenne au cours de la nuit",
      y = "Contacts par nuit (15 minutes)"
    ) +
    theme_bat()

  save_plot(plot, "03_activite_au_cours_de_la_nuit", width = 8, height = 4.8)
}

# Export individuel:
export_activite_au_cours_de_la_nuit()

export_chronologie_nocturne <- function() {
  hour_date <- df %>%
    mutate(night_bin = floor(night_hour / profile_bin_width) * profile_bin_width) %>%
    count(Night_Date, night_bin, name = "activity") %>%
    complete(
      Night_Date = nights,
      night_bin = seq(night_start, night_end - profile_bin_width, by = profile_bin_width),
      fill = list(activity = 0)
    ) %>%
    mutate(Night_Date = factor(Night_Date, levels = rev(nights)))

  plot <- ggplot(hour_date, aes(x = night_bin, y = Night_Date, fill = activity)) +
    geom_tile(width = profile_bin_width, height = 1, color = "white", linewidth = 0.12) +
    scale_x_night() +
    scale_fill_gradientn(colors = heat_colors, trans = "sqrt") +
    coord_fixed(ratio = profile_bin_width, xlim = c(night_start, night_end), expand = FALSE) +
    labs(
      title = "Chronologie de l'activité nocturne (30 minutes)",
      y = "Nuit",
      fill = "Contacts"
    ) +
    theme_bat() +
    theme(
      panel.grid = element_blank(),
      axis.text.y = element_text(size = 6)
    )

  save_plot(plot, "04_chronologie_nocturne", width = 8, height = 15)
}

# Export individuel:
export_chronologie_nocturne()

export_activite_par_nuit <- function() {
  night_activity <- df %>%
    count(Night_Date, name = "activity")

  plot <- ggplot(night_activity, aes(x = Night_Date, y = activity)) +
    geom_col(fill = bat_green, width = 0.82) +
    geom_text(aes(label = activity), vjust = -0.35, color = bat_ink, size = 2.7) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(
      title = "Intensité acoustique par nuit",
      y = "Nombre de contacts"
    ) +
    theme_bat()

  save_plot(plot, "05_activite_par_nuit", width = 8, height = 4.8)
}

# Export individuel:
export_activite_par_nuit()

export_richesse_par_nuit <- function() {
  night_richness <- df %>%
    group_by(Night_Date) %>%
    summarise(richness = n_distinct(Id), .groups = "drop")

  plot <- ggplot(night_richness, aes(x = Night_Date, y = richness)) +
    geom_line(color = bat_gold, linewidth = 0.65) +
    geom_point(color = bat_gold, size = 1.3) +
    scale_y_continuous(breaks = scales::pretty_breaks()) +
    labs(
      title = "Richesse spécifique par nuit",
      y = "Nombre d'especes"
    ) +
    theme_bat()

  save_plot(plot, "06_richesse_par_nuit", width = 8, height = 4.8)
}

# Export individuel:
export_richesse_par_nuit()

# ---------------------------------------------------------------------------------------
# 6. Figures detaillées par espèce
# ---------------------------------------------------------------------------------------

plot_species_profiles <- function(bin_width, title, subtitle) {
  hour_species <- summarise_species_profiles(df, bin_width)

  ggplot(hour_species, aes(x = night_bin, y = mean_activity, color = Id, fill = Id)) +
    geom_ribbon(
      aes(
        ymin = pmax(mean_activity - sd_activity, 0),
        ymax = mean_activity + sd_activity
      ),
      alpha = 0.16,
      color = NA
    ) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 0.9) +
    facet_wrap(vars(Id), scales = "free_y", ncol = 4) +
    scale_x_night(step = 4) +
    coord_x_night() +
    scale_color_manual(values = species_colors, guide = "none") +
    scale_fill_manual(values = species_colors, guide = "none") +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Heure de la nuit",
      y = "Nombre de contacts (moyen)"
    ) +
    theme_bat(base_size = 9)
}

export_profils_par_espece <- function() {
  plot <- plot_species_profiles(
    bin_width = profile_bin_width,
    title = "Profils d'activité nocturne par espèce",
  )

  save_plot(plot, "07_profils_par_espece", width = 11, height = 12)
}

# Export individuel:
export_profils_par_espece()

export_distributions_par_espece <- function() {
  species_distribution <- df %>%
    mutate(
      night_bin = floor(night_hour / species_distribution_bin_width) * species_distribution_bin_width,
      bin_center = round(night_bin + species_distribution_bin_width / 2, 6)
    ) %>%
    count(Id, bin_center, name = "activity") %>%
    complete(
      Id = species,
      bin_center = seq(
        night_start + species_distribution_bin_width / 2,
        night_end - species_distribution_bin_width / 2,
        by = species_distribution_bin_width
      ) %>% round(6),
      fill = list(activity = 0)
    )

  plot <- ggplot(
    species_distribution,
    aes(x = bin_center, y = activity, fill = Id)
  ) +
    geom_col(width = species_distribution_bin_width, color = "white", linewidth = 0.05) +
    facet_wrap(vars(Id), scales = "free_y", ncol = 4) +
    scale_x_night(step = 4) +
    coord_x_night() +
    scale_fill_manual(values = species_colors, guide = "none") +
    labs(
      title = "Distribution horaire des contacts par espèce",
      y = "Nombre de contacts (5 minutes)"
    ) +
    theme_bat(base_size = 9)

  save_plot(plot, "08_distributions_par_espece", width = 11, height = 12)
}

# Export individuel:
export_distributions_par_espece()

export_profils_horaires_par_espece <- function() {
  plot <- plot_species_profiles(
    bin_width = hourly_profile_bin_width,
    title = "Profils horaires d'activité par espèce",
  )

  save_plot(plot, "09_profils_horaires_par_espece", width = 11, height = 12)
}

# Export individuel:
export_profils_horaires_par_espece()

# ---------------------------------------------------------------------------------------
# 7. Communautés par site
# ---------------------------------------------------------------------------------------

export_heatmap_communautes <- function() {
  community_matrix <- make_community_matrix(df)
  community_cluster <- cluster_communities(community_matrix)
  site_order <- if (is.null(community_cluster)) {
    rownames(community_matrix)
  } else {
    community_cluster$labels[community_cluster$order]
  }

  heatmap_df <- as.data.frame(as.table(community_matrix)) %>%
    rename(Place = Var1, Id = Var2, activity = Freq) %>%
    mutate(Place = factor(Place, levels = rev(site_order)))

  plot <- ggplot(heatmap_df, aes(x = Id, y = Place, fill = activity)) +
    geom_tile(color = "white", linewidth = 0.35) +
    scale_fill_gradientn(colors = heat_colors, trans = "sqrt") +
    coord_fixed() +
    labs(
      title = "Communautés acoustiques par site",
      fill = "Contacts"
    ) +
    theme_bat() +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  save_plot(plot, "10_heatmap_communautes", width = 10, height = max(3.8, nrow(community_matrix) * 0.45))
}

# Export individuel:
export_heatmap_communautes()

export_dendrogramme_communautes <- function() {
  community_matrix <- make_community_matrix(df)
  community_cluster <- cluster_communities(community_matrix)
  output_file <- file.path(output_dir, "11_dendrogramme_communautes.png")

  grDevices::png(output_file, width = 1900, height = 1250, res = 180, bg = "white")
  old_par <- par(no.readonly = TRUE)
  on.exit({
    par(old_par)
    grDevices::dev.off()
  })

  par(
    mar = c(5, 3, 4, 10) + 0.1,
    col.axis = bat_ink,
    col.lab = bat_ink,
    col.main = bat_ink,
    family = "sans"
  )

  if (is.null(community_cluster)) {
    plot.new()
    title(main = "Dendrogramme des communautés acoustiques")
    text(
      0.5,
      0.58,
      "Au moins deux sites sont necessaires pour calculer un dendrogramme.",
      col = bat_ink,
      cex = 1.1
    )
    text(0.5, 0.46, "Le fichier charge contient actuellement un seul site.", col = "#61706B")
  } else {
    plot(
      as.dendrogram(community_cluster),
      horiz = TRUE,
      main = "Dendrogramme des communautés acoustiques",
      xlab = "Distance de Bray-Curtis",
      ylab = "",
      cex = 1.15
    )
    grid(nx = NULL, ny = NA, col = bat_mist, lty = "dotted")
  }

  message("Figure exportee: ", output_file)
  invisible(community_cluster)
}

# Export individuel:
export_dendrogramme_communautes()
