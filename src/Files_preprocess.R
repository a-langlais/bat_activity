# =============================================================================
# Prétraitement audio - Équivalent Kaleidoscope Lite
# =============================================================================
# Ce script reproduit le prétraitement de Kaleidoscope Lite :
#   - Récupération de tous les fichiers .wav d'un répertoire
#   - Découpage en segments → toujours ≤50s EN SORTIE (après expansion)
#   - Split des channels stéréo en deux fichiers mono (optionnel)
#   - Application de l'expansion de temps si nécessaire
#
# Deux modes selon l'état des fichiers d'entrée :
#
#   already_expanded = TRUE  (fichiers déjà en TE×10, e.g. Pettersson D500X)
#     → Segments ≤50s (domaine brut = domaine de sortie, sample rate inchangé)
#
#   already_expanded = FALSE (fichiers full-spectrum / direct, e.g. SM4BAT FS)
#     → Segments ≤5s brutes (= 50s après TE×10)
#     → Correction du sample rate ÷10 appliquée après découpage
#
# Ordre des opérations :
#   1. Lecture
#   2. Découpage en segments
#   3. Split L/R si stéréo et demandé
#   4. Correction du sample rate si already_expanded = FALSE
#   5. Écriture
#
# Dépendances : tuneR
# =============================================================================

library(tuneR)

# =============================================================================
# PARAMÈTRES UTILISATEUR — Modifier selon vos besoins
# =============================================================================

# Répertoire contenant les fichiers .wav à traiter
input_dir <- "C:/Users/langl/Documents/ABC PNRMP/Bourg/Data"

# Répertoire de sortie (sera créé s'il n'existe pas)
output_dir <- "C:/Users/langl/Documents/ABC PNRMP/PRETRAITEMENT/Car170114-2025-Pass2-Z2_ANGLIERS_Bourg"

# Les fichiers d'entrée sont-ils DÉJÀ en expansion de temps (TE×10) ?
# TRUE → fichiers déjà expansés (e.g. Pettersson D500X, Batbox, etc.). Découpage ≤50s, sample rate NON modifié
# FALSE → fichiers full-spectrum / direct (e.g. Wildlife Acoustics SM4BAT FS, AudioMoth en direct recording, etc.). Découpage ≤5s brutes, puis sample rate ÷10 appliqué
already_expanded <- FALSE

# Faut-il séparer les channels stéréo en deux fichiers mono ?
# TRUE  → chaque segment stéréo donne deux fichiers : _L.wav et _R.wav
# FALSE → les fichiers stéréo sont conservés tels quels
split_channels <- TRUE

# Traitement récursif (inclure les sous-dossiers) ?
recursive <- FALSE

# =============================================================================
# FONCTIONS
# =============================================================================

#' Durée maximale d'un segment en secondes (domaine brut, avant toute correction)
#'
#'   - Déjà expansé  : 50s brutes → 50s en sortie  (sample rate inchangé)
#'   - Pas expansé   :  5s brutes → 50s en sortie  (sample rate ÷10 → ×10 durée)
#'
#' @param already_exp Booléen : fichiers déjà en TE×10 ?
#' @return Durée maximale en secondes (domaine brut)
max_segment_duration <- function(already_exp) {
  if (already_exp) 50 else 5
}


#' Découpe un objet Wave en segments de durée maximale fixée
#'
#' @param wave      Objet Wave (tuneR)
#' @param max_dur_s Durée maximale d'un segment en secondes (domaine brut)
#' @return Liste d'objets Wave
segment_wave <- function(wave, max_dur_s) {
  sr         <- wave@samp.rate
  total_samp <- length(wave@left)
  max_samp   <- floor(max_dur_s * sr)
  
  if (total_samp <= max_samp) {
    return(list(wave))
  }
  
  segments <- list()
  start    <- 1L
  
  while (start <= total_samp) {
    end <- min(start + max_samp - 1L, total_samp)
    
    if (wave@stereo) {
      seg <- Wave(
        left      = wave@left[start:end],
        right     = wave@right[start:end],
        samp.rate = sr,
        bit       = wave@bit
      )
    } else {
      seg <- Wave(
        left      = wave@left[start:end],
        samp.rate = sr,
        bit       = wave@bit
      )
    }
    
    segments <- c(segments, list(seg))
    start    <- end + 1L
  }
  
  return(segments)
}


#' Applique l'expansion de temps (correction sample rate ÷10)
#'
#' À n'appliquer que si already_expanded = FALSE.
#' Divise le sample rate par 10 pour restituer les vraies fréquences ultrasoniques.
#'
#' @param wave Objet Wave (tuneR)
#' @return Objet Wave avec sample rate corrigé
apply_time_expansion <- function(wave) {
  new_rate <- wave@samp.rate / 10L
  if (new_rate < 1000) {
    warning(sprintf(
      "Sample rate après expansion trop bas : %d Hz. Vérifiez vos fichiers source.",
      as.integer(new_rate)
    ))
  }
  wave@samp.rate <- as.integer(new_rate)
  return(wave)
}


#' Formate un index de segment en suffixe zéro-paddé (_001, _002, …)
#'
#' @param i     Index du segment
#' @param total Nombre total de segments
#' @return Chaîne de caractères formatée
segment_suffix <- function(i, total) {
  width <- max(3L, nchar(as.character(total)))
  sprintf(paste0("_%0", width, "d"), i - 1L)
}


#' Écrit un segment avec les options demandées
#'
#' @param seg         Objet Wave (segment brut)
#' @param base_path   Chemin de base sans extension ni suffixe de channel
#' @param split_ch    Séparer les channels stéréo ?
#' @param already_exp Fichiers déjà expansés ? (détermine si on corrige le SR)
#' @return Vecteur des chemins de fichiers créés
write_segment <- function(seg, base_path, split_ch, already_exp) {
  paths <- c()
  
  if (seg@stereo && split_ch) {
    # Stéréo → deux fichiers mono
    for (ch in c("left", "right")) {
      label <- ifelse(ch == "left", "L", "R")
      mono  <- tuneR::channel(seg, ch)
      if (!already_exp) mono <- apply_time_expansion(mono)
      out_p <- paste0(base_path, "_", label, ".wav")
      tuneR::writeWave(mono, out_p)
      paths <- c(paths, out_p)
    }
  } else {
    # Mono ou stéréo conservée
    if (!already_exp) seg <- apply_time_expansion(seg)
    out_p <- paste0(base_path, ".wav")
    tuneR::writeWave(seg, out_p)
    paths <- c(paths, out_p)
  }
  
  return(paths)
}


#' Traite un seul fichier .wav
#'
#' @param filepath    Chemin complet du fichier source
#' @param out_dir     Répertoire de sortie
#' @param split_ch    Séparer les channels stéréo ?
#' @param already_exp Fichiers déjà en TE×10 ?
#' @return Liste avec statut, nombre de segments et chemins créés
process_wav_file <- function(filepath, out_dir, split_ch, already_exp) {
  filename  <- basename(filepath)
  base_name <- tools::file_path_sans_ext(filename)
  result    <- list(
    file         = filename,
    status       = "OK",
    n_segments   = 0L,
    output_files = c(),
    message      = ""
  )
  
  tryCatch({
    # Étape 1 : Lecture
    wave <- tuneR::readWave(filepath)
    
    # Étape 2 : Découpage (domaine brut, avant toute correction)
    max_dur  <- max_segment_duration(already_exp)
    segments <- segment_wave(wave, max_dur)
    n_seg    <- length(segments)
    result$n_segments <- n_seg
    
    # Étapes 3 + 4 + 5 : split + expansion (si besoin) + écriture
    all_paths <- c()
    
    for (i in seq_along(segments)) {
      sfx      <- segment_suffix(i, n_seg)
      base_out <- file.path(out_dir, paste0(base_name, sfx))
      paths    <- write_segment(segments[[i]], base_out, split_ch, already_exp)
      all_paths <- c(all_paths, paths)
    }
    
    result$output_files <- all_paths
    
    mode_str <- if (already_exp) {
      "déjà TE×10 | segments ≤50s | SR inchangé"
    } else {
      "full-spectrum | segments ≤5s brutes → ≤50s après TE×10 | SR÷10"
    }
    channel_str <- if (wave@stereo) {
      ifelse(split_ch, "stéréo→L+R", "stéréo conservée")
    } else {
      "mono"
    }
    result$message <- sprintf("%s | %s | %d segment(s)", mode_str, channel_str, n_seg)
    
  }, error = function(e) {
    result$status  <<- "ERREUR"
    result$message <<- conditionMessage(e)
  })
  
  return(result)
}


#' Lance le prétraitement sur un répertoire complet
#'
#' @param in_dir      Répertoire source
#' @param out_dir     Répertoire de sortie
#' @param already_exp Fichiers déjà en TE×10 ?
#' @param split_ch    Séparer les channels stéréo ?
#' @param recur       Traitement récursif ?
run_preprocessing <- function(in_dir, out_dir, already_exp, split_ch, recur = FALSE) {
  
  if (!dir.exists(in_dir)) {
    stop(sprintf("Le répertoire source n'existe pas : %s", in_dir))
  }
  
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
    message(sprintf("Répertoire de sortie créé : %s", out_dir))
  }
  
  wav_files <- list.files(
    path        = in_dir,
    pattern     = "\\.wav$",
    full.names  = TRUE,
    recursive   = recur,
    ignore.case = TRUE
  )
  
  if (length(wav_files) == 0) {
    message("Aucun fichier .wav trouvé dans le répertoire spécifié.")
    return(invisible(NULL))
  }
  
  max_dur  <- max_segment_duration(already_exp)
  mode_lbl <- ifelse(already_exp,
                     "TE×10 — SR inchangé, segments ≤50s",
                     "Full-spectrum — SR÷10, segments ≤5s brutes → ≤50s"
  )
  chan_lbl <- ifelse(split_ch, "Split L+R", "Canaux conservés")
  rec_lbl  <- ifelse(recur, "Récursif", "Non récursif")
  
  message(sprintf("\n=== Prétraitement des fichiers sons ==="))
  message(sprintf("Source  : %s", in_dir))
  message(sprintf("Sortie  : %s", out_dir))
  message(sprintf("Options : %d fichiers  |  %s  |  %s  |  %s\n",
                  length(wav_files), mode_lbl, chan_lbl, rec_lbl))
  
  results <- vector("list", length(wav_files))
  
  for (i in seq_along(wav_files)) {
    fp <- wav_files[i]
    
    if (recur) {
      rel_path <- dirname(sub(
        paste0("^", normalizePath(in_dir), .Platform$file.sep), "",
        normalizePath(fp)
      ))
      sub_out <- file.path(out_dir, rel_path)
      if (!dir.exists(sub_out)) dir.create(sub_out, recursive = TRUE)
    } else {
      sub_out <- out_dir
    }
    
    res <- process_wav_file(fp, sub_out, split_ch, already_exp)
    results[[i]] <- res
    
    # Barre de progression
    pct    <- round(i / length(wav_files) * 100)
    bar_w  <- 40L
    filled <- round(bar_w * i / length(wav_files))
    bar    <- paste0(strrep("█", filled), strrep("░", bar_w - filled))
    cat(sprintf("\r  [%s] %3d%%  (%d/%d)", bar, pct, i, length(wav_files)))
    flush.console()
  }
  cat("\n")
  
  # Rapport final
  n_ok  <- sum(sapply(results, function(r) r$status == "OK"))
  n_err <- sum(sapply(results, function(r) r$status == "ERREUR"))
  n_seg <- sum(sapply(results, function(r) r$n_segments))
  n_out <- sum(sapply(results, function(r) length(r$output_files)))
  
  message(sprintf("\n=== Rapport ==="))
  message(sprintf("Fichiers source traités  : %d / %d", n_ok, length(wav_files)))
  message(sprintf("Segments générés         : %d", n_seg))
  message(sprintf("Fichiers de sortie créés : %d", n_out))
  if (n_err > 0) {
    message(sprintf("Erreurs                  : %d", n_err))
    errors <- Filter(function(r) r$status == "ERREUR", results)
    for (e in errors) message(sprintf("  - %s : %s", e$file, e$message))
  }
  message(sprintf("Sortie disponible dans   : %s\n", out_dir))
  
  return(invisible(results))
}

# =============================================================================
# EXÉCUTION
# =============================================================================

results <- run_preprocessing(
  in_dir      = input_dir,
  out_dir     = output_dir,
  already_exp = already_expanded,
  split_ch    = split_channels,
  recur       = recursive
)

