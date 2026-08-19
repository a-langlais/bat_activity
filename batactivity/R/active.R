#' Analyze active bat survey data
#'
#' Computes active survey indicators by point: species richness, contact counts,
#' estimated contacts per hour, and behavior proportions.
#'
#' @param data A standard active survey data frame with `Place`, `Id`, and
#'   `Activity` columns.
#' @param duration Listening duration in minutes for each point.
#' @param npoint Number of sampled points.
#'
#' @return A data frame of active survey indicators.
#' @export
bat_active <- function(data, duration, npoint) {
  if (!is.data.frame(data)) {
    stop("data must be a data.frame.", call. = FALSE)
  }
  if (!is.numeric(duration) || length(duration) != 1 || is.na(duration) || duration <= 0) {
    stop("duration must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(npoint) || length(npoint) != 1 || is.na(npoint) || npoint < 1 || npoint != round(npoint)) {
    stop("npoint must be a positive integer.", call. = FALSE)
  }
  require_columns(data, c("Place", "Id", "Activity"), "data")
  if (nrow(data) == 0) {
    stop("data must contain at least one row.", call. = FALSE)
  }

  places <- unique(as.character(data$Place))
  rows <- lapply(places, function(place) {
    subset <- data[as.character(data$Place) == place, , drop = FALSE]
    contacts <- nrow(subset)
    data.frame(
      Point = place,
      n_sp = length(unique(subset$Id[!is.na(subset$Id)])),
      contacts = contacts,
      CPHe = contacts * 60 / duration,
      c_sonar = sum(subset$Activity == "Transit", na.rm = TRUE) / contacts * 100,
      c_social = sum(subset$Activity == "Social", na.rm = TRUE) / contacts * 100,
      c_feeding = sum(subset$Activity == "Chasse", na.rm = TRUE) / contacts * 100,
      stringsAsFactors = FALSE
    )
  })

  active_table <- do.call(rbind, rows)

  if (npoint > 1) {
    total_contacts <- nrow(data)
    global_summary <- data.frame(
      Point = "All",
      n_sp = length(unique(data$Id[!is.na(data$Id)])),
      contacts = total_contacts,
      CPHe = total_contacts * 60 / duration / npoint,
      c_sonar = sum(data$Activity == "Transit", na.rm = TRUE) / total_contacts * 100,
      c_social = sum(data$Activity == "Social", na.rm = TRUE) / total_contacts * 100,
      c_feeding = sum(data$Activity == "Chasse", na.rm = TRUE) / total_contacts * 100,
      stringsAsFactors = FALSE
    )
    active_table <- rbind(active_table, global_summary)
  }

  numeric_cols <- vapply(active_table, is.numeric, logical(1))
  active_table[numeric_cols] <- lapply(active_table[numeric_cols], function(x) {
    x[is.na(x)] <- 0
    x
  })
  character_cols <- vapply(active_table, is.character, logical(1))
  active_table[character_cols] <- lapply(active_table[character_cols], function(x) {
    x[is.na(x)] <- ""
    x
  })

  rownames(active_table) <- NULL
  active_table
}
