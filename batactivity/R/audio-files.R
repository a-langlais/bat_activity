#' Plan or apply timestamp-based audio file renaming
#'
#' Builds new `.wav` file names from each file's modification time. By default
#' the function returns a dry-run plan and does not rename files.
#'
#' @param files Character vector of audio file paths.
#' @param output_dir Optional directory for renamed files. Defaults to each
#'   file's current directory.
#' @param dry_run Logical. When `TRUE`, return the rename plan without changing
#'   files.
#' @param extension File extension to use for new file names.
#'
#' @return A data frame with old paths, new paths, and rename status.
#' @export
rename_audio_files <- function(files,
                               output_dir = NULL,
                               dry_run = TRUE,
                               extension = ".wav") {
  if (!is.character(files) || length(files) == 0) {
    stop("files must be a non-empty character vector.", call. = FALSE)
  }
  if (!is.logical(dry_run) || length(dry_run) != 1 || is.na(dry_run)) {
    stop("dry_run must be TRUE or FALSE.", call. = FALSE)
  }

  info <- file.info(files)
  missing_files <- files[is.na(info$mtime)]
  if (length(missing_files) > 0) {
    stop("Some files do not exist: ", paste(missing_files, collapse = ", "), call. = FALSE)
  }

  if (is.null(output_dir)) {
    output_dir <- dirname(files)
  } else if (!dir.exists(output_dir)) {
    stop("output_dir does not exist.", call. = FALSE)
  } else {
    output_dir <- rep(output_dir, length(files))
  }

  base_names <- format(info$mtime, "%Y%m%d_%H%M%S")
  suffix <- stats::ave(base_names, base_names, FUN = seq_along)
  duplicated_names <- stats::ave(base_names, base_names, FUN = length) > 1
  base_names[duplicated_names] <- paste0(base_names[duplicated_names], "_", suffix[duplicated_names])

  extension <- sub("^\\.*", ".", extension)
  new_paths <- file.path(output_dir, paste0(base_names, extension))
  plan <- data.frame(
    old_path = files,
    new_path = new_paths,
    renamed = NA,
    stringsAsFactors = FALSE
  )

  if (!isTRUE(dry_run)) {
    plan$renamed <- file.rename(files, new_paths)
  }

  plan
}
