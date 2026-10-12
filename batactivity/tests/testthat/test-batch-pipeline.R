run_batch_script <- function(script, arguments, working_directory) {
  rscript <- file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  )
  output <- system2(rscript, c(script, arguments), stdout = TRUE, stderr = TRUE)
  status <- attr(output, "status")
  expect_true(is.null(status) || status == 0, info = paste(output, collapse = "\n"))
  invisible(output)
}

test_that("batch scripts standardize, calculate and correct a complete file chain", {
  repository_root <- normalizePath(test_path("..", "..", ".."), winslash = "/")
  temporary_directory <- tempfile("bat-activity-pipeline-")
  dir.create(temporary_directory)
  on.exit(unlink(temporary_directory, recursive = TRUE), add = TRUE)

  input_file <- file.path(temporary_directory, "Car791053-2026-Pass1-Z2-GAL-U1_clean.csv")
  utils::write.table(
    data.frame(
      "nom du fichier" = c(
        "Car791053-2026-Pass1-Z2-GAL-U1_20260625_210000_000.wav",
        "Car791053-2026-Pass1-Z2-GAL-U1_20260626_001000_000.wav",
        "Car791053-2026-Pass1-Z2-GAL-U1_20260626_045600_000.wav"
      ),
      validateur_taxon = c("Pippip", "Pippip", "Myodau"),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    file = input_file, row.names = FALSE, col.names = TRUE, sep = ";", quote = TRUE
  )

  old_working_directory <- getwd()
  setwd(repository_root)
  on.exit(setwd(old_working_directory), add = TRUE)

  run_batch_script(
    file.path("scripts", "03_standardiser_lot.R"),
    c("Tadarida", "--dossier", temporary_directory),
    repository_root
  )
  standardized_file <- file.path(temporary_directory, "Car791053-2026-Pass1-Z2-GAL-U1_clean_format.csv")
  expect_true(file.exists(standardized_file))

  run_batch_script(
    file.path("scripts", "04_indicateurs_passifs_lot.R"),
    c("--ville", "Bordeaux", "--fuseau", "Europe/Paris", "--horaires", "20:00", "08:00", "--dossier", temporary_directory),
    repository_root
  )
  raw_results_file <- file.path(temporary_directory, "Car791053-2026-Pass1-Z2-GAL-U1_raw_results.csv")
  expect_true(file.exists(raw_results_file))
  raw_results <- utils::read.csv2(raw_results_file, stringsAsFactors = FALSE)
  expect_equal(raw_results$contacts[raw_results$Id == "Pippip"], 2)
  expect_equal(raw_results$contacts[raw_results$Id == "Myodau"], 1)

  run_batch_script(
    file.path("scripts", "05_correction_detectabilite.R"),
    c("--milieu", "ferme", "--dossier", temporary_directory),
    repository_root
  )
  corrected_file <- file.path(temporary_directory, "Car791053-2026-Pass1-Z2-GAL-U1_corrected_results.csv")
  expect_true(file.exists(corrected_file))
  corrected_results <- utils::read.csv2(corrected_file, stringsAsFactors = FALSE)
  expect_identical(names(corrected_results), names(raw_results))
  expect_equal(corrected_results$contacts[corrected_results$Id == "Myodau"], 1.2)
  expect_equal(
    corrected_results$minute_positive[corrected_results$Id == "Myodau"],
    raw_results$minute_positive[raw_results$Id == "Myodau"]
  )
})
