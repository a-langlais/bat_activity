test_that("standardize_table converts SonoChiro data without writing by default", {
  input <- data.frame(
    File = c("BST-1_20180610_223019_295.wav", "BST-1_20180611_015219_944.wav"),
    Id = c("Pipnat/Pippip", "Pipkuh/Pipnat"),
    stringsAsFactors = FALSE
  )

  old_wd <- getwd()
  temp_dir <- tempdir()
  setwd(temp_dir)
  on.exit(setwd(old_wd), add = TRUE)
  if (file.exists("BatTable.csv")) {
    file.remove("BatTable.csv")
  }

  result <- standardize_table(input, software = "SonoChiro")

  expect_s3_class(result, "data.frame")
  expect_equal(names(result), c(
    "File", "Place", "Id", "Night_Date", "Date_Time", "Date",
    "Year", "Month", "Week", "Day", "Time", "Hour", "Minute"
  ))
  expect_false(file.exists("BatTable.csv"))
  expect_equal(as.character(result$Night_Date[[2]]), "2018-06-10")
})

test_that("TableFormatage writes only when output_file is provided", {
  input <- data.frame(
    File = "BST-1_20180610_223019_295.wav",
    Id = "Pipnat/Pippip",
    stringsAsFactors = FALSE
  )
  output_file <- tempfile(fileext = ".csv")

  result <- TableFormatage(input, sftw = "SonoChiro", output_file = output_file)

  expect_true(file.exists(output_file))
  expect_equal(nrow(result), 1)
})

test_that("standardize_table reads Tadarida timestamps after variable prefixes", {
  input <- data.frame(
    "nom du fichier" = c(
      "Car791025-2026-Pass1-Z4-GAL-U7_20260625_231344_000",
      "Car791025-2026-Pass1-Z4-GAL-U7_20260626_001446_000"
    ),
    tadarida_taxon = c("Barbar", "Barbar"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  result <- standardize_table(input, software = "Tadarida")

  expect_equal(result$Place, c("GAL-U7", "GAL-U7"))
  expect_equal(as.character(result$Date), c("2026-06-25", "2026-06-26"))
  expect_equal(result$Time, c("23:13", "00:14"))
  expect_equal(as.character(result$Night_Date[[2]]), "2026-06-25")
})

test_that("the application uses the same Tadarida standardization as the package", {
  app_function <- test_path("..", "..", "..", "shiny-app", "R", "Standardize.R")
  source(app_function, local = TRUE)
  input <- data.frame(
    "nom du fichier" = "Car791025-2026-Pass1-Z4-GAL-U7_20260625_231344_000.wav",
    validateur_taxon = "Pippip",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  expect_equal(
    standardize_table_app(input, software = "Tadarida"),
    standardize_table(input, software = "Tadarida")
  )
})
