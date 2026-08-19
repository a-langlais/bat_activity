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
