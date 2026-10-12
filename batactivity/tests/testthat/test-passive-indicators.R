passive_fixture <- function() {
  data.frame(
    Place = c("GAL-U1", "GAL-U1", "GAL-U1", "GAL-U1", "GAL-U1"),
    Id = c("Pippip", "Pippip", "Pippip", "Pippip", "Myodau"),
    Night_Date = as.Date(c("2026-06-25", "2026-06-25", "2026-06-25", "2026-06-26", "2026-06-25")),
    Date_Time = c(
      "2026-06-25 21:15:00", "2026-06-25 21:15:00", "2026-06-26 00:10:00",
      "2026-06-26 22:05:00", "2026-06-26 04:56:00"
    ),
    stringsAsFactors = FALSE
  )
}

test_that("passive indicators include zero nights and count positive times distinctly", {
  result <- species_place_activity(
    passive_fixture(),
    record_time = c("20:00", "08:00"),
    timezone = "Europe/Paris"
  )

  pip <- result[result$Id == "Pippip", , drop = FALSE]
  myo <- result[result$Id == "Myodau", , drop = FALSE]

  expect_equal(pip$contacts, 4)
  expect_equal(pip$night_positive, 2)
  expect_equal(pip$hour_positive, 3)
  expect_equal(pip$minute_positive, 3)
  expect_equal(pip$min_CPN, 1)
  expect_equal(pip$mean_CPN, 2)
  expect_equal(pip$max_CPN, 3)
  expect_equal(pip$min_CPH, 1 / 12)
  expect_equal(pip$mean_CPH, 2 / 12)
  expect_equal(pip$max_CPH, 3 / 12)

  expect_equal(myo$contacts, 1)
  expect_equal(myo$night_positive, 1)
  expect_equal(myo$hour_positive, 1)
  expect_equal(myo$minute_positive, 1)
  expect_equal(myo$min_CPN, 0)
  expect_equal(myo$mean_CPN, 0.5)
})

test_that("fixed-hour mode excludes contacts outside the selected window", {
  data <- passive_fixture()
  data <- rbind(
    data,
    data.frame(
      Place = "GAL-U1", Id = "Pippip", Night_Date = as.Date("2026-06-25"),
      Date_Time = "2026-06-25 12:00:00", stringsAsFactors = FALSE
    )
  )

  result <- species_place_activity(data, record_time = c("20:00", "08:00"))
  expect_equal(result$contacts[result$Id == "Pippip"], 4)
})

test_that("fixed-hour CPH uses the actual duration on a daylight-saving transition", {
  data <- data.frame(
    Place = "GAL-U1", Id = "Pippip", Night_Date = as.Date("2026-03-28"),
    Date_Time = "2026-03-28 21:00:00", stringsAsFactors = FALSE
  )

  result <- species_place_activity(
    data,
    record_time = c("20:00", "08:00"),
    timezone = "Europe/Paris"
  )

  expect_equal(result$contacts, 1)
  expect_equal(result$mean_CPH, 1 / 11)
})

test_that("solar mode retains contacts and uses the exact solar duration for CPH", {
  skip_if_not_installed("suncalc")
  data <- data.frame(
    Place = c("GAL-U1", "GAL-U1"),
    Id = c("Pippip", "Pippip"),
    Night_Date = as.Date(c("2026-06-25", "2026-06-25")),
    Date_Time = c("2026-06-25 12:00:00", "2026-06-26 00:10:00"),
    stringsAsFactors = FALSE
  )
  offsets <- c(before_sunset = 30, after_sunrise = 30)

  result <- species_place_activity(
    data,
    city = "Bordeaux",
    sun_offsets = offsets,
    timezone = "Europe/Paris"
  )
  solar <- suncalc::getSunlightTimes(as.Date("2026-06-25"), 44.8378, -0.5792, keep = "sunset")$sunset
  sunrise <- suncalc::getSunlightTimes(as.Date("2026-06-26"), 44.8378, -0.5792, keep = "sunrise")$sunrise
  expected_duration <- as.numeric(difftime(sunrise + 30 * 60, solar - 30 * 60, units = "hours"))

  expect_equal(result$contacts, 2)
  expect_equal(result$night_positive, 1)
  expect_equal(result$hour_positive, 2)
  expect_equal(result$minute_positive, 2)
  expect_equal(result$mean_CPH, 2 / expected_duration)
})

test_that("the application and package use the same passive calculation", {
  app_function <- test_path("..", "..", "..", "shiny-app", "R", "BatPassive.R")
  source(app_function, local = TRUE)

  package_result <- species_place_activity(
    passive_fixture(), record_time = c("20:00", "08:00"), timezone = "Europe/Paris"
  )
  app_result <- BatPassive(
    passive_fixture(), record_time = c("20:00", "08:00"), timezone = "Europe/Paris"
  )

  expect_equal(app_result, package_result)
})
