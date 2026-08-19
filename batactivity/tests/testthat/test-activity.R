test_that("bat_active computes active survey indicators", {
  input <- data.frame(
    Place = c("P1", "P1", "P2"),
    Id = c("Pip", "Nyc", "Pip"),
    Activity = c("Transit", "Chasse", "Social"),
    stringsAsFactors = FALSE
  )

  result <- bat_active(input, duration = 10, npoint = 2)

  expect_equal(result$Point, c("P1", "P2", "All"))
  expect_equal(result$contacts, c(2, 1, 3))
  expect_equal(result$n_sp, c(2, 1, 2))
  expect_equal(result$CPHe[[3]], 9)
})

test_that("species_place_activity computes passive indicators", {
  input <- data.frame(
    Place = c("S1", "S1", "S1"),
    Id = c("Pip", "Pip", "Nyc"),
    Night_Date = as.Date(c("2026-08-18", "2026-08-18", "2026-08-19")),
    Date_Time = as.POSIXct(
      c("2026-08-18 22:10", "2026-08-18 22:20", "2026-08-19 23:00"),
      tz = "Europe/Paris"
    ),
    Year = c("2026", "2026", "2026"),
    Month = c("08", "08", "08"),
    Week = c("34", "34", "34"),
    Day = c("18", "18", "19"),
    Time = c("22:10", "22:20", "23:00"),
    Hour = c(22, 22, 23),
    Minute = c(10, 20, 0),
    stringsAsFactors = FALSE
  )

  result <- species_place_activity(input, nights = 2, record_time = c("22:00", "06:00"))

  expect_equal(nrow(result), 2)
  expect_true(all(c("contacts", "night_positive", "mean_CPH") %in% names(result)))
})
