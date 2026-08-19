test_that("rename_audio_files plans renames without side effects by default", {
  old_file <- tempfile(fileext = ".wav")
  file.create(old_file)

  plan <- rename_audio_files(old_file)

  expect_s3_class(plan, "data.frame")
  expect_equal(plan$old_path, old_file)
  expect_true(file.exists(old_file))
  expect_true(is.na(plan$renamed))
})
