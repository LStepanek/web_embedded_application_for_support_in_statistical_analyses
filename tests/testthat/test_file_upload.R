library(shinytest2)
library(testthat)

test_that("CSV file upload works", {
  app <- AppDriver$new(variant = NULL)

  test_file <- file.path("tests", "test_data.csv")
  write.csv(mtcars, test_file, row.names = FALSE)

  app$upload_file(file_upload = test_file)
  expect_true(app$wait_for_value(output = "data_table"))

  # Snapshot the table output for regression testing
  app$expect_screenshot("uploaded_data_table")
})
