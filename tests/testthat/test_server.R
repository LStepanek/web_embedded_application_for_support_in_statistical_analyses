library(shinytest2)
library(testthat)

test_that("Data processing works", {
  app <- AppDriver$new(variant = NULL)

  # Upload test file
  test_file <- file.path("tests", "test_data.csv")
  write.csv(mtcars, test_file, row.names = FALSE)
  app$upload_file(file_upload = test_file)

  # Ensure dataset is processed correctly
  expect_true(app$wait_for_value(output = "data_table"))

  # Test if first column can be used as rownames
  app$set_inputs(use_first_col_as_rownames = TRUE)
  app$wait_for_value(output = "data_table")
})
