library(shinytest2)
library(testthat)

test_that("Shiny app UI loads", {
  app <- AppDriver$new(variant = NULL)
  
  # Verify UI elements exist
  expect_true(app$wait_for_value(output = "data_preview_label"))
  expect_true(app$wait_for_value(output = "data_table"))
  
  # Check built-in dataset selection
  app$set_inputs(use_builtin = TRUE)
  app$set_inputs(builtin_dataset = "mtcars")
  app$wait_for_value(output = "data_table")
  
  # Snapshot UI for regression testing
  app$expect_screenshot()
})
