library(testthat)
source("../server.R")  # Load reactive functions for testing

test_that("Column type transformation", {
  sample_data <- data.frame(A = c("1", "2", "3"), B = c("A", "B", "C"))
  col_types <- c("N", "S")  # Expect column A as numeric, B as character
  
  # Assume function exists
  processed_data <- convert_column_types(sample_data, col_types)
  expect_type(processed_data$A, "double")
  expect_type(processed_data$B, "character")
})
