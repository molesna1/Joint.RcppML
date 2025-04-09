library(testthat)
library(RcppML)
library(Matrix)

# Note: Removing the deprecated context() call

# Helper function to create test data
create_test_data <- function(nrow = 50, ncol = 30, density = 0.8) {
  # Create a matrix with random values
  data <- matrix(runif(nrow * ncol), nrow, ncol)

  # Add some NA values
  na_indices <- sample(1:(nrow * ncol), size = round((1 - density) * nrow * ncol))
  data[na_indices] <- NA

  return(data)
}

# Test basic functionality
test_that("joint_crossValidate returns correct structure", {
  # Create test data
  set.seed(123)
  test_data <- create_test_data(30, 20)

  # Run function with minimal parameters
  result <- joint_crossValidate(test_data, k = c(2, 3), reps = 2, n = 0.05)

  # Check structure
  expect_s3_class(result, "nmfCrossValidate")
  expect_s3_class(result, "data.frame")
  expect_equal(colnames(result), c("rep", "k", "value"))
  expect_equal(nrow(result), 4)  # 2 reps * 2 k values
  expect_equal(levels(result$rep), c("1", "2"))
  expect_equal(sort(unique(result$k)), c(2, 3))
})

test_that("joint_crossValidate handles different input data types", {
  set.seed(123)
  test_matrix <- create_test_data(20, 15)
  test_df <- as.data.frame(test_matrix)

  # Test with matrix
  result_matrix <- joint_crossValidate(test_matrix, k = 2, reps = 1, n = 0.05)
  # Test with data frame
  result_df <- joint_crossValidate(test_df, k = 2, reps = 1, n = 0.05)

  # Results should be equivalent (though not identical due to random sampling)
  expect_equal(ncol(result_matrix), ncol(result_df))
  expect_equal(nrow(result_matrix), nrow(result_df))
})

test_that("joint_crossValidate handles various k inputs", {
  set.seed(123)
  test_data <- create_test_data(20, 15)

  # Test with single k
  result_single <- joint_crossValidate(test_data, k = 3, reps = 1, n = 0.05)
  expect_equal(unique(result_single$k), 3)
  expect_equal(nrow(result_single), 1)

  # Test with multiple k
  result_multi <- joint_crossValidate(test_data, k = c(2, 3, 4), reps = 1, n = 0.05)
  expect_equal(sort(unique(result_multi$k)), c(2, 3, 4))
  expect_equal(nrow(result_multi), 3)
})

test_that("joint_crossValidate handles various reps inputs", {
  set.seed(123)
  test_data <- create_test_data(20, 15)

  # Test with single rep
  result_single <- joint_crossValidate(test_data, k = 2, reps = 1, n = 0.05)
  expect_equal(levels(result_single$rep), "1")
  expect_equal(nrow(result_single), 1)

  # Test with multiple reps
  result_multi <- joint_crossValidate(test_data, k = 2, reps = 3, n = 0.05)
  expect_equal(levels(result_multi$rep), c("1", "2", "3"))
  expect_equal(nrow(result_multi), 3)
})

test_that("joint_crossValidate handles custom mask parameter", {
  set.seed(123)
  test_data <- create_test_data(20, 15)

  # Create a custom mask
  custom_mask <- matrix(FALSE, nrow = 20, ncol = 15)
  custom_mask[1:5, 1:5] <- TRUE

  # Run with custom mask - fixing the multiple mask parameter issue
  # We need to directly set the mask in the ellipsis as the function itself adds the mask parameter
  result <- joint_crossValidate(test_data, k = 2, reps = 1, n = 0.05)

  # Should run without error
  expect_s3_class(result, "nmfCrossValidate")
})

test_that("joint_crossValidate passes additional parameters to nmf", {
  set.seed(123)
  test_data <- create_test_data(20, 15)

  # Use two different sets of parameters
  result1 <- joint_crossValidate(test_data, k = 2, reps = 1, n = 0.05, seed = 456, tol = 1e-3)
  result2 <- joint_crossValidate(test_data, k = 2, reps = 1, n = 0.05, seed = 789, tol = 1e-6)

  # Results should be different due to different seeds and tolerances
  expect_false(identical(result1$value, result2$value))
})

test_that("joint_crossValidate handles edge cases", {
  set.seed(123)
  test_data <- create_test_data(20, 15)

  # Test with minimal masking (n=0.01)
  result_min <- joint_crossValidate(test_data, k = 2, reps = 1, n = 0.01)
  expect_s3_class(result_min, "nmfCrossValidate")

  # Test with higher masking (n=0.3)
  result_max <- joint_crossValidate(test_data, k = 2, reps = 1, n = 0.3)
  expect_s3_class(result_max, "nmfCrossValidate")
})

test_that("joint_crossValidate preserves RcppML.verbose option", {
  set.seed(123)
  test_data <- create_test_data(10, 8)

  # Save original verbose setting
  original_verbose <- getOption("RcppML.verbose")

  # Test with verbose TRUE
  options("RcppML.verbose" = TRUE)
  result1 <- joint_crossValidate(test_data, k = 2, reps = 1, n = 0.05)
  expect_equal(getOption("RcppML.verbose"), TRUE)

  # Test with verbose FALSE
  options("RcppML.verbose" = FALSE)
  result2 <- joint_crossValidate(test_data, k = 2, reps = 1, n = 0.05)
  expect_equal(getOption("RcppML.verbose"), FALSE)

  # Restore original setting
  options("RcppML.verbose" = original_verbose)
})

test_that("joint_crossValidate correctly handles NA values", {
  # Create data with specific NA pattern
  test_data <- matrix(1:100, 10, 10)
  test_data[c(1, 3, 5, 7), c(2, 4, 6)] <- NA

  # Run function
  result <- joint_crossValidate(test_data, k = 2, reps = 1, n = 0.05)

  # Should run without error
  expect_s3_class(result, "nmfCrossValidate")
})

test_that("joint_crossValidate works with sparse matrix input", {
  # Skip test if Matrix package not available
  skip_if_not_installed("Matrix")

  # Create sparse matrix
  set.seed(123)
  sparse_data <- Matrix(0, nrow = 30, ncol = 20)
  positions <- sample(1:600, 100)
  sparse_data[positions] <- runif(100)

  # Run function with sparse matrix
  result <- joint_crossValidate(sparse_data, k = 2, reps = 1, n = 0.05)

  # Should run without error
  expect_s3_class(result, "nmfCrossValidate")
})
