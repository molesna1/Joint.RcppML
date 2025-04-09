library(testthat)

# Dummy NMF model
W_matrix <- matrix(runif(20), nrow = 5, ncol = 4)
rownames(W_matrix) <- paste0("Gene", 1:5)
colnames(W_matrix) <- paste0("Comp", 1:4)
nmf_model <- list(W = W_matrix)

# Single filter dataframe (subset of rownames)
filter_df1 <- data.frame(dummy = 1:3)
rownames(filter_df1) <- c("Gene1", "Gene2", "Gene3")

# Another filter dataframe (non-overlapping)
filter_df2 <- data.frame(dummy = 1:3)
rownames(filter_df2) <- c("X1", "X2", "X3")

# Test 1: Basic top features without filters
test_that("Returns top features for all components", {
  result <- topFeatures(nmf_model, n = 2)
  expect_type(result, "list")
  expect_equal(length(result), ncol(W_matrix))
  lapply(result, function(x) expect_length(x, 2))
})

# Test 2: Subsetting specific components
test_that("Returns top features for selected components only", {
  result <- topFeatures(nmf_model, n = 3, components = c(1, 3))
  expect_equal(names(result), c("Comp1", "Comp3"))
  lapply(result, function(x) expect_length(x, 3))
})

# Test 3: Invalid component index throws error
test_that("Invalid component index throws an error", {
  expect_error(topFeatures(nmf_model, components = c(10)), "Invalid component indices specified")
})

# Test 4: Filtering with a single dataframe
test_that("Filters top features using a single filter dataframe", {
  result <- topFeatures(nmf_model, n = 3, filter_dfs = list(filter_df1))
  expect_type(result, "list")
  expect_equal(length(result), ncol(W_matrix))
  expect_true(all(unlist(result) %in% rownames(filter_df1)))
})

# Test 5: Filtering with a single dataframe that has no match
test_that("Returns warning and empty sets if no features match single filter", {
  expect_warning({
    result <- topFeatures(nmf_model, n = 3, filter_dfs = list(filter_df2))
  }, "No features found in filter dataframe")
  expect_true(all(lengths(result) == 0))
})

# Test 6: Filtering with multiple dataframes
test_that("Returns top features for each component per filter dataframe", {
  result <- topFeatures(nmf_model, n = 2, filter_dfs = list(filter_df1, filter_df2))

  # Only components with matching features should be included
  expect_true(all(grepl("_df[1-2]$", names(result))))

  # There should be results for df1
  expect_true(any(grepl("_df1$", names(result))))

  # There should be no results for df2 (all skipped due to no matching features)
  expect_false(any(grepl("_df2$", names(result))))
})

# Test 7: Handles case when top n > number of features
test_that("Handles case where n > number of matching features", {
  result <- topFeatures(nmf_model, n = 10, filter_dfs = list(filter_df1))
  expect_true(all(lengths(result) <= 3))  # Only 3 matching features in filter_df1
})

