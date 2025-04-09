library(testthat)
library(ggplot2)
library(uwot)

# Dummy NMF model with H matrix
dummy_H <- matrix(runif(20), nrow = 4, ncol = 5)
colnames(dummy_H) <- paste0("Sample", 1:5)
nmf_model <- list(h = dummy_H)

# Dummy group labels
group_labels_vector <- c("A", "B", "A", "B", "A")
group_labels_list <- list(
  c("A", "B", "A", "B", "A"),
  c("X", "Y", "X", "Y", "Y")
)

# Dummy custom color sets
custom_colors_named <- c(A = "red", B = "blue")
custom_colors_list <- list(
  c(A = "red", B = "blue"),
  c(X = "green", Y = "purple")
)

# Test 1: Returns a ggplot for vector input
test_that("Returns ggplot object with single vector group labels", {
  result <- nmf_umap(nmf_model, group_labels_vector, n_neighbors = 2)
  expect_s3_class(result, "gg")
  expect_true("Group" %in% names(result$data))
  expect_equal(nrow(result$data), length(group_labels_vector))
})

# Test 2: Returns list of ggplot objects with list input
test_that("Returns list of ggplots with group label list input", {
  result <- nmf_umap(nmf_model, group_labels_list, n_neighbors = 2)
  expect_type(result, "list")
  expect_length(result, length(group_labels_list))
  lapply(result, function(p) expect_s3_class(p, "gg"))
})

# Test 3: Works with custom colors (named vector)
test_that("Applies named custom colors for vector input", {
  result <- nmf_umap(nmf_model, group_labels_vector, custom_colors_named, n_neighbors = 2)
  expect_s3_class(result, "gg")
  expect_true("Group" %in% names(result$data))
})

# Test 4: Works with custom colors list for list input
test_that("Applies custom color list correctly for multiple groupings", {
  result <- nmf_umap(nmf_model, group_labels_list, custom_colors_list, n_neighbors = 2)
  expect_type(result, "list")
  expect_length(result, 2)
  lapply(result, function(p) expect_s3_class(p, "gg"))
})

# Test 5: Returns expected number of rows in plotted data
test_that("UMAP embedding matches number of samples", {
  result <- nmf_umap(nmf_model, group_labels_vector, n_neighbors = 2)
  expect_equal(nrow(result$data), ncol(dummy_H))
})
