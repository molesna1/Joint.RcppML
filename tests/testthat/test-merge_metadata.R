# Sample data for tests
metadata1 <- data.frame(
  SampleID = c("S1", "S2", "S3"),
  Group = factor(c("A", "B", "A")),
  Age = c(25, 30, 28),
  stringsAsFactors = FALSE
)

metadata2 <- data.frame(
  SampleID = c("S4", "S5", "S6"),
  Group = factor(c("C", "B", "C")),
  Age = c(40, 35, 38),
  stringsAsFactors = FALSE
)

data_matrix <- matrix(
  rnorm(18), nrow = 3,
  dimnames = list(c("Gene1", "Gene2", "Gene3"), c("S1", "S2", "S3", "S4", "S5", "S6"))
)

# Test 1: Single metadata column (numeric), present in both metadata1 and metadata2
test_that("Merges numeric column correctly across both metadata frames", {
  result <- merge_metadata(metadata1, metadata2, id_col = "SampleID", data_matrix, "Age")
  expect_equal(length(result), ncol(data_matrix))
  expect_equal(result, c(25, 30, 28, 40, 35, 38))
})

# Test 2: Column exists only in metadata1
test_that("Handles column only in metadata1", {
  metadata2_mod <- metadata2[, -which(names(metadata2) == "Group")]
  result <- merge_metadata(metadata1, metadata2_mod, "SampleID", data_matrix, "Group")
  expect_equal(as.character(result), c("A", "B", "A", NA, NA, NA))
})

# Test 3: Column exists only in metadata2
test_that("Handles column only in metadata2", {
  metadata1_mod <- metadata1[, -which(names(metadata1) == "Group")]
  result <- merge_metadata(metadata1_mod, metadata2, "SampleID", data_matrix, "Group")
  expect_equal(as.character(result), c(NA, NA, NA, "C", "B", "C"))
})

# Test 4: Column not found in either metadata dataframe
test_that("Handles missing column in both metadata", {
  result <- merge_metadata(metadata1, metadata2, "SampleID", data_matrix, "NonexistentColumn")
  expect_true(all(is.na(result)))
  expect_equal(length(result), ncol(data_matrix))
})
