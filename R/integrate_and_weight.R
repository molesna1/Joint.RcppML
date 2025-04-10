#' Integrate and Weight Datasets for Joint NMF analysis with the RcppML package
#'
#' Function will first normalize samples within each input dataframe (optional), then join and weight two separate datasets with partially
#' shared samples, such that the sum of the data in each is equivalent for samples that are shared. The output is a list containing a matrix
#' and a sparse matrix. The sparse matrix can be used as the input for Joint NMF analysis.
#'
#'This function requires an input of two datasets (dataframes or matrices) with samples in columns and features in rows.
#'These will typically be from two different modalities with partially matched samples.
#'Both sets of input data should be normalized as is appropriate for the data type. Additional sample normalization may be appropriate in certain
#'cases. An example would be raw beta values from methylation data, where sample normalization ensures that all samples have the same
#'sum total of beta values, ensuring that those with less global methylation (such as tumors) are given equal weight. Column normalization
#'should NOT be used if there are missing values in the dataset.
#'Column names for matched samples MUST be the same. All features (rownames) should all be unique.
#'
#' @param data1 First set of data (data.frame or matrix)
#' @param data2 Second set of data (data.frame or matrix)
#' @param normalize_data1 Column normalize data1. Use if you want to normalize samples within a dataset prior to weighting. (TRUE/FALSE)
#' @param normalize_data2 Column normalize data2. Use if you want to normalize samples within a dataset prior to weighting. (TRUE/FALSE)
#' @return A list containing a combined_matrix and sparse matrix with the integrated and weighted data.
#' The combined_matrix is in matrix form for easy viewing, while the sparse matrix can be used as input for the RcppML package
#' The output will also contain the weights used for scaling and the number of shared and unique samples
#' @export


integrate_and_weight <- function(data1, data2, normalize_data1 = TRUE, normalize_data2 = TRUE) {

  # Convert data frames to matrices if needed and ensure numeric
  if(is.data.frame(data1)) {
    data1 <- as.matrix(data1)
    data1 <- matrix(as.numeric(data1), nrow=nrow(data1),
                    dimnames=list(rownames(data1), colnames(data1)))
  }
  if(is.data.frame(data2)) {
    data2 <- as.matrix(data2)
    data2 <- matrix(as.numeric(data2), nrow=nrow(data2),
                    dimnames=list(rownames(data2), colnames(data2)))
  }

  # Check for non-numeric values
  if(any(!is.numeric(data1))) {
    stop("Dataset 1 contains non-numeric values")
  }
  if(any(!is.numeric(data2))) {
    stop("Dataset 2 contains non-numeric values")
  }

  # Get sample names (column names) from both datasets
  samples1 <- colnames(data1)
  samples2 <- colnames(data2)
  features1 <- rownames(data1)
  features2 <- rownames(data2)

  if(is.null(samples1) || is.null(samples2)) {
    stop("Both datasets must have column names (sample IDs)")
  }

  # Check for overlapping features
  shared_features <- intersect(features1, features2)
  if(length(shared_features) > 0) {
    warning("Datasets contain shared features. Features should be unique between datasets.")
  }

  # Find shared samples
  shared_samples <- intersect(samples1, samples2)
  all_samples <- union(samples1, samples2)

  if(length(shared_samples) == 0) {
    stop("No shared samples found between datasets")
  }

  # Store original column sums
  col_sums1 <- colSums(data1, na.rm=TRUE)
  col_sums2 <- colSums(data2, na.rm=TRUE)

  # Normalize each column in datasets based on parameters
  if(normalize_data1) {
    normalized_data1 <- sweep(data1, 2, col_sums1, "/")
    data1 <- normalized_data1
  }

  if(normalize_data2) {
    normalized_data2 <- sweep(data2, 2, col_sums2, "/")
    data2 <- normalized_data2
  }

  # Calculate sums using all features but only shared samples
  sum1 <- sum(data1[, shared_samples, drop=FALSE], na.rm=TRUE)
  sum2 <- sum(data2[, shared_samples, drop=FALSE], na.rm=TRUE)

  # Calculate weighting factor
  if(sum1 > sum2) {
    weight_factor <- sum2/sum1
    weighted_data1 <- data1 * weight_factor
    weighted_data2 <- data2
  } else {
    weight_factor <- sum1/sum2
    weighted_data1 <- data1
    weighted_data2 <- data2 * weight_factor
  }

  # Create expanded matrices to include all samples
  expanded_data1 <- matrix(NA,
                           nrow=nrow(weighted_data1),
                           ncol=length(all_samples),
                           dimnames=list(rownames(weighted_data1), all_samples))
  expanded_data2 <- matrix(NA,
                           nrow=nrow(weighted_data2),
                           ncol=length(all_samples),
                           dimnames=list(rownames(weighted_data2), all_samples))

  # Fill in the values from weighted datasets
  expanded_data1[, samples1] <- weighted_data1
  expanded_data2[, samples2] <- weighted_data2

  # Create combined matrix
  combined_matrix <- rbind(expanded_data1, expanded_data2)

  # Create and save sparse matrix
  sparse_matrix <- Matrix::Matrix(combined_matrix, sparse = TRUE)

  # Return the dense combined matrix and additional information
  return(list(
    combined_matrix = combined_matrix,
    sparse_matrix = sparse_matrix,
    weight_factor = weight_factor,
    original_sums = list(sum1 = sum1, sum2 = sum2),
    normalized = list(data1 = normalize_data1, data2 = normalize_data2),
    column_sums = list(data1 = col_sums1, data2 = col_sums2),
    shared_samples = shared_samples,
    unique_samples1 = setdiff(samples1, samples2),
    unique_samples2 = setdiff(samples2, samples1)
  ))
}
