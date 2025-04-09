#' Function to merge metadata from two datasets and extract label of interest
#'
#' For downstream joint NMF analysis, you need to align labels of interest to the samples in your input data,
#' but often the metadata is stored in multiple dataframes corresponding to the different data types. This
#' provides an easy workaround to extract a metadata label of interest and align it to the combined input data file
#' in one line.
#'
#' This function requires two dataframes containing sample metadata.
#' Note that your "group of interest" and sample "ID column" MUST have the same name in both dataframes
#' if you want group labels from both dataframes. Labels only present in one data type are acceptable
#' and NA values will be used for any missing information.
#'
#'
#' @param data_matrix NMF input data or combined dataframe, with column names corresponding to your samples
#' @param metadata1 First metadata table
#' @param metadata2 Second metadata table
#' @param id_col Name of the column in metadata that contains sample IDs ("Sample_ID").
#' Must be the same for both dataframes
#' @param metadata_columns Metadata column(s) of interest
#' @return A vector of your aligned group label of choice, or a list of vectors if there is more than one group
#' @export

merge_metadata <- function(metadata1, metadata2, id_col, data_matrix, metadata_columns) {
  # Convert metadata_columns to a character vector if it's a single string
  if (is.character(metadata_columns) && length(metadata_columns) == 1) {
    metadata_columns <- c(metadata_columns)
  }

  # Get matrix column names (Sample IDs)
  matrix_ids <- colnames(data_matrix)

  # Initialize results list
  result_list <- list()

  # Process each metadata column
  for (col in metadata_columns) {
    # Check if column exists in either dataframe
    in_metadata1 <- col %in% colnames(metadata1)
    in_metadata2 <- col %in% colnames(metadata2)

    if (!in_metadata1 && !in_metadata2) {
      warning(paste("Column", col, "not found in either metadata dataframe"))
      # Create a vector of NAs with the same length as matrix columns
      result_list[[col]] <- rep(NA, length(matrix_ids))
      next
    }

    # Determine column type (factor, numeric, or other)
    col_type <- NULL
    if (in_metadata1 && is.factor(metadata1[[col]])) {
      col_type <- "factor"
    } else if (in_metadata2 && is.factor(metadata2[[col]])) {
      col_type <- "factor"
    } else if (in_metadata1 && is.numeric(metadata1[[col]])) {
      col_type <- "numeric"
    } else if (in_metadata2 && is.numeric(metadata2[[col]])) {
      col_type <- "numeric"
    }

    # Create a named vector to store results
    aligned_vector <- rep(NA, length(matrix_ids))
    names(aligned_vector) <- matrix_ids

    # Fill values from metadata1 (first priority)
    if (in_metadata1) {
      metadata1_map <- as.character(metadata1[[col]])
      names(metadata1_map) <- metadata1[[id_col]]

      # Match and fill values
      for (id in matrix_ids) {
        if (id %in% metadata1[[id_col]]) {
          id_index <- which(metadata1[[id_col]] == id)[1]
          aligned_vector[id] <- metadata1_map[id_index]
        }
      }
    }

    # Fill values from metadata2 (where metadata1 doesn't have values)
    if (in_metadata2) {
      metadata2_map <- as.character(metadata2[[col]])
      names(metadata2_map) <- metadata2[[id_col]]

      # Match and fill values where still NA
      for (id in matrix_ids) {
        if (is.na(aligned_vector[id]) && id %in% metadata2[[id_col]]) {
          id_index <- which(metadata2[[id_col]] == id)[1]
          aligned_vector[id] <- metadata2_map[id_index]
        }
      }
    }

    # Coerce to appropriate type if needed
    if (!is.null(col_type)) {
      if (col_type == "factor") {
        aligned_vector <- as.factor(aligned_vector)
      } else if (col_type == "numeric") {
        aligned_vector <- as.numeric(aligned_vector)
      }
    }

    # Add to results list
    result_list[[col]] <- aligned_vector
  }

  # Return a vector if only one column, otherwise return the list
  if (length(metadata_columns) == 1) {
    return(result_list[[1]])
  } else {
    return(result_list)
  }
}
