#' Function to merge metadata from two datasets and extract label of interest
#'
#' For downstream joint NMF analysis, it is necessary to have labels of interest that align to the samples in your 
#' input data, but often the metadata is stored in multiple dataframes corresponding to the multiple data types. This
#' provides an easy workaround to extract a metadata label of interest and align it to the combined input data file, 
#' without needing to fully join two disparate tables containing different sets of metadata.
#' 
#' This function requires two dataframes containing sample metadata.
#' Note that your "group of interest" and "ID column" to map to each sample MUST have the same name in both dataframes
#'
#'
#' @param combined_df: NMF input data or combined dataframe, with column names corresponding to your samples in the correct order
#' meta1: First metadata table
#' meta2: Second metadata table
#' id_col: Name of the column in metadata that contains sample IDs ("Sample_ID")
#' meta_col: Metadata column(s) of interest
#' @return A vector of your aligned group label of choice, or a list of vectors if more than one group
#' @export

merge_metadata <- function(combined_df, meta1, meta2, id_col, meta_cols) {
  # Extract sample names
  sample_names <- colnames(combined_df)
  
  # Convert meta_cols to a vector if it's a single string
  if(!is.vector(meta_cols) || is.character(meta_cols) && length(meta_cols) == 1) {
    meta_cols <- c(meta_cols)
  }
  
  # Process each metadata column and store results in a list
  result_list <- lapply(meta_cols, function(meta_col) {
    # Create a helper function to extract and prepare metadata
    extract_meta <- function(meta_df) {
      selected <- meta_df[, c(id_col, meta_col), drop = FALSE]
      setNames(selected[[meta_col]], selected[[id_col]])
    }
    
    # Extract metadata from both dataframes
    meta1_vector <- extract_meta(meta1)
    meta2_vector <- extract_meta(meta2)
    
    # Determine data type based on both metadata sources
    is_numeric <- is.numeric(meta1[[meta_col]]) || is.numeric(meta2[[meta_col]])
    is_factor <- is.factor(meta1[[meta_col]]) || is.factor(meta2[[meta_col]])
    
    # Combine metadata with priority to meta1
    combined_metadata <- c(meta1_vector, meta2_vector[!names(meta2_vector) %in% names(meta1_vector)])
    
    # Convert to appropriate type if needed
    if(is_numeric) {
      combined_metadata <- as.numeric(as.character(combined_metadata))
    } else if(is_factor) {
      combined_metadata <- as.factor(as.character(combined_metadata))
    }
    
    # Match to sample order
    combined_metadata[sample_names]
  })
  
  # If only one column was processed, return just the vector instead of a list
  if(length(meta_cols) == 1) {
    return(result_list[[1]])
  }
  
  # Name the list elements with the column names
  names(result_list) <- meta_cols
  
  return(result_list)
}