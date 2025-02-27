#' Modified version of the crossValidate function from the RcppML package
#'
#' Existing crossValidate function is not compatible with data that contains both NA and 0 values.
#' This is a modified version to address that challenge, specifically designed to accomodate joint NMF analyses
#' that will always contain missing data. The function works with the same code as the original, but selects random values
#' to mask from those that are not "NA". Randomly masked values are combined with all other NA values for computation,
#' and are then used alone for model evaluation. 
#'
#'
#' @param data: Input data for joint NMF, in matrix or data.frame format
#' k: Number of groups to evaluate (Example, 2:8)
#' reps: Number of repeats, default=3
#' n: Percent of data to mask, default= 0.05 (5 percent)
#' @return object to be plotted using plot()
#' @export

joint_crossValidate <- function(data, k, reps = 3, n = 0.05, verbose = FALSE, ...) {
  verbose <- getOption("RcppML.verbose")
  options("RcppML.verbose" = FALSE)
  p <- list(...)
  defaults <- list("mask" = NULL)
  for (i in 1:length(defaults))
    if (is.null(p[[names(defaults)[[i]]]])) p[[names(defaults)[[i]]]] <- defaults[[i]]
  
  # Convert data to matrix if it isn't already
  data <- as.matrix(data)
  
  # Create initial mask from NA values
  na_mask <- is.na(data)
  
  # Replace NA values with 0 for NMF
  data[na_mask] <- 0
  
  if(!is.null(p$mask)){
    if(!is.character(p$mask)){
      if(canCoerce(p$mask, "ngCMatrix")){
        p$mask <- as(p$mask, "ngCMatrix")
      } else if(canCoerce(p$mask, "matrix")){
        p$mask <- as(as.matrix(p$mask), "ngCMatrix")
      } else stop("supplied masking value was invalid or not coercible to a matrix")
    }
  }
  
  results <- list()
  for(rep in 1:reps){
    if(verbose) cat("\nReplicate ", rep, ", rank: ", sep = "")
    
    # Create random mask for non-NA values
    if(!is.null(p$seed)) set.seed(p$seed + rep)
    
    # Calculate number of non-NA values to mask
    n_nonNA <- sum(!na_mask)
    n_to_mask <- round(n * n_nonNA)
    
    # Randomly select non-NA values to mask
    nonNA_indices <- which(!na_mask)
    masked_indices <- sample(nonNA_indices, n_to_mask)
    
    # Create combined mask (TRUE for both NA and randomly masked values)
    combined_mask <- na_mask
    combined_mask[masked_indices] <- TRUE
    
    # Convert to sparse matrix format
    mask_matrix <- as(combined_mask, "ngCMatrix")
    
    for(rank in k){
      if(verbose) cat(rank, " ")
      
      mask_22 <- mask_21 <- mask_11 <- mask_w1 <- mask_w2 <- p$mask
      m <- RcppML::nmf(data, rank, mask = mask_matrix, ...)
      
      # Evaluate only on randomly masked values (excluding original NA values)
      evaluation_mask <- matrix(FALSE, nrow = nrow(data), ncol = ncol(data))
      evaluation_mask[masked_indices] <- TRUE
      
      value <- evaluate(m, data, mask = evaluation_mask, missing_only = TRUE)
      results[[length(results) + 1]] <- c(rep, rank, value)
    }
  }
  
  results <- data.frame(do.call(rbind, results))
  colnames(results) <- c("rep", "k", "value")
  class(results) <- c("nmfCrossValidate", "data.frame")
  results$rep <- as.factor(results$rep)
  options("RcppML.verbose" = verbose)
  return(results)
}