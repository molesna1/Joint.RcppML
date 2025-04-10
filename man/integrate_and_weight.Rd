% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Integrate_and_weight.R
\name{integrate_and_weight}
\alias{integrate_and_weight}
\title{Integrate and Weight Datasets for Joint NMF analysis with the RcppML package}
\usage{
integrate_and_weight(
  data1,
  data2,
  normalize_data1 = TRUE,
  normalize_data2 = TRUE
)
}
\arguments{
\item{data1}{First set of data (data.frame or matrix)}

\item{data2}{Second set of data (data.frame or matrix)}

\item{normalize_data1}{Column normalize data1. Use if you want to normalize samples within a dataset prior to weighting. (TRUE/FALSE)}

\item{normalize_data2}{Column normalize data2. Use if you want to normalize samples within a dataset prior to weighting. (TRUE/FALSE)}
}
\value{
A list containing a combined_matrix and sparse matrix with the integrated and weighted data.
The combined_matrix is in matrix form for easy viewing, while the sparse matrix can be used as input for the RcppML package
The output will also contain the weights used for scaling and the number of shared and unique samples
}
\description{
Function will first normalize samples within each input dataframe (optional), then join and weight two separate datasets with partially
shared samples, such that the sum of the data in each is equivalent for samples that are shared. The output is a list containing a matrix
and a sparse matrix. The sparse matrix can be used as the input for Joint NMF analysis.
}
\details{
This function requires an input of two datasets (dataframes or matrices) with samples in columns and features in rows.
These will typically be from two different modalities with partially matched samples.
Both sets of input data should be normalized as is appropriate for the data type. Additional sample normalization may be appropriate in certain
cases. An example would be raw beta values from methylation data, where sample normalization ensures that all samples have the same
sum total of beta values, ensuring that those with less global methylation (such as tumors) are given equal weight. Column normalization
should NOT be used if there are missing values in the dataset.
Column names for matched samples MUST be the same. All features (rownames) should all be unique.
}
