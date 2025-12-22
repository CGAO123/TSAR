#' Example normalized TSA dataset
#'
#' A demonstration dataset of TSA measurements that have been normalized and
#' merged into the TSAR/TSAR-new long format (one row per observation).
#' Suitable for examples in \code{\link{TSA_wells_plot}} and
#' \code{\link{TSA_compare_plot}}.
#'
#' @format A data frame with 63,544 rows and 14 variables:
#' \describe{
#'   \item{well_ID}{Unique well identifier (character).}
#'   \item{Well}{Well label (character), e.g., "B01".}
#'   \item{Protein}{Protein name/label (character).}
#'   \item{Ligand}{Ligand name/label (character).}
#'   \item{Tm}{Estimated melting temperature for the well/condition (numeric).}
#'   \item{dTm D}{Delta Tm field as stored in the dataset (numeric; may be NA).}
#'   \item{Analysis Group}{Analysis group label (character).}
#'   \item{Flags}{QC/flag field (character or numeric depending on preprocessing).}
#'   \item{Experiment File Name}{Source experiment/file name (character).}
#'   \item{condition_ID}{Condition identifier used throughout TSAR (character).}
#'   \item{Well.Position}{Well position label (character).}
#'   \item{Temperature}{Temperature in degrees Celsius (numeric).}
#'   \item{Fluorescence}{Raw fluorescence (numeric).}
#'   \item{RFU}{Normalized fluorescence / relative fluorescence units (numeric).}
#' }
#'
#' @source Included with the package for demonstration and testing.
#'
#' @examples
#' data("example_normalized_data")
#' head(example_normalized_data)
#'
#' @docType data
#' @name example_normalized_data
NULL
