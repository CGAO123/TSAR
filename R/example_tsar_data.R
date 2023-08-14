#' Example tsar_data file
#'
#' Dataset Description:
#'     This is an example dataset of the tsar_data strucutre. The data
#'     frame contains well ID, conditions, and experimental details.
#'
#' @name example_tsar_data
#'
#' @format A data frame with the following columns:
#'     \describe{
#'       \item{Well}{Well position}
#'       \item{Temperature}{Temperature in degrees}
#'       \item{Fluorescence}{Fluorescence reading}
#'       \item{Normalized}{Normalized value}
#'       \item{norm_deriv}{Calculated first derivative}
#'       \item{Tm}{Tm value}
#'       \item{Protein}{Protein information}
#'       \item{Ligand}{Ligand information}
#'       \item{ExperimentFileName}{Experiment file name}
#'       \item{well_ID}{Well ID}
#'       \item{condition_ID}{Condition ID}
#'     }
#'
#' @usage data(example_tsar_data)
#' @return example tsar_data in data frame
#' @keywords dataset
#' @source experimentally obtained
#' @keywords dataset
data(example_tsar_data, envir = environment())
