#' qPCR_data2 Dataset
#'
#' Dataset Description:
#'   This dataset contains qPCR data for the CA121 protein and common vitamins.
#'   It provides binding fluorescence measurements obtained using thermoshift.
#'   A different experiemnt trial containing data of similar property as
#'   qPCR_data1.
#'
#' @name qPCR_data2
#'
#' @format A data frame with the following columns:
#'   \describe{
#'     \item{Well}{Well Count, not required for user}
#'     \item{Well.Position}{Well Label, i.e. A01; required input}
#'     \item{Reading}{reading count in time series, not required for user}
#'     \item{Temperature}{temperature reading, required input}
#'     \item{Fluorescence}{fluorescence reading, required input}
#'   }
#'
#' @usage data(qPCR_data2)
#' @return qPCR_data2 data frame
#' @keywords dataset
data(qPCR_data2, envir = environment())