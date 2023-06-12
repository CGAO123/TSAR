#' normalize function
#'
#' Normalize reads in raw data. This function normalizes data by standardized
#'   them according to max and min. It also reformat data types by checking
#'   for potential error
#'
#' @importFrom magrittr %>%
#'
#' @param raw_data data frame; the raw data set input
#' @param fluo integer; the Fluorescence variable column id
#' (e.g. fluo = 5 when 5th column of the data frame is the Fluorescence value)
#'   if fluorescence variable is named exactly as "Fluorescence", fluo does not
#'   need to be specified. i.e. fluo is set to -1 by default,
#'   suggesting the variable is named "Fluorescence"
#' @param selected list of character strings;
#'   variables from the original data set users intend to keep.
#'   Variable default set to c("Well.Position", "Temperature", "Fluorescence",
#'   "Normalized") if not otherwise specified. If data frame variables are named
#'   differently, user needs to specify what column variables to keep
#' @return cleaned up data framed with selected columns
#' #examples
#' #normalized <-normalize(data = raw_data, fluo = 5, selected =
#' #c("Well.Position", "Temperature", "Fluorescence", "Normalized"))
#'
#' @family data_preprocess
#' @export
normalize <- function(
    raw_data,
    fluo = -1,
    selected = c("Well.Position",
                 "Temperature",
                 "Fluorescence",
                 "Normalized")) {

  norm_data <- raw_data %>%
    #make sure fluoresence and temperature variable is in double type
    #step to ensure that calculations do not return error
    transform(Fluorescence = as.double(gsub(",", "", Fluorescence))) %>%
    transform(Temperature = as.double(gsub(",", "", Temperature))) %>%
    transform(Well.Position = paste(
        substr(Well.Position, 1, 1),
        sprintf("%02s", substr(Well.Position, 2,
                               nchar(Well.Position))), sep = ""))

  if (fluo == -1) {
    #assign min and max
    min_f <- min(norm_data$Fluorescence)
    max_f <- max(norm_data$Fluorescence)
    norm_data <- norm_data %>%
      #normalize by max and min
      mutate(Normalized = (Fluorescence - min_f) / (max_f - min_f)) %>%
      #select only desired variables (e.g. drop raw derivative values)
      dplyr:: select(selected)
  } else {
    norm_data <- norm_data %>%
      #normalize by max and min
      mutate(Normalized =
                 (.[[fluo]] - min(.[[fluo]])) /
                    (max(.[[fluo]]) - min(.[[fluo]]))) %>%
      #select only desired variables (e.g. drop raw derivative values)
      dplyr:: select(selected)
  }
}

#' modeling function
#'
#' function finds fitted fluorescence values
#'
#'
#' @importFrom magrittr %>%
#' @importFrom mgcv gam
#'
#' @param norm_data data frame input
#' @param x temperature column
#' @param y normalized fluorescence column
#' @return gam model fitted with formula set to y ~ s(x, bs = "cs")
#'
#' @family data_preprocess
#' #example
#' #gammodel <- model_gam(Data = normalized,
#'                          x = normalized$temperature,
#'                          y = normalized$fluorescence)
#'
#' @export
model_gam <- function(norm_data, x, y) {
    mgcv::gam(formula = y ~ s(x, bs = "cs"),
              data = norm_data,
              method = "GACV.Cp")
}


#' Refit and calculate derivative function
#'
#' Model_fit calculates derivatives by refitting model onto data
#'
#' @importFrom magrittr %>%
#'
#' @param norm_data data frame; the raw data set input
#' @param model fitted model containing fitted values
#' @param smoothed inform whether data already contains smoothed model; set
#'   \code{smoothed = "Flourescence"} to calculate norm_deriv upon the
#'   existing model
#' @return data frame; with calculated derivative columns
#'
#' @family data_preprocess
#' #examples
#' #fitted <- modelfit(data = gammodel, model = fitted)
#'
#' @export
#'
model_fit <- function(norm_data, model, smoothed) {
    if (missing(smoothed)) {
        norm_data <- norm_data %>%
        #pull in the fitted values into data frame
        mutate(fitted = model$fitted.values)
        #calculate derivative using fitted values
        norm_data <- mutate(norm_data,
                            norm_deriv =
                                c(diff(norm_data$fitted) /
                                diff(norm_data$Temperature), NA))
    }else {
        norm_data <- mutate(norm_data,
                            norm_deriv =
                                c(diff(norm_data[, smoothed]) /
                                diff(norm_data$Temperature), NA))
    }
}
