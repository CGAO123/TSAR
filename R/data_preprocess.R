#' Normalize Fluorescence
#'
#' normalize() reads in raw_data. This function normalizes data by standardizing
#'   them according to maximum and minimum fluorescence per well, with maximum
#'   set to 1 and minimum set to 0. It also reformats data types by checking
#'   for potential error. i.e. a string speicfying 100,000 will be read in
#'   as number, 100000, without issue.
#'   Function is applicable only to data of a single well, do not call on
#'   an entire data frame of all 96 well data. It is intended for single well
#'   screening purposes.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select
#'
#' @param raw_data data frame; raw dataset input, should be of only one well.
#'   If multiple wells need to be normalized, use \code{\link{gam_analysis}}()
#'   for 96 well application. If only preliminary screening is needed, use
#'   \code{\link[TSAR]{screen}}().
#' @param fluo integer; the Fluorescence variable column id
#' (e.g. fluo = 5 when 5th column of the data frame is the Fluorescence value)
#'   if fluorescence variable is named exactly as "Fluorescence", fluo does not
#'   need to be specified. i.e. fluo is set to NA by default,
#'   suggesting the variable is named "Fluorescence".
#' @param selected list of character strings;
#'   variables from the original data set users intend to keep.
#'   Variable default set to c("Well.Position", "Temperature", "Fluorescence",
#'   "Normalized") if not otherwise specified. If data frame variables are named
#'   differently, user needs to specify what column variables to keep.
#' @return cleaned up data framed with selected columns
#' @examples
#' data("qPCR_data1")
#' test <- subset(qPCR_data1, Well.Position == "A01")
#' normalize(test)
#'
#' @family data_preprocess
#' @export
normalize <- function(
    raw_data,
    fluo = NA,
    selected = c(
        "Well.Position",
        "Temperature",
        "Fluorescence",
        "Normalized"
    )) {
    norm_data <- raw_data %>%
        # make sure fluorescence and temperature variable is in double type
        # step to ensure that calculations do not return error
        transform(
            Fluorescence =
                as.double(gsub(",", "", raw_data$Fluorescence))
        ) %>%
        transform(
            Temperature =
                as.double(gsub(",", "", raw_data$Temperature))
        ) %>%
        transform(Well.Position = paste(
            substr(raw_data$Well.Position, 1, 1),
            sprintf("%02s", substr(
                raw_data$Well.Position, 2,
                nchar(raw_data$Well.Position)
            )),
            sep = ""
        ))

    if (is.na(fluo)) {
        # assign min and max
        min_f <- min(norm_data$Fluorescence)
        max_f <- max(norm_data$Fluorescence)
        norm_data <- norm_data %>%
            # normalize by max and min
            mutate(
                Normalized =
                    (norm_data$Fluorescence - min_f) / (max_f - min_f)
            ) %>%
            # select only desired variables (e.g. drop raw derivative values)
            dplyr::select(selected)
    } else {
        norm_data <- norm_data %>%
            # normalize by max and min
            mutate(
                Normalized =
                    (norm_data[[fluo]] - min(norm_data[[fluo]])) /
                        (max(norm_data[[fluo]]) - min(norm_data[[fluo]]))
            ) %>%
            # select only desired variables (e.g. drop raw derivative values)
            dplyr::select(selected)
    }
}

#' Generalized Addidtive Modeling on TSA data
#'
#' Function finds fitted fluorescence values by imposing generalized
#'   additive model on fluorescence data by temperature. Model assumes
#'   method = "GACV.Cp" and sets to \code{formula = y ~ s(x, bs = "ad")}.
#'   Function inherits function from gam package, \code{\link{gam}}().
#'
#'
#' @importFrom magrittr %>%
#' @importFrom mgcv gam
#' @importFrom dplyr mutate select
#'
#' @param norm_data data frame input, preferably normalized using
#'   \code{\link{normalize}}.
#' @param x temperature column
#' @param y normalized fluorescence column
#' @return dtaa frame containing gam model fitted values
#'
#' @family data_preprocess
#' @examples
#' data("qPCR_data1")
#' test <- subset(qPCR_data1, Well.Position == "A01")
#' test <- normalize(test, fluo = 5, selected = c(
#'     "Well.Position", "Temperature",
#'     "Fluorescence", "Normalized"
#' ))
#' model_gam(test, x = test$Temperature, y = test$Normalized)
#'
#' @export
model_gam <- function(norm_data, x, y) {
    mgcv::gam(
        formula = y ~ s(x, bs = "ad"),
        data = norm_data,
        method = "GACV.Cp"
    )
}


#' Refit and calculate derivative function
#'
#' Model_fit calculates derivatives by refitting model onto data. Only runs
#'   on data of a single well.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select
#'
#' @param norm_data data frame; the raw data set input
#' @param model fitted model containing fitted values
#' @param smoothed inform whether data already contains a smoothed model; Input
#'   the column name of the smoothed data to override values of gam model
#'   fitting. For example, existing "Fluorescence" column contains data already
#'   smoothed, set \code{smoothed = "Flourescence"} to calculate derivative
#'   function upon the called smoothed data.
#' @return data frame; with calculated derivative columns
#'
#' @family data_preprocess
#'
#' @examples
#' data("qPCR_data1")
#' test <- subset(qPCR_data1, Well.Position == "A01")
#' test <- normalize(test, fluo = 5, selected = c(
#'     "Well.Position", "Temperature",
#'     "Fluorescence", "Normalized"
#' ))
#' gammodel <- model_gam(test, x = test$Temperature, y = test$Normalized)
#' model_fit(test, model = gammodel)
#' # if data come smoothed, run ...
#' model_fit(test, smoothed = "Fluorescence")
#'
#' @export
#'
model_fit <- function(norm_data, model, smoothed) {
    if (missing(smoothed)) {
        norm_data <- norm_data %>%
            # pull in the fitted values into data frame
            mutate(fitted = model$fitted.values)
        # calculate derivative using fitted values
        norm_data <- mutate(norm_data,
            norm_deriv =
                c(diff(norm_data$fitted) /
                    diff(norm_data$Temperature), NA)
        )
    } else {
        norm_data <- mutate(norm_data,
            norm_deriv =
                c(diff(norm_data[, smoothed]) /
                    diff(norm_data$Temperature), NA)
        )
    }
}
