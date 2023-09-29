#' Find inflection point function
#'
#' Looks for Tm temperature values by finding the inflection point in the
#'     fluorescence data. The inflection point is approximated by
#'     locating the maximum first derivative stored in "norm_deriv" column.
#'
#' @param norm_data data frame; data frame input containing derivative values
#'     can only be data frames for one well; finding inflections points across
#'     multiple wells require iteration through individual wells
#' @param min restricts finding to be above the given minimum temperature
#' @param max restricts finding to be below the given maximum temperature
#'     parameter min and max can be used to remove messy or undesired data
#'     for better accuracy in tm estimation; removing data is  before fitting
#'     the model is more recommended than removing here
#'
#' @return integer; tm estimation
#'
#' @family tsa_analysis
#'
#' @examples
#' data("qPCR_data1")
#' test <- subset(qPCR_data1, Well.Position == "A01")
#' test <- normalize(test, fluo = 5, selected = c(
#'     "Well.Position", "Temperature",
#'     "Fluorescence", "Normalized"
#' ))
#' gammodel <- model_gam(test, x = test$Temperature, y = test$Normalized)
#' fit <- model_fit(test, model = gammodel)
#' Tm_est(fit)
#'
#' @export
#'
Tm_est <- function(norm_data, min, max) {
    # if min and max are not specified, default to check across the graph
    if (missing(min) && missing(max)) {
        return(norm_data$Temperature[which.max(norm_data$norm_deriv)])
    } else { # allow input to restrict domain of search for inflection points
        x <- norm_data[norm_data$Temperature >= min &
            norm_data$Temperature <= max, ]
        return(x$Temperature[which.max(norm_data$norm_deriv)])
    }
}


#' Analysis of all 96 wells through gam modeling
#'
#' Function pipeline that combines separated functions and iterate through
#'     each well to estimate the Tm.
#' @importFrom mgcv gam
#' @importFrom dplyr select mutate
#'
#' @param raw_data data frame; raw data frame
#' @param keep Boolean; set to \code{keep = TRUE}
#'     by default to return normalized data and fitted data
#' @param smoothed Boolean; set to \code{smoothed = FALSE} by default,
#'     if data is already smoothed, set smoothed to true
#' @param boltzmann Boolean; set to \code{boltzmann = FALSE} by default. Set
#'     to \code{boltzmann = TRUE} if a botlzmann fit is preferred.
#' @param fit Boolean; set to \code{fit = FALSE} by default, \code{fit = TRUE}
#'     returns access to information of each model fit. Not accessible in shiny.
#' @param selections list of characters; the variables in raw data user intends
#'     to keep. It is set, by default, to \code{c("Well.Position",
#'     "Temperature", "Fluorescence", "Normalized")}.
#' @param fluo_col integer; the Fluorescence variable column id
#'     (e.g. fluo = 5 when 5th column of data frame is the Fluorescence value)
#'     if fluorescence variable is named exactly as "Fluorescence", fluo does
#'     not need to be specified.
#'
#' @return List of data frames, list of three data frame outputs,
#'     Tm estimation by well, data set, fit of model by well.
#'
#' @family tsa_analysis
#'
#' @examples
#' data("qPCR_data1")
#' gam_analysis(qPCR_data1,
#'     smoothed = TRUE, boltzmann = FALSE, fluo_col = 5,
#'     selections = c(
#'         "Well.Position", "Temperature", "Fluorescence",
#'         "Normalized"
#'     )
#' )
#' model <- gam_analysis(qPCR_data1, smoothed = FALSE, boltzmann = TRUE)
#'
#' @export
gam_analysis <- function(
        raw_data,
        keep = TRUE,
        fit = FALSE,
        smoothed = FALSE,
        boltzmann = FALSE,
        fluo_col = NA,
        selections = c(
            "Well.Position",
            "Temperature",
            "Fluorescence",
            "Normalized"
        )) {

    # Function to process each well
    process_well <- function(i) {
        by_well <- subset(raw_data, Well.Position == i)
        by_well <- normalize(
            raw_data = by_well,
            fluo = fluo_col,
            selected = selections
        )

        # Initialize variables for each well
        tm <- c()
        kept <- data.frame()
        fitsummaries <- list()

        # fit model if not smoothed
        if (!smoothed) {
            if (boltzmann) {
                model <- run_boltzmann(norm_data = by_well)
            } else {
                model <- model_gam(
                    norm_data = by_well,
                    x = by_well$Temperature,
                    y = by_well$Normalized
                )
            }

            # check if user wishes to keep summaries of each well's model fit
            if (fit) {
                newsum <- list(summary(model))
                fitsummaries <- append(fitsummaries, newsum)
            }

            # calculate derivatives and append derivated using fitted values
            by_well <- model_fit(norm_data = by_well, model = model)
        } else {
            # if data smoothing is already present, not modeling required
            # calculate derivatives using smoothed data, append derivatives
            by_well <- mutate(by_well,
                        norm_deriv = c(diff(by_well$Normalized) /
                                        diff(by_well$Temperature), NA))
        }

        # estimate tm values and concat them into one list
        tm <- c(tm, Tm_est(by_well))

        # if user wishes to keep all fitted data
        if (keep) {
            if (!smoothed) {
                # keep selected variables, fitted values, and derivatives
                by_well <- dplyr::select(
                    by_well,
                    all_of(c(unlist(selections), "fitted", "norm_deriv"))
                )
            } else {
                # keep selected variables and derivatives
                by_well <- dplyr::select(
                    by_well,
                    all_of(c(unlist(selections), "norm_deriv"))
                )
            }
            # concat data frame of each well into one big data frame
            kept <- rbind(kept, by_well)
        }

        return(list(tm = tm, kept = kept, fitsummaries = fitsummaries))
    }

    # Apply process_well to each unique well
    unique_well_positions <- unique(raw_data$Well.Position)
    result_list <- lapply(unique_well_positions, process_well)

    # Combine results
    tm_list <- lapply(result_list, function(result) result$tm)
    kept_list <- lapply(result_list, function(result) result$kept)
    fitsummaries <- unlist(lapply(result_list,
                                    function(result) result$fitsummaries),
                                    recursive = FALSE)

    # Combine the lists into single data frames
    tm <- data.frame(do.call(rbind, tm_list))
    colnames(tm) <- "tm"
    kept <- data.frame(do.call(rbind, kept_list))

    # Prepare output list
    tobereturned <- list(tm)

    if (keep) {
        tobereturned <- append(tobereturned, list(kept))
    }

    if (fit) {
        tobereturned <- append(tobereturned, list(fitsummaries))
    }

    return(tobereturned)
}
