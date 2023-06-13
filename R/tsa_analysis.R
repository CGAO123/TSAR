#' find inflection point function
#'
#' looks for Tm temperature values by finding the inflection point in the
#'   normalized fluorescence data. The inflection point is approximated by
#'   locating the maximum first derivative
#'
#' @param norm_data data frame; data frame input containing derivative values
#'   can only be data frames for one well; finding inflections points across
#'   multiple wells require iteration through individual wells
#' @param min restricts finding to be above the given minimum temperature
#' @param max restricts finding to be below the given maximum temperature
#'   parameter min and max can be used to remove messy or undesired data
#'   for better accuracy in tm estimation; removing data is  before fitting the
#'   model is more recommended than removing here
<<<<<<< HEAD
#' @return integer; tm estimation
#'
=======
#'
#' @return integer; tm estimation
#'
#' @family tsa_analysis
#'
>>>>>>> main
#' #examples
#' #well <- fitted %>%
#' #   filter(Well.Position == "A1")
#' #tm_est(A1)
#' #tm_est(A1, min = 30, max= 35)
#' @export
#'
<<<<<<< HEAD
Tm_est <- function(norm_data, min, max){
    # if min and max are not specified, default to check across the graph
    if(missing(min) && missing(max)){
        return(norm_data$Temperature[which.max(norm_data$norm_deriv)])
        # allow input to restrict the domain of search for inflection points
    }else{
        x <- norm_data[norm_data$Temperature >= min &
                      norm_data$Temperature <= max, ]
        return(x$Temperature[which.max(x$norm_deriv)])
=======
tm_est <- function(norm_data, min, max) {
    # if min and max are not specified, default to check across the graph
    if (missing(min) && missing(max)) {
        return(norm_data$Temperature[which.max(norm_data$norm_deriv)])
    }else {# allow input to restrict the domain of search for inflection points
        x <- norm_data[norm_data$Temperature >= min &
                      norm_data$Temperature <= max, ]
        return(x$Temperature[which.max(norm_data$norm_deriv)])
>>>>>>> main
    }

}


#' gam_analysis function run full board
#'
#' function pipeline that combines separated functions and iterate through
#'   each well to find the tm estimation
#' @importFrom mgcv gam
#' @importFrom magrittr %>%
#'
#' @param raw_data data frame; raw data frame
#' @param keep Boolean; set to T by default to return normalized data and
#'   fitted data
#' @param smoothed Boolean; set to false by default,
#'   if data is already smoothed, set smoothed to true
#' @param fit Boolean; set to F by default, T returns access to information
#'   of each model fit
#' @param selections list of characters; the variables in raw data user intends
#'   to keep. It is set, by default, to c("Well.Position", "Temperature",
#'   "Fluorescence", "Normalized").
<<<<<<< HEAD
#' @return list of data frames, list of three data frame outputs,
#'   tm estimation by well, data set, fit of model by well
#'
#' #examples
#' #raw_data <- read.delim(header = TRUE, skip = 0, nrow = 112976,
#' "/Users/candygao/Desktop/qpcrresult/CA_IP_HCB_2_20220110_134917_RawData_
#' Thermal Shift_02_55.eds.txt")
#' #Result_R <- gam_analysis(rawdata = raw_data, keep = T, fit = T, smoothed = F,
#'                  selections = c("Well.Position", "Temperature",
=======
#' @param fluo integer; the Fluorescence variable column id
#' (e.g. fluo = 5 when 5th column of the data frame is the Fluorescence value)
#'   if fluorescence variable is named exactly as "Fluorescence", fluo does not
#'   need to be specified. i.e. fluo is set to -1 by default,
#'   suggesting the variable is named "Fluorescence"
#'
#' @return list of data frames, list of three data frame outputs,
#'   tm estimation by well, data set, fit of model by well
#'
#' @family tsa_analysis
#' #examples
#' #raw_data <- utils::read.delim(header = TRUE, skip = 0, nrow = 112976,
#' "/Users/candygao/Desktop/qpcrresult/CA_IP_HCB_2_20220110_134917_RawData_
#' Thermal Shift_02_55.eds.txt")
#' #Result_R <- gam_analysis(rawdata = raw_data, keep = T, fit = T,
#'               smoothed = F, selections = c("Well.Position", "Temperature",
>>>>>>> main
#'                  "Fluorescence", "Normalized"))
#'
#' @export
gam_analysis <- function(
    raw_data,
    keep = TRUE,
    fit = FALSE,
    smoothed = FALSE,
<<<<<<< HEAD
    selections = c("Well.Position", "Temperature",
                   "Fluorescence", "Normalized")){
=======
    fluo = -1,
    selections = c("Well.Position",
                   "Temperature",
                   "Fluorescence",
                   "Normalized")
    ) {
>>>>>>> main
    #Initialize variables
    tm <- c()
    kept <- data.frame()
    fitsummaries <- list()
    #iterate through each individual well
<<<<<<< HEAD
    for(i in unique(raw_data$Well.Position)){
        by_well <- raw_data %>%
            filter(Well.Position == i)
        #normalize data
        by_well <- normalize(raw_data = by_well,
                             Fluo = 5,
=======
    for (i in unique(raw_data$Well.Position)) {
        by_well <- raw_data %>%
            filter(raw_data$Well.Position == i)
        #normalize data
        by_well <- normalize(raw_data = by_well,
                             fluo = fluo,
>>>>>>> main
                             selected = selections)

        #check is data is already smooth
        #fit model is not smoothed
<<<<<<< HEAD
        if(smoothed == FALSE) {
=======
        if (smoothed == FALSE) {
>>>>>>> main
            gammodel <- model_gam(norm_data = by_well,
                                  x = by_well$Temperature,
                                  y = by_well$Normalized)
            #check if user wishes to keep summaries of each well's model fit
<<<<<<< HEAD
            if(fit == TRUE) {
=======
            if (fit == TRUE) {
>>>>>>> main
                newsum <- list(summary(gammodel))
                #concat fit summaries
                fitsummaries <- append(fitsummaries, newsum)
            }
            #calculate derivatives and append derivated using fitted values
            by_well <- model_fit(norm_data = by_well, model = gammodel)
        } else {
<<<<<<< HEAD
            #if data smoothing is already present, not further modeling is required
            #calculate derivatives using present smoothed data and append derivatives
            by_well <- by_well %>%
                mutate(norm_deriv = c(diff(Normalized)/diff(Temperature), NA))
        }

        #estimate tm values and concat them into one list
        tm <- c(tm, Tm_est(by_well))

        #if user wishes to keep all fitted data
        if(keep==TRUE) {
            if(smoothed ==FALSE) {
                #keep selected variables, fitted values, and derivatives
                by_well <- by_well%>%
                    dplyr:: select(selections, fitted)
            } else {
                #keep selected variables and derivatives
                by_well <- by_well%>%
=======
            #if data smoothing is already present, not further modeling required
            #calculate derivatives using smoothed data, append derivatives
            by_well <- by_well %>%
                mutate(norm_deriv = c(diff(by_well$Normalized)
                                      / diff(by_well$Temperature), NA))
        }

        #estimate tm values and concat them into one list
        tm <- c(tm, tm_est(by_well))

        #if user wishes to keep all fitted data
        if (keep == TRUE) {
            if (smoothed == FALSE) {
                #keep selected variables, fitted values, and derivatives
                by_well <- by_well %>%
                    dplyr:: select(selections, fitted)
            } else {
                #keep selected variables and derivatives
                by_well <- by_well %>%
>>>>>>> main
                    dplyr:: select(selections)
            }
            #concat data frame of each well into one big data frame
            kept <- rbind(kept, by_well)
        }
    }

    #typify all return values into data frames
    tm <- data.frame(tm)
    kept <- data.frame(kept)
    #prepare intial list of output
    tobereturned <- list(tm)

    #concat by users' specifications
<<<<<<< HEAD
    if(keep == TRUE) { tobereturned <- append(tobereturned, list(kept)) }
    if(fit == TRUE) { tobereturned <- append(tobereturned, list(fitsummaries)) }
=======
    if (keep == TRUE) {
        tobereturned <- append(tobereturned, list(kept))
    }

    if (fit == TRUE) {
        tobereturned <- append(tobereturned, list(fitsummaries))
    }

>>>>>>> main
    #return list of data frames
    return(tobereturned)

}
