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
#' @return integer; tm estimation
#'
#' #examples
#' #well <- fitted %>%
#' #   filter(Well.Position == "A1")
#' #tm_est(A1)
#' #tm_est(A1, min = 30, max= 35)
#' @export
#'
Tm_est <- function(norm_data, min, max){
    # if min and max are not specified, default to check across the graph
    if(missing(min) && missing(max)){
        return(norm_data$Temperature[which.max(norm_data$norm_deriv)])
        # allow input to restrict the domain of search for inflection points
    }else{
        x <- norm_data[norm_data$Temperature >= min &
                      norm_data$Temperature <= max, ]
        return(x$Temperature[which.max(x$norm_deriv)])
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
#' @return list of data frames, list of three data frame outputs,
#'   tm estimation by well, data set, fit of model by well
#'
#' #examples
#' #raw_data <- read.delim(header = TRUE, skip = 0, nrow = 112976,
#' "/Users/candygao/Desktop/qpcrresult/CA_IP_HCB_2_20220110_134917_RawData_
#' Thermal Shift_02_55.eds.txt")
#' #Result_R <- gam_analysis(rawdata = raw_data, keep = T, fit = T, smoothed = F,
#'                  selections = c("Well.Position", "Temperature",
#'                  "Fluorescence", "Normalized"))
#'
#' @export
gam_analysis <- function(
    raw_data,
    keep = TRUE,
    fit = FALSE,
    smoothed = FALSE,
    selections = c("Well.Position", "Temperature",
                   "Fluorescence", "Normalized")){
    #Initialize variables
    tm <- c()
    kept <- data.frame()
    fitsummaries <- list()
    #iterate through each individual well
    for(i in unique(raw_data$Well.Position)){
        by_well <- raw_data %>%
            filter(Well.Position == i)
        #normalize data
        by_well <- normalize(raw_data = by_well,
                             Fluo = 5,
                             selected = selections)

        #check is data is already smooth
        #fit model is not smoothed
        if(smoothed == FALSE) {
            gammodel <- model_gam(norm_data = by_well,
                                  x = by_well$Temperature,
                                  y = by_well$Normalized)
            #check if user wishes to keep summaries of each well's model fit
            if(fit == TRUE) {
                newsum <- list(summary(gammodel))
                #concat fit summaries
                fitsummaries <- append(fitsummaries, newsum)
            }
            #calculate derivatives and append derivated using fitted values
            by_well <- model_fit(norm_data = by_well, model = gammodel)
        } else {
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
    if(keep == TRUE) { tobereturned <- append(tobereturned, list(kept)) }
    if(fit == TRUE) { tobereturned <- append(tobereturned, list(fitsummaries)) }
    #return list of data frames
    return(tobereturned)

}