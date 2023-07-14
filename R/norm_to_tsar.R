#' Merge and format data for sequential applications of graphing functions
#'
#' This function merges data of experiment replicates across different dates.
#'   It merges and produces information variables used to group wells of same
#'   set up.
#'
#' @importFrom dplyr rename mutate
#'
#' @param data A character vector specifying the file paths of the data files
#'   or data frame objects of analysis data set.
#' @param name A character vector specifying the experiment names.
#' @param date A character vector specifying the dates.
#'
#' @details This function merges and normalizes test data from multiple files.
#'   The lengths of the \code{data}, \code{name}, and \code{date} vectors
#'   must match, otherwise an error is thrown.
#'
#' @examples
#' data("qPCR_data1")
#' result <- gam_analysis(qPCR_data1, smoothed = TRUE, fluo = 5)
#' data("Well_Information")
#' norm_data <- join_well_info( file_path = NULL, file = Well_Information,
#'   read_tsar(result, code = 2), type = "by_template")
#' norm_data <- na.omit(norm_data)
#' data("qPCR_data2")
#' result2 <- gam_analysis(qPCR_data1, smoothed = TRUE, fluo = 5)
#' norm_data2 <- join_well_info( file_path = NULL, file = Well_Information,
#'   read_tsar(result2, code = 2), type = "by_template")
#' norm_data2 <- na.omit(norm_data2)
#' tsar_data <- merge_norm(
#'   data = list(norm_data, norm_data2),
#'   name = c( "Thermal Shift_162.eds.csv", "Thermal Shift_168.eds.csv"),
#'   date = c("20230203", "20230209"))
#'
#'
#' @return data frame in the format of tsat_data
#'
#' @family TSAR Formatting
#'
#' @export
#'

merge_norm <- function(data,
                       name,
                       date) {
    if (length(data) != length(name) || length(date) != length(name)) {
        stop("Data, name, and date counts do not match.")
    }
    if (length(data) == 0) {
        stop("No data input, please check parameter input.")
    }

    tsar_data <- c()
    dataset <- c()


    for (i in 1:length(data)) {
        if (is.data.frame(data[[i]]) == FALSE) {
            dataset[[i]] <- data.frame(read.csv(
                file = toString(data[i]),
                header = TRUE
            ))
        } else {
            dataset[[i]] <- data.frame(data[[i]])
        }
    }


    for (i in 1:length(dataset)) {
        cur <- dataset[[i]]
        cur <- mutate(cur, ExperimentFileName = name[i])
        tsar_cur <- cur %>%
            mutate(well_ID = paste(cur$Well.Position, cur$Protein, cur$Ligand,
                date[i],
                sep = "_"
            ))
        tsar_data <- rbind(tsar_data, tsar_cur)
    }

    tsar_data <- data.frame(tsar_data)
    tsar_data <- tsar_data %>%
        mutate(condition_ID = paste(tsar_data$Protein,
            tsar_data$Ligand,
            sep = "_"
        )) %>%
        dplyr::rename(Tm = tm, Well = Well.Position)

    return(tsar_data)
}
