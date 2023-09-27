#' Merge and format norm_data into tsar_data
#'
#' This function merges data of experiment replicates across different dates.
#'     It merges and produces information variables used to group wells of same
#'     set up.
#'
#' @importFrom dplyr rename mutate bind_rows
#' @importFrom magrittr %>%
#'
#' @param data list, a character vector specifying the file paths of the data
#'     files or data frame objects of analysis data set. For example, given data
#'     frames named "data1" and "data2", specify parameter as
#'     \code{data = list(data1, data2)}.
#' @param name list, character vector specifying the experiment names.
#' @param date list, character vector specifying the dates. Does not require
#'     any date format restrictions.
#'
#' @details This function merges and normalizes test data from multiple files.
#'     The lengths of the \code{data}, \code{name}, and \code{date} vectors
#'     must match, otherwise an error is thrown.
#'
#' @examples
#' data("qPCR_data1")
#' result <- gam_analysis(qPCR_data1, smoothed = TRUE, fluo = 5)
#' data("well_information")
#' norm_data <- join_well_info(
#'     file_path = NULL, file = well_information,
#'     read_tsar(result, output_content = 2), type = "by_template"
#' )
#' norm_data <- na.omit(norm_data)
#' data("qPCR_data2")
#' result2 <- gam_analysis(qPCR_data1, smoothed = TRUE, fluo = 5)
#' norm_data2 <- join_well_info(
#'     file_path = NULL, file = well_information,
#'     read_tsar(result2, output_content = 2), type = "by_template"
#' )
#' norm_data2 <- na.omit(norm_data2)
#' tsar_data <- merge_norm(
#'     data = list(norm_data, norm_data2),
#'     name = c("Thermal Shift_162.eds.csv", "Thermal Shift_168.eds.csv"),
#'     date = c("20230203", "20230209")
#' )
#'
#' @return data frame in the format of tsar_data
#'
#' @family TSAR Formatting
#'
#' @export
#'
merge_norm <- function(data,
                       name,
                       date) {
    #check for matching lengths of parameters
    if (length(data) != length(name) || length(date) != length(name)) {
        stop("Data, name, and date counts do not match.")
    } else if (length(data) == 0) {
        stop("No data input, please check parameter input.")
    }

    tsar_data <- c()
    dataset <- c()

    #pivot data input
    dataset <- lapply(data, function(x) {
        if (!is.data.frame(x)) {
            return(as.data.frame(read.csv(file = toString(x), header = TRUE)))
        } else {
            return(as.data.frame(x))
        }
    })

    #create well ID and combine datasets
    tsar_data <- lapply(seq_along(dataset), function(i) {
        cur <- dataset[[i]]
        cur <- cur %>%
            mutate(ExperimentFileName = name[i])
        cur <- cur %>%
            mutate(well_ID = paste(Well.Position, Protein, Ligand,
                            date[i], sep = "_"))
        return(cur)
    }) %>% bind_rows()

    #create condition ID
    tsar_data <- data.frame(tsar_data)
    tsar_data <- tsar_data %>%
        mutate(condition_ID = paste(tsar_data$Protein,
            tsar_data$Ligand,
            sep = "_"
        )) %>%
        dplyr::rename(Tm = tm, Well = Well.Position)

    return(tsar_data)
}
