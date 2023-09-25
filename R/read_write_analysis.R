#' Read analysis result
#'
#' reads previous pipeline output lists from \code{\link{gam_analysis}}()
#'     and organizes them into separate data frames.
#'
#' @param gam_result list; input uses resulting output of
#'     \code{\link{gam_analysis}}() function
#' @param output_content integer;
#'     \code{output_content = 0} returns only the tm value by wells
#'     \code{output_content = 1} returns data table with fitted values
#'     \code{output_content = 2} returns the combination of 0 and 1
#' @return output files with select dataset
#'
#' @family read_write_analysis
#'
#' @examples
#' data("qPCR_data1")
#' result <- gam_analysis(qPCR_data1,
#'     smoothed = TRUE, fluo_col = 5,
#'     selections = c(
#'         "Well.Position", "Temperature", "Fluorescence", "Normalized"
#'     )
#' )
#' read_tsar(result, output_content = 0)
#' output_data <- read_tsar(result, output_content = 2)
#'
#' @export
read_tsar <- function(gam_result, output_content) {
    # returns only the tm value by wells
    wellpos <- gam_result[[2]]$Well.Position
    wellpos <- paste(substr(wellpos, 1, 1),
        sprintf("%02s", substr(wellpos, 2, nchar(wellpos))),
        sep = ""
    )
    if (output_content == 0) {
        tmval <- gam_result[[1]]
        tmval <- data.frame(
            Well.Position =
                unique(wellpos[grepl("[A-Z]+[0-9]+[0-9]$", wellpos)]),
            TM = tmval
        )
        return(tmval)
    } else if (output_content == 1) {
        # returns the data table with the fitted values
        dat <- gam_result[[2]]
        dat <- data.frame(dat)
        return(dat)
    } else if (output_content == 2) {
        # returns the all data, combination of 0 and 1
        tmval <- gam_result[[1]]
        dat <- gam_result[[2]]
        dat <- data.frame(dat)
        tmval <- data.frame(
            Well.Position =
                unique(wellpos[grepl(
                    "[A-Z]+[0-9]+[0-9]$",
                    wellpos
                )]), TM = tmval
        )
        # left join seperate data frames into one
        all <- left_join(dat, tmval, by = "Well.Position")
        return(all)
    }
}


#' write output files
#'
#' writes output into csv or txt files
#'
#' @importFrom utils write.csv write.table
#'
#' @param data input data frame
#' @param name string, name file to be saved as. Final name will be appended
#'     "tsar_output"
#' @param file \code{file = "txt"} writes txt output files;
#'     \code{file = "csv"} writes csv output files;
#'     default set to \code{file = "txt"}
#' @return file output on the working directory where data was read in
#'
#' @family read_write_analysis
#' @examples
#' data("qPCR_data1")
#' result <- gam_analysis(qPCR_data1,
#'     smoothed = TRUE, fluo_col = 5,
#'     selections = c(
#'         "Well.Position", "Temperature", "Fluorescence", "Normalized"
#'     )
#' )
#' output_data <- read_tsar(result, output_content = 2)
#' # example does not run, will build excessive file in package
#' # write_tsar(output_data, name = "2022_03_18_test", file = "txt")
#'
#' @export
#'
write_tsar <- function(data, name, file = "txt") {
    file <- match.arg(file, choices = c("txt", "csv"))
    # write csv
    if (file == "csv") {
        rename <- paste(name, "tsar_output.csv", sep = "_")
        utils::write.csv(data,
            file = rename,
            row.names = FALSE,
            quote = FALSE
        )
    #write txt file
    } else if (file == "txt") {
        rename <- paste(name, "tsar_output.txt", sep = "_")
        utils::write.table(data,
            file = rename,
            row.names = FALSE,
            sep = "\t",
            quote = FALSE
        )
    }
}
