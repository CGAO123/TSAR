#' read result
#'
#' reads previous pipeline output lists and organizes them into separate
#'   data frames
#'
#' @param result list; input uses resulting output of gam_analysis() function
#' @param code integer; \code{code = 0} returns only the tm value by wells
#'                      \code{code = 1} returns data table with fitted values
#'                      \code{code = 2} returns the combination of 0 and 1
#' @return output files with select dataset
#'
#' @family read_write_analysis
#'
#' @examples
#' #read_tsar(result, code = 0)
#' #output_data <- read_tsar(result, code = 2)
#'
#' @export
read_tsar <- function(result, code) {
    #returns only the tm value by wells
    wellpos <- result[[2]]$Well.Position
    wellpos <- paste(substr(wellpos, 1, 1),
                     sprintf("%02s", substr(wellpos, 2, nchar(wellpos))),
                     sep = "")
    if (code == 0) {
        tmval <- result[[1]]
        tmval <- data.frame(Well.Position =
                        unique(wellpos[grepl("[A-Z]+[0-9]+[0-9]$", wellpos)]),
                        TM = tmval)
        return(tmval)
    }else if (code == 1) {
        #returns the data table with the fitted values
        dat <- result[[2]]
        dat <- data.frame(dat)
        return(dat)
    }else if (code == 2) {
        #returns the all data, combination of 0 and 1
        tmval <- result[[1]]
        dat <- result[[2]]
        dat <- data.frame(dat)
        tmval <- data.frame(Well.Position =
                            unique(wellpos[grepl("[A-Z]+[0-9]+[0-9]$",
                                                 wellpos)]), TM = tmval)
        #left join seperate data frames into one
        all <- left_join(dat, tmval, by = "Well.Position")
        return(all)
    }
}


#' write output files
#'
#' writes output into csv or txt files
#'
#' @import utils
#'
#' @param data input data frame
#' @param name naming for file
#' @param file \code{file = "txt"} writes txt output files;
#'             \code{file = "csv"} writes csv output files;
#'             default set to \code{file = "txt"}
#' @return file output on the working directory where data was read in
#'
#' @family read_write_analysis
#' @examples
#' #write.tsar(output_data, name = "2022_03_18_test", file = "txt")
#'
#' @export
#'
write_tsar <- function(data, name, file = "txt") {
    if (file == "csv") {
        rename <- paste(name, "tsar_output.csv", sep = "_")
        utils::write.csv(data,
                  file = rename,
                  row.names = FALSE,
                  quote = FALSE)
    }else if (file == "txt") {
        rename <- paste(name, "tsar_output.txt", sep = "_")
        utils::write.table(data,
                    file = rename,
                    row.names = FALSE,
                    sep = "\t",
                    quote = FALSE)
    }
}
