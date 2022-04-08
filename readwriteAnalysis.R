#' read result
#'
#' reads previous pipeline output lists
#' organizes them into separate data frames
#'
#' @param result list; input uses resulting output of gam_analysis() function
#' @param code integer; code = 0 returns only the tm value by wells
#'                      code = 1 returns the data table with the fitted values
#'                      code = 2 returns the combination of 0 and 1
#' @return
#' @examples
#' #read.tsar(result, code = 0)
#' #output_data <- read.tsar(result, code = 2)
#'
#' @export
read.tsar <- function(result, code){
    #returns only the tm value by wells
    Wellpos <- paste(substr(raw_data$Well.Position,1,1),
                     sprintf("%02s",substr(raw_data$Well.Position,2,
                                           nchar(raw_data$Well.Position))),
                     sep = "")
    if(code == 0){
        tmval <- result[[1]]
        tmval <- data.frame(Well.Position = unique(Wellpos), TM = tmval)
        return(tmval)
    }else if(code == 1){
        #returns the data table with the fitted values
        dat <- result[[2]]
        dat <- data.frame(dat)
        return(dat)
    }else if (code == 2){
        #returns the all data, combination of 0 and 1
        tmval <- result[[1]]
        dat <- result[[2]]
        dat <- data.frame(dat)
        tmval <- data.frame(Well.Position = unique(Wellpos), TM = tmval)
        #left join seperate data frames into one
        all <- left_join(dat, tmval, by = "Well.Position")
        return(all)
    }
}


#' write output files
#'
#' writes output into csv or txt files
#'
#' @param data input data frame
#' @param name naming for file
#' @param file "txt" writes txt output files;
#'             "csv" writes csv output files;
#'             default set to "txt"
#' @return file output on the working directory where data was read in
#' @examples
#' #write.tsar(output_data, name = "2022_03_18_test", file = "txt")
#'
#' @export
write.tsar <- function(data, name, file = "txt"){
    if(file == "csv"){
        rename <- paste(name, "tsar_output.csv", sep="_")
        write.csv(data, file = rename, row.names=FALSE, quote = FALSE)
    }else if(file == "txt"){
        rename <- paste(name, "tsar_output.txt", sep="_")
        write.table(data, file = rename, row.names=FALSE, sep = "\t", quote = FALSE)
    }
}
