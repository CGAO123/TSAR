#' well information input function
#'
#' reads in the ligand and protein information and joins them accordingly
#' to the big data frame for graphing purposes
#'
#' @importFrom magrittr %>%
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_longer
#'
#' @param file_path string; file path to read in the file
#' @param analysis_path data frame; data frame containing smoothed fluorescence
#' data and tm values
#' @param skips integer; number indicating the number of headers present in
#' input file, default set to 2 when file input is by_well
#' If the input follows the excel template, this parameter does not apply,
#' can be ignored
#' @param nrows ineger; number indicating the number of rows the data is.
#' Default set to 95 assuming analysis on 96 well plate. If inputting by excel
#' template, this parameter does not apply, may be ignored.
#' @return
#' examples
#' #Analysis <- join.well_info("/Users/candygao/Desktop/qpcrresult/
#'                            CA_IP_HCB_2_20220110_134917_AnalysisResults.txt",
#'                            output_data, type = "by_well")
#' #Analysis <- join.well_info("~/Desktop/qpcrresult/Well Information Template.xlsx",
#'                          output_data,
#'                          type = "by_template")
#'
#' @export
join.well_info <- function(file_path,
                           analysis_file,
                           skips = 2,
                           nrows = 95,
                           type){

    #input by well
    if (type == "by_well"){
        well_info <- read.delim(header = TRUE,
                                skip = skips,
                                nrow = nrows,
                                file_path)
        well_info <- well_info%>%
            dplyr::select(Well, Protein, Ligand)
        #input by excel template
    }else if (type == "by_template"){
        #read file, specify column as texts
        well_info_table <- readxl:: read_excel(file_path,
                                      col_types =
                                    c("text","text","text","text","text","text",
                                    "text","text","text","text","text","text",
                                    "text","text","text","text","text","text",
                                    "text","text","text","text","text","text",
                                    "text"))
        #remove header row
        well_info_table <- well_info_table[-c(1),]
        #seperate protein and ligand information into two data frames
        protein <- well_info_table[,c(1,2,4,6,8,10,12,14,16,18,20,22,24)]
        ligand <- well_info_table[,-c(2,4,6,8,10,12,14,16,18,20,22,24)]
        #rename information by well
        names(protein) <-
            c("Col","01","02","03","04","05","06","07","08","09","10","11","12")
        names(ligand) <-
            c("Col","01","02","03","04","05","06","07","08","09","10","11","12")

        #pivot table to place values into correct positions
        protein <- protein %>%
            tidyr::pivot_longer(cols = !Col,
                         names_to = "pos",
                         values_to = "Protein")%>%
            mutate(Well  = paste(Col, pos, sep = "")) %>%
            dplyr::select(Well, Protein)

        ligand <- ligand %>%
            tidyr::pivot_longer(cols = !Col,
                         names_to = "pos",
                         values_to = "Ligand")%>%
            mutate(Well  = paste(Col, pos, sep = "")) %>%
            dplyr::select(Well, Ligand)

        #join protein and ligand data frames by well
        well_info <- left_join(protein, ligand, by = "Well" )
    }

    #join well information to big data frame
    combined <- left_join(analysis_file, well_info,
                          by = c("Well.Position" = "Well"))
    return(combined)
}

