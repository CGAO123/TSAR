#' Well information input function
#'
#' Reads in the ligand and protein information and joins them accordingly
#'     to the big data frame for graphing purposes.
#'
#' @importFrom magrittr %>%
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate select left_join
#'
#' @param file_path string; file path to read in the file
#' @param file object; use file to override the need of file_path if
#'     information is already read in
#' @param analysis_file data frame; data frame containing smoothed fluorescence
#'     data and tm values
#' @param skips integer; number indicating the number of headers present in
#'     input file, default set to 0 when file input is "by_well"
#'     If the input follows the excel template, this parameter does not apply.
#' @param nrows integer; number indicating the number of rows the data is.
#'     Default set to 96 assuming analysis on 96 well plate. Parameter is only
#'     applicable when file input is "by_well". If inputting by excel
#'     template, this parameter does not apply, please ignore.
#' @param type string; variable specifies the type of input read in.
#'     type = "by_well" requires input of csv or txt files of three variables:
#'     Well, Protein, Ligand.
#'     type = "by_template" requires input of excel file following the template
#'     format provided
#' @return outputs data frame joining data information with well information
#'
#' @family read_write_analysis
#' @examples
#' data("qPCR_data1")
#' result <- gam_analysis(qPCR_data1, smoothed = TRUE, fluo = 5)
#' data("well_information")
#' join_well_info(
#'     file_path = NULL, file = well_information,
#'     read_tsar(result, output_content = 2), type = "by_template"
#' )
#'
#' @export
join_well_info <- function(
    file_path,
    file = NULL,
    analysis_file,
    skips = 0,
    nrows = 96,
    type) {
    # input by well
    type <- match.arg(type, choices = c("by_well", "by_template"))

    if (type == "by_well") {
        well_info <- utils::read.delim(
            header = TRUE,
            skip = skips,
            nrow = nrows,
            file_path
        )
        well_info <- well_info %>%
            dplyr::select(c("Well", "Protein", "Ligand"))
        # input by excel template
    } else if (type == "by_template") {
        # read file, specify column as texts
        if (!is.null(file) && length(file) != 0) {
            well_info_table <- file
        } else {
            well_info_table <- readxl::read_excel(file_path,
                col_types =
                    c(
                        "text", "text", "text", "text", "text", "text",
                        "text", "text", "text", "text", "text", "text",
                        "text", "text", "text", "text", "text", "text",
                        "text", "text", "text", "text", "text", "text",
                        "text"
                    )
            )
        }

        # remove header row
        well_info_table <- well_info_table[-c(1), ]
        # seperate protein and ligand information into two data frames
        protein <- well_info_table[, c(
            1, 2, 4, 6, 8, 10, 12,
            14, 16, 18, 20, 22, 24
        )]
        ligand <- well_info_table[, -c(
            2, 4, 6, 8, 10, 12, 14,
            16, 18, 20, 22, 24
        )]
        # rename information by well
        names(protein) <-
            c(
                "Col", "01", "02", "03", "04", "05",
                "06", "07", "08", "09", "10", "11", "12"
            )
        names(ligand) <-
            c(
                "Col", "01", "02", "03", "04", "05",
                "06", "07", "08", "09", "10", "11", "12"
            )

        # pivot table to place values into correct positions
        protein <- protein %>%
            tidyr::pivot_longer(
                cols = !Col,
                names_to = "pos",
                values_to = "Protein"
            ) %>%
            mutate(Well = paste(Col, pos, sep = "")) %>%
            dplyr::select(Well, Protein)

        ligand <- ligand %>%
            tidyr::pivot_longer(
                cols = !Col,
                names_to = "pos",
                values_to = "Ligand"
            ) %>%
            mutate(Well = paste(Col, pos, sep = "")) %>%
            dplyr::select(Well, Ligand)

        # join protein and ligand data frames by well
        well_info <- left_join(protein,
            ligand,
            by = "Well"
        )
    }

    # join well information to big data frame
    combined <- left_join(analysis_file,
        well_info,
        by = c("Well.Position" = "Well")
    )

    combined <- na.omit(combined)
    return(combined)
}
