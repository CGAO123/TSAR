#' Merge and format data for sequential applications of graphing functions
#'
#' This function merges data of experiment replicates across different dates.
#'   It merges and produces information variables used to group wells of same
#'   set up.
#' @param norm_data1 data frame; normalized data outputted by read_tsar
#'   following gam_analysis. Input data for experiment replication day 1
#' @param norm_data2 data frame; normalized data outputted by read_tsar
#'   following gam_analysis. Input data for experiment replication day 2
#' @param file_name1 character string; input file name of experiment day 1
#'   for data tracing purpose
#' @param file_name2 character string; input file name of experiment day 2
#'   for data tracing purpose
#' @param exp_date1 character string; input date of experiment
#' @param exp_date2 character string; input date of experiment
#'
#' @export
#'
merge_norm <- function(
    norm_data1,
    norm_data2,
    file_name1,
    file_name2,
    exp_date1,
    exp_date2) {

    norm_data1 <- norm_data1 %>%
        mutate(ExperimentFileName = file_name1)
    norm_data2 <- norm_data2 %>%
        mutate(ExperimentFileName = file_name2)
    tsar_data <- data.frame(rbind(norm_data1, norm_data2))
    tsar_data <- tsar_data %>%
        #add well_ID column
        mutate(well_ID = paste(tsar_data$Well.Position,
                               tsar_data$Protein,
                               tsar_data$Ligand,
                               exp_date1
                               , sep="_"))%>%
        #add condition_ID column
        mutate(condition_ID = paste(tsar_data$Protein,
                                    tsar_data$Ligand,
                                    sep = "_")) %>%
        rename(Tm = tm, Well = Well.Position)
    return(tsar_data)
}
