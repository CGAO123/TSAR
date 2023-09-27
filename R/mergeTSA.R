#' Merge TSA Raw Data and Analysis Files
#'
#'
#' This function is used to load both the Raw Data and the Analysis Results
#'     which are returned by the TSA software. Both output files have unique
#'     information regarding the experiment, and these need reunited for
#'     downstream analysis. Automatically generated Well IDs are use to merge
#'     similar data within the same experiment from the different files. Both
#'     Raw Data and the Analysis Results files must be specified. The returned,
#'     merged results from this function are required for downstream analysis
#'     as the format is set up for the automated workflow.
#'
#' @importFrom dplyr bind_rows
#'
#' @param raw_data_path,analysis_file_path a character string or vector of
#'     character strings; the path or the name of the file which the
#'     'RawData' or "AnalysisData'  are to be read from. Raw data and Analysis
#'     Data are to be loaded as a pair of results from the same experiment.
#'     When loading multiple files, the index/position of the pairs are
#'     merged where the first file specified by raw_data_path is to be merged
#'     with the first file specified by analysis_file_path. The same is done
#'     for the second, third, .. etc. \cr
#'     Either a .txt or .csv file; file type can vary between file pairs.
#'
#'     raw_data_path path must contain the term \emph{RawData} and
#'     analysis_file_path must contain the term  \emph{AnalysisResults} as the
#'     TSA software automatically assigns this when exporting data. Data is
#'     loaded from the \code{\link{read_raw_data}} and
#'     \code{\link{read_analysis}} functions within this merge_TSA function.
#'

#' @param protein can be used to select for an individual or multiple protein(s)
#'     as a character string matching protein names assigned in the TSA
#'     software. NA by default.
#' @param ligand can be used to select for an individual or multiple ligand(s)
#'     as a character string matching ligand names assigned in the TSA software.
#'     NA by default.
#'
#'
#' @return A data frame of merged TSA data.
#'
#' @section IDs:
#'     The TSAR package relies on matching conditions and file names for each
#'     well and for each set of conditions between multiple files output by
#'     the TSA software. Conditions are assigned to individual wells within
#'     the TSA software; these assigned values are detected by
#'     \code{\link{read_analysis}} and
#'     \code{\link{read_raw_data}} then are converted into IDs.
#'     Ensure your labeling of values within the TSA software is consistent
#'     so that similar values can be merged - typos or varying terms will be
#'     treated as distinct values within TSAR unless the values
#'     are manually specified by the user.
#'     Automatically generated well IDs within a TSA file can be found using the
#'     \code{\link{well_IDs}} function; condition IDs can be found using the
#'     \code{\link{condition_IDs}} function. \cr\cr
#'
#'
#'     \strong{Condition IDs} are generated only in the
#'     \code{\link{read_analysis}}, see that function's documentation for
#'     more details. Condition IDs are assigned to raw data in the
#'     \code{\link{merge_TSA}} function.
#'      \cr\cr
#'
#'     \strong{Well IDs} are similar to Condition IDs, as they are
#'     generated from columns in TSA output. Well IDs are used to match
#'     the analysis and raw data files for the same experiment, as both files
#'     contain unique, useful information for each well.
#'     The well ID includes the .eds file name saved from the
#'     PCR machine to match equivalent wells between files of the same
#'     experiment. Each well on all plates should have a unique well ID.
#'     If you wish to change or specify the file name used for the well ID,
#'     a new name can be manually assigned with the "manual_file" argument.
#'     \cr\cr
#'
#'
#' @family TSAR Formatting
#' @seealso \code{\link{read_raw_data}} and \code{\link{read_analysis}}
#'     for loading data.
#' @examples
#' # note: example does not contain example data to run
#' # merge_TSA(analysis_file_path, raw_data_path)
#'
#' @export
merge_TSA <- function(
    analysis_file_path,
    raw_data_path,
    protein = NA, # can filter by protein, as character
    ligand = NA # can filter by ligand, as character
    ) {
    #--- check the files are uploaded in pairs.
    n_pairs <- length(analysis_file_path) # needed for loop below
    if (!n_pairs == length(raw_data_path)) {
        stop("analysis_file_path and raw_data_path must be equal length")
    }


    tsa_data_list <- lapply(1:n_pairs, function(i) { # Repeat for every pair
        raw_data_i <- read_raw_data(path = raw_data_path[i]) # Load raw data
        raw_data_i <- raw_data_i[!names(raw_data_i) %in% c("Well", "Reading")]
        analysis_i <- read_analysis(path = analysis_file_path[i]) #read analyze
        tsa_data_i <- merge(analysis_i, raw_data_i, by = "well_ID") #Merge file
        return(tsa_data_i)
    })
    tsa_data <- bind_rows(tsa_data_list) #bind preprocessed list

    #--- Filters
    if (!is.na(protein)) { # Filter by protein
        tsa_data <- tsa_data[tsa_data$Protein %in% protein, ]
    }
    if (!is.na(ligand)) { # Filter by ligand
        tsa_data <- tsa_data[tsa_data$Ligand %in% ligand, ]
    }
    return(tsa_data)
}
