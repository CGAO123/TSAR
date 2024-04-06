#' Read TSA Analysis Data
#'
#' Open TSA Analysis files. This function is used to load data output from
#'     the thermal shift software
#'     analysis tab. Can be either .txt or .csv file with a path / file name
#'     including the string "AnalysisResults" due to its automatic naming from
#'     the software. The values assigned to wells within the TSA software are
#'     automatically extracted from the loaded file; values must be assigned
#'     within the TSA software for the automated workflow (See IDs Section
#'     Below).
#'     \strong{Note:} Wells that do not have an Analysis Group assigned are
#'     removed.
#'     The TSA software automatically assigns all wells to Analysis Group 1 by
#'     default, and can be changed but not removed by the software.
#'
#' @importFrom stringr str_detect str_replace_all
#' @importFrom tidyr unite
#'
#' @param path a character string; the path or the name of the file which the
#'    'AnalysisResults' data are to be read from. Either a .txt or .csv file.
#'     The path must contain the term \emph{AnalysisResults} as the TSA software
#'     automatically assigns this when exporting data.
#' @param type either c("boltzmann", "derivative"); \code{type = "derivative")}
#'     by default. Determines what model of
#'     Tm estimation to load from the TSA software. Loads Tms as 'Tm B'  when
#'     \code{type = "boltzmann")}; loads Tms as 'Tm D' when
#'     \code{type = "derivative")}.
#' @param conditions A character vector of condition types assigned within the
#'     TSA software to load.
#'     \code{conditions = c("Protein", "Ligand")} by default.
#'     These conditions are used to generate the IDs discussed.
#' @param manual_file NA by default. User can specify .eds for merging if needed
#'     for Well IDs if needed with a character string.
#' @param manual_conditions,manual_wells NA by default,
#'     enabling automated analysis.
#'     A character vector of Condition IDs and Well IDs to manually assign
#'     each row of the read data.
#' @param skip_flags logical value; \code{type = FALSE} by default.
#'     When \code{type = TRUE}, wells that have flags reported by TSA software
#'     are removed.
#' @return A data frame of TSA analysis data.
#' @section IDs:
#'     The TSAR package relies on matching conditions and file names for each
#'     well and for each set of conditions between multiple files output by
#'     the TSA software. Conditions are
#'     assigned to individual wells within the TSA software; these assigned
#'     values are detected by
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
#'     \strong{Condition IDs} are generated from columns in TSA output
#'     specified by the 'conditions' argument.
#'     Protein and Ligand values, the default conditions within the TSA
#'     software, are the values used to create these IDs. You can manually
#'     specify the
#'     condition categories from the TSA software, including user-made
#'     conditions. Condition IDs are used to match equivalent observations
#'     between technical and biological replicates. Wells with identical
#'     condition IDs, specified by the 'conditions' argument, will be
#'     aggregated in down-stream analysis; user-specified conditions must
#'     remain consistent in use and order to create compatible IDs between TSA
#'     files from the same experiment and between replicates. \cr\cr
#'
#'     \strong{Well IDs} are similar to Condition IDs, as they are
#'     generated from columns in TSA output that are
#'     specified by the 'conditions' argument. Well IDs are used to match
#'     the analysis and raw data files for the same experiment, as both files
#'     contain unique, useful information for each well. In addition to the
#'     condition ID, the well ID includes the .eds file name saved from the
#'     PCR machine to match equivalent wells between files of the same
#'     experiment. Each well on all plates should have a unique well ID.
#'     If you wish to change or specify the file name used for the well ID,
#'     a new name can be manually assigned with the "manual_file" argument.
#'     \cr\cr
#'
#'     The user may manually assign condition IDs using the
#'     'manual_conditions' argument rather than using the automatically
#'     generated IDs. The same is true for well IDs, which can be manually
#'     assigned with 'manual_wells'. This is not suggested, as there may be
#'     issues with matching if well/conditions are not properly matching. This
#'     gives the potential for errors in downstream applications as well.
#'
#' @family Read TSA Data
#' @seealso \code{\link{read_raw_data}} for loading accompanying data.
#'     \code{\link{merge_TSA}} for joining Analysis Results and Raw Data files
#'     from the TSA software.
#' @examples
#' path <- "~/Desktop/analysis_data"
#' # note: example does not contain example data to run
#' # read_analysis(path)
#' @export
#'
#'
read_analysis <- function(
    path,
    type = "derivative",
    conditions = c("Protein", "Ligand"),
    manual_conditions = NA,
    manual_wells = NA,
    skip_flags = FALSE,
    manual_file = NA) {

    type <- match.arg(type, choices = c("fluorescence",
                                        "derivative", "boltzmann"))

    # Note: Wells that do not have an Analysis Group assigned are removed
    #--- Loading and formatting


    if (!stringr::str_detect(path, "AnalysisResults")) {
        warning('Check the input file,
                    The path name does not include "AnalysisResults"')
    }
    if (stringr::str_detect(path, pattern = "(.*\\.txt$)|(.*\\.csv$)")) {
        if (stringr::str_detect(path, pattern = "(.*\\.txt$)")) {
            analysis <- read.delim(path,
                skip = 2,
                na.strings = c("", "NA", " ")
            )
        }
        if (stringr::str_detect(path, pattern = "(.*\\.csv$)")) {
            analysis <- read.csv(path,
                skip = 2,
                na.strings = c("", "NA", " ")
            )
        }
        names(analysis) <- stringr::str_replace_all(names(analysis), "\\.", " ")
        names(analysis)[names(analysis) == "Flag Indicator"] <- "Flags"
        analysis <- analysis[seq_len(96), ] # Trim to 96 wells
        analysis$`Tm D` <- as.numeric(analysis$`Tm D`)
        analysis$`Tm B` <- as.numeric(analysis$`Tm B`)
    } else {
        stop("File type not .csv or .txt")
    }
    if (type == "boltzmann") {
        col_names <-
            c(
                "Well", conditions, "Tm B", "dTm B",
                "Analysis Group", "Flags", "Experiment File Name"
            )
    }
    if (type == "derivative") {
        col_names <-
            c(
                "Well", conditions, "Tm D", "dTm D",
                "Analysis Group", "Flags", "Experiment File Name"
            )
    }
    # Keep only rows with an analysis group and required+condition cols
    analysis <- analysis[(!is.na(analysis$`Analysis Group`)), col_names]
    names(analysis)[names(analysis) %in% c("Tm D", "Tm B")] <- "Tm"
    #--- Generating ID codes for downstream functions
    #--- Making condition_ID-same for all equivalent wells (for well matching)
    if (is.na(manual_conditions)) { # Default, auto generate condition IDs
        analysis$condition_ID <-
            tidyr::unite(analysis, "condition_ID", conditions)$condition_ID
    } else {
        analysis$condition_ID <- manual_conditions # manual well assignments
    }
    #--- Making well_codes-each well is independent (for raw&analysis matching)
    if (!is.na(manual_file)) {
        analysis$`Experiment File Name` <- manual_file
    }
    if (is.na(manual_wells)) { # Default, auto generate condition IDs
        analysis$well_ID <-
            tidyr::unite(
                analysis, "well_ID",
                c("Well", "Experiment File Name")
            )$well_ID
    } else {
        analysis$well_ID <- manual_wells # manual well assignments
    }
    if (skip_flags) { # remove flagged rows if user specifies
        analysis <- analysis[analysis$Flags == 0, ]
    }

    return(analysis)
}
