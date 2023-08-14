#' Calculate Tm difference for all conditions
#'
#' From a specified control condition, the change in Tm is calculated for
#'     each condition in the tsa_data. Specifically,
#'     \eqn{Tm = condition - control}. Individual Tm values are averaged by
#'     condition, see \code{\link{TSA_average}} for details.
#'     To see all conditions use \code{condition_IDs(tsa_data)}.
#' @inheritParams TSA_average
#' @param control_condition character string matching a Condition ID. Must be
#'     equal to a value within tsa_data$condition_ID. See unique condition IDs
#'     with \code{\link{condition_IDs}}.
#' @return a data frame of reformatted data with the \code{\link{TSA_average}}
#'     data and the Tm.
#' @family TSAR Formatting
#' @seealso \code{\link{merge_TSA}} for preparing data.
#'     \code{\link{TSA_average}} for more information on the output data.
#'     \code{\link{condition_IDs}} to get unique Condition IDs within the
#'     input.
#'     \code{\link{TSA_boxplot}} for application.
#' @examples
#' data("example_tsar_data")
#' control <- condition_IDs(example_tsar_data)[1]
#' Tm_difference(example_tsar_data, control_condition = control)
#'
#' @export
Tm_difference <-
    function(tsa_data,
             control_condition) {
        TM_DF <- TSA_Tms(tsa_data)

        if (!is.na(control_condition)) {
            if (!control_condition %in% condition_IDs(tsa_data)) {
                stop("control_condition is not found in tsa_data$condition_ID")
            }
            ctrl_avg <- TM_DF$Avg_Tm[TM_DF$condition_ID == control_condition]
        }
        TM_DF$delta_Tm <- TM_DF$Avg_Tm - ctrl_avg

        return(TM_DF)
    }
