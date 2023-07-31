#' Screen raw curves
#'
#' screens multiple wells of data and prepares to assist identification of
#'   corrupted wells and odd out behaviors
#'
#' @import ggplot2
#' @importFrom dplyr rename
#'
#' @param raw_data input raw_data
#' @param checkrange list type input identifying range of wells to select.
#'   For example, if viewing first 8 wells from row A to C is needed, one can
#'   specify the row letters and column numbers like this:
#'   \code{checkrange = c("A", "C", "1", "8")}
#' @param checklist use this parameter to view selected Wells with full
#'   Well names. For example, \code{checklist = c('A01', 'D11')}
#'
#' @examples
#' data("qPCR_data1")
#' screen(qPCR_data1, checkrange = c("A", "C", "1", "12"))
#'
#' @return returns a ggplot graph colors by well IDs
#'
#' @family data_preprocess
#'
#' @export



screen <- function(raw_data,
                   checkrange = NULL,
                   checklist = NULL) {
    if (is.null(checkrange) && (is.null(checklist))) {
        section <- raw_data
    } else {
        if (!(is.null(checklist))) {
            checkrange <- checklist
        } else {
            grid <- expand.grid(
                LETTERS[match(checkrange[1], LETTERS):
                match(checkrange[2], LETTERS)],
                sprintf("%02d", checkrange[3]:
                checkrange[4])
            )
            checkrange <- paste0(grid$Var1, grid$Var2)
        }
        if ("Well.Position" %in% names(raw_data)) {
            section <- subset(raw_data, Well.Position %in% checkrange)
        } else if ("Well" %in% names(raw_data)) {
            section <- subset(raw_data, Well %in% checkrange)
            section <- dplyr::rename(section, Well.Position = Well)
        } else {
            stop("Error: No valid Well variable was found.
                  Make sure it is named 'Well.Position' or 'Well'")
        }
    }
    screened <- data.frame()
    for (i in unique(section$Well.Position)) {
        by_well <- subset(section, Well.Position == i)
        by_well <- normalize(by_well,
            selected = c(
                "Well.Position", "Temperature",
                "Fluorescence", "Normalized"
            )
        )
        screened <- rbind(screened, by_well)
    }
    if (nrow(screened) == 0) {
        stop("Error: Select of Wells do not exist or are already removed.")
        return()
    }
    ggplot(data = screened, aes(
        x = Temperature,
        y = Fluorescence,
        color = Well.Position)) +
        geom_line(size = 0.2) +
        theme_bw() +
        theme(panel.grid.major = element_blank())
}

#' Remove selected raw curves
#'
#' Removes selected curves with specified wells and range.
#'
#' @param raw_data dataframe; to be processed data
#' @param removerange list type input identifying range of wells to select.
#'   For example, if removing all 12 wells from row D to H is needed, one can
#'   specify the row letters and column numbers like this:
#'   \code{removerange = c("D", "H", "1", "12")}
#' @param removelist use this parameter to remove selected Wells with full
#'   Well names. For example, \code{removelist = c('A01', 'D11')}
#'
#' @examples
#' data("qPCR_data1")
#' remove_raw(qPCR_data1, removelist = c("A01", "D11"))
#'
#' @return dataframe; data frame with specified well removed
#' @family data_preprocess
#' @export
#'
remove_raw <- function(raw_data,
                       removerange = NULL,
                       removelist = NULL) {
    if (is.null(removerange) && is.null(removelist)) {
        return(raw_data)
    } else if (!is.null(removerange)) {
        grid <- expand.grid(
            LETTERS[match(removerange[1], LETTERS):match(
                removerange[2], LETTERS
            )],
            sprintf("%02d", removerange[3]:removerange[4])
        )
        removerange <- paste0(grid$Var1, grid$Var2)
    }
    removerange <- c(removerange, removelist)

    if ("Well.Position" %in% names(raw_data)) {
        return(subset(raw_data, !Well.Position %in% removerange))
    } else if ("Well" %in% names(raw_data)) {
        return(subset(raw_data, !Well %in% removerange))
    } else {
        stop("Error: No valid Well variable was found.
         Make sure it is named 'Well.Position' or 'Well'")
    }
}



#' View Model
#'
#' Function reviews data by well and output a graph of the fit and a graph of
#'   derivative. Function called within analyze_norm function.
#'
#' @import ggplot2
#' @param raw_data dataset input, not processing needed
#' @examples
#' data("qPCR_data1")
#' test <- subset(qPCR_data1, Well.Position == "A01")
#' test <- normalize(test)
#' gammodel <- model_gam(test, x = test$Temperature, y = test$Normalized)
#' test <- model_fit(test, model = gammodel)
#' view_model(test)
#'
#' @return list of two ggplot graphs
#' @family data_preprocess
#' @export
#'
view_model <- function(raw_data) {
    fit <- ggplot(data = raw_data, aes(x = Temperature, y = Normalized)) +
        geom_point(
            shape = 1, alpha = 0.5,
            aes(color = "Normalized Fluorescence")
        ) +
        geom_line(aes(y = fitted, color = "Fitted Model"), size = 0.5) +
        geom_vline(xintercept = Tm_est(raw_data), color = "red") +
        labs(color = "Curves", y = "RFU") +
        theme_bw() +
        theme(panel.grid.major = element_blank())

    raw_data <- na.omit(raw_data)
    deriv <- ggplot(data = raw_data, aes(
        x = Temperature, y = norm_deriv,
        color = Well.Position
    )) +
        geom_point(
            shape = 1, alpha = 0.5, size = 0.3,
            aes(color = "derivative Norm-Fluo")
        ) +
        geom_vline(xintercept = Tm_est(raw_data), color = "red") +
        scale_color_manual(values = "blue") +
        labs(color = "Curves", y = "dRFU") +
        theme_bw() +
        theme(panel.grid.major = element_blank())

    return(list(fit, deriv))
}
