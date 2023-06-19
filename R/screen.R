#' screen function allows user to screen data for selection
#'
#' screens multiple wells of data and prepares to assist identification of
#'   corrupted wells and odd out behaviors
#'
#' @importFrom magrittr %>%
#' @import ggplot2
#'
#'
#' @param raw_data input raw_data
#' @param checkrange list type input identifying specific selections of well.
#'   For example, if screening for only 6 wells of row A is needed, one can
#'   specify the row letters and column numbers like this:
#'   `checkrange = c("A", "C", "1", "8")`
#' @param checklist use this parameter to view selected Wells with full
#'   Well names
#'
#'
#' @return returns a ggplot graph colors by well IDs
#'
#' @family data_preprocess
#'
#' @export



screen <- function(raw_data,
                   checkrange = NULL,
                   checklist = NULL) {
    #check for variable name, if Well.Position is not found throw error
    #do the same for Temperature and fluorescence


    if (is.null(checkrange) & (is.null(checklist))) {
        section <- raw_data
    } else {
        if (!(is.null(checklist))) {
            checkrange <- checklist
        } else {
            grid <- expand.grid(LETTERS[match(checkrange[1], LETTERS):
                                        match(checkrange[2], LETTERS)],
                                sprintf("%02d", checkrange[3]:
                                                checkrange[4]))
            checkrange <- paste0(grid$Var1, grid$Var2)
        }

        if ('Well.Position' %in% names(raw_data)){
            section <- filter(raw_data, Well.Position %in% checkrange)
        } else if ('Well' %in% names(raw_data)){
            section <- filter(raw_data, Well %in% checkrange) %>%
                rename(Well.Position = Well)
        } else {
            error("No valid Well variable was found.
                  Make sure it is named 'Well.Position' or 'Well'")
        }
    }
    screened <- data.frame()
    for (i in unique(section$Well.Position)) {
        by_well <- section %>%
            filter(Well.Position == i)
        by_well <- normalize(by_well,
                             selected = c("Well.Position", "Temperature",
                                          "Fluorescence", "Normalized"))
        screened <- rbind(screened, by_well)
    }

    ggplot(data = screened, aes(x = Temperature,
                                y = Fluorescence,
                                color = Well.Position)) +
        geom_point(shape = 1)
    #ggplotly(g)

}

#' remove_raw removes selected curves
#'
#' @param raw_data dataframe; to be processed data
#' @param removerange list; list of 4 string, specifying range of wells to be removed
#' @param removelist list; list of indiviudal well numbers that need to be removed
#'
#' @return dataframe; data frame with specified well removed
#'
#' @export
#'
remove_raw <- function(raw_data,
                       removerange = NULL,
                       removelist = NULL) {

    if (is.null(removerange) & (is.null(removelist))) {
        return(raw_data)
    } else if (!(is.null(removerange))) {
            grid <- expand.grid(LETTERS[match(removerange[1], LETTERS):
                                            match(removerange[2], LETTERS)],
                                sprintf("%02d", removerange[3]:
                                            removerange[4]))
            removerange <- paste0(grid$Var1, grid$Var2)
        }
    removerange <- c(removerange, removelist)


    if ('Well.Position' %in% names(raw_data)) {
        return(filter(raw_data, !Well.Position %in% removerange))
    } else if ('Well' %in% names(raw_data)) {
        return(filter(raw_data, !Well %in% removerange))
    } else {
        error("No valid Well variable was found.
                  Make sure it is named 'Well.Position' or 'Well'")
    }

}
