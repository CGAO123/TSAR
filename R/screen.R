#' Screen
#'
#' screens multiple wells of data and prepares to assist identification of
#'   corrupted wells and odd out behaviors
#'
#' @importFrom magrittr %>%
#' @import ggplot2
#'
#'
#' @param raw_data input raw_data
#' @param checkrange list type input identifying range of wells to select.
#'   For example, if viewing first 8 wells from row A to C is needed, one can
#'   specify the row letters and column numbers like this:
#'   `checkrange = c("A", "C", "1", "8")`
#' @param checklist use this parameter to view selected Wells with full
#'   Well names. For example, `checklist = c('A01', 'D11')`
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
  # check for variable name, if Well.Position is not found throw error
  # do the same for Temperature and fluorescence


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
      section <- filter(raw_data, Well.Position %in% checkrange)
    } else if ("Well" %in% names(raw_data)) {
      section <- filter(raw_data, Well %in% checkrange) %>%
        rename(Well.Position = Well)
    } else {
      stop("Error: No valid Well variable was found.
                  Make sure it is named 'Well.Position' or 'Well'")
    }
  }
  screened <- data.frame()
  for (i in unique(section$Well.Position)) {
    by_well <- section %>%
      filter(Well.Position == i)
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
    color = Well.Position
  )) +
    geom_line(size = 0.2) +
    theme_bw() +
    theme(panel.grid.major = element_blank())
}

#' remove_raw
#'
#' removes selected curves with specified wells and range
#' @import dplyr
#'
#' @param raw_data dataframe; to be processed data
#' @param removerange list type input identifying range of wells to select.
#'   For example, if removing all 12 wells from row D to H is needed, one can
#'   specify the row letters and column numbers like this:
#'   `removerange = c("D", "H", "1", "12")`
#' @param removelist use this parameter to remove selected Wells with full
#'   Well names. For example, `removelist = c('A01', 'D11')`
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
    return(filter(raw_data, !Well.Position %in% removerange))
  } else if ("Well" %in% names(raw_data)) {
    return(filter(raw_data, !Well %in% removerange))
  } else {
    stop("Error: No valid Well variable was found.
         Make sure it is named 'Well.Position' or 'Well'")
  }
}



#' View Model
#'
#' @import ggplot2
#' @param norm_data dataset input, data should match the needs of norm_data
#'
#' @return ggplot
#' @family data_preprocess
#' @export
#'
view_model <- function(norm_data) {
  ggplot(data = norm_data, aes(x = Temperature, y = Normalized)) +
    geom_point(
      shape = 1, alpha = 0.5,
      aes(color = "Normalized Fluorescence")
    ) +
    geom_line(aes(y = fitted, color = "Fitted Model"), size = 0.5) +
    geom_vline(xintercept = tm_est(norm_data), color = "red") +
    labs(color = "Curves") +
    theme_bw() +
    theme(panel.grid.major = element_blank())
}
