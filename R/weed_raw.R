#' Weed Raw
#'
#' The weed_raw function allows users to interact with a screening graph
#'   and select curves to weed out before entering analysis. Function wraps
#'   together \code{\link[TSAR]{screen}} and \code{\link{remove_raw}}.
#'
#' @importFrom plotly ggplotly plotlyOutput renderPlotly event_data
#' @import shiny
#' @importFrom shinyjs runjs useShinyjs
#' @importFrom jsonlite toJSON
#' @importFrom shinyWidgets materialSwitch
#'
#' @export
#'
#' @param raw_data The raw data for screening.
#' @param checkrange list type input identifying range of wells to select.
#'   For example, if viewing first 8 wells from row A to C is needed, one can
#'   specify the row letters and column numbers like this:
#'   \code{checkrange = c("A", "C", "1", "8")}
#' @param checklist use this parameter to view selected Wells with full
#'   Well names. For example, \code{checklist = c('A01', 'D11')}
#'
#' @return prompts separate app window for user interaction,
#'   does not return specific value
#'
#' @family data_preprocess
#'
#' @seealso \code{\link[TSAR]{screen}} and \code{\link{remove_raw}}
#'
#' @examples
#' data("qPCR_data1")
#' if (interactive()) {
#'     weed_raw(qPCR_data1, checkrange = c("A", "B", "1", "12"))
#' }
#'
weed_raw <- function(raw_data,
                     checkrange = NULL,
                     checklist = NULL) {
    source("~/Desktop/TSAR/R/observers_weed.R")
    source("~/Desktop/TSAR/R/outputs_weed.R")
    source("~/Desktop/TSAR/R/interface_weed.R")

    server <- function(input, output) {
        clicked_points <- reactiveValues(legend_text = NULL)
        highlighted_cells <- reactiveVal(NULL)
        unhighlighted_cells <- reactiveVal(NULL)
        dataset <- reactiveVal(raw_data)

        observe_grid(
            input, output, highlighted_cells,
            unhighlighted_cells
        )
        clear_selection(input, output, clicked_points, highlighted_cells)
        unhighlight_grid(input, output, highlighted_cells)
        highlight_grid(input, output, highlighted_cells)
        toggle_grid(input, output)
        print_click(input, output, clicked_points)
        copy_click(input, output, highlighted_cells, clicked_points)
        copy_click_full(
            input, output, highlighted_cells, dataName,
            clicked_points
        )
        stop_window(input, output)
        remove_selected(
            input, output, dataset, checkrange, checklist,
            gg1, highlighted_cells, clicked_points
        )
        refresh(input, output, dataset, checkrange, checklist, gg1)
        register_click(input, output, clicked_points, gg1)
        view_selected(
            input, output, dataset, checkrange, checklist,
            highlighted_cells, clicked_points, gg1
        )

        output$distPlot <- plotly::renderPlotly({
            gg1 <<- TSAR::screen(dataset(),
                checkrange = checkrange,
                checklist = checklist
            )
            plotdata <<- plotly::ggplotly(gg1, source = "plotdata")
            plotdata
            event_register(plotdata, "plotly_click")
        })
    }
    app <- shinyApp(ui = mainpage(raw_data), server = server)
}
