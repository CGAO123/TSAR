#' Graph tsar_data
#'
#' Function allows users to graph out tsar_data, building boxplot, compare
#'     plots, and curves by condition. Input of data as parameter is optional.
#'     graph_tsar wraps together all graphing functions and relative helper
#'     functions.
#'
#' @import shiny
#' @import ggplot2
#' @importFrom plotly ggplotly subplot layout plotlyOutput renderPlotly
#' @importFrom shinyjs toggle hidden
#' @importFrom ggpubr ggarrange
#' @importFrom shinyWidgets actionBttn
#'
#' @export
#'
#' @param tsar_data tsar data outputted by merge_norm or merge_tsa.
#'     Parameter is optional. If no data is passed, access the merge panel
#'     to merge norm_data into tsar_data.
#'
#' @return prompts separate app window for user interaction,
#'     does not return specific value; generates boxplot and compare plots
#'     according to user input
#'
#' @family TSA Plots
#'
#' @seealso \code{\link{TSA_boxplot}}, \code{\link{TSA_compare_plot}},
#'     \code{\link{condition_IDs}}, \code{\link{well_IDs}},
#'     \code{\link{merge_norm}}, \code{\link{TSA_Tms}},
#'     \code{\link{Tm_difference}}
#'
#' @examples
#' if (interactive()) {
#'     data("example_tsar_data")
#'     shiny::runApp(graph_tsar(example_tsar_data))
#' }
#'
graph_tsar <- function(tsar_data = data.frame()) {
    ui <- graphpage(tsar_data)

    server <- function(input, output, session) {
        datelist <- reactiveVal(NULL)
        options(shiny.maxRequestSize = 30 * 1024^2)
        dated <- reactiveVal(FALSE)
        compare <- reactiveVal(NULL)
        graph_tsar_data <- reactiveVal(tsar_data)
        stock_tsar_data <- reactiveVal(tsar_data)

        dummy_plot(input, output)
        hide_p(input, output)
        toggle_p(input, output)
        initialize(input, output, graph_tsar_data)
        build_dates(input, output)
        save_dates(input, output, dated, datelist)
        saved_merged(input, output, graph_tsar_data)
        merge_update(input, output, dated, graph_tsar_data, datelist, session)
        build_boxplot(input, output, graph_tsar_data)
        build_compare(input, output, graph_tsar_data, compare)
        view_compare(input, output, compare)
        build_curves(input, output, graph_tsar_data)
        build_derivatives(input, output, graph_tsar_data)
        render_condition(input, output, graph_tsar_data)
        render_well(input, output, graph_tsar_data)
        render_tm(input, output, graph_tsar_data)
        render_message(input, output)
        remove_selected_graph(input, output, graph_tsar_data)
        restore_removal(input, output, graph_tsar_data, stock_tsar_data)
        closegraph(input, output)
    }

    app <- shinyApp(ui = ui, server = server)
}
