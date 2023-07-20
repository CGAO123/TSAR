#' Graph tsar_data
#'
#' The weed_raw function allows users to interact with a screening graph
#'   and select curves to weed out before entering analysis. Function wraps
#'   together \code{\link[TSAR]{screen}} and \code{\link{remove_raw}}.
#'
#' @import shiny
#' @import ggplot2
#' @importFrom shinyjs toggle hidden
#' @importFrom ggpubr ggarrange
#' @importFrom shinyWidgets actionBttn
#'
#' @export
#'
#' @param tsar_data tsar data outputted by norm_to_tsar or merge_tsa
#'
#' @return prompts separate app window for user interaction,
#'   does not return specific value; generates boxplot and compare plots
#'   according to user input
#'
#' @family TSA Plots
#'
#' @seealso \code{\link{TSA_boxplot}}, \code{\link{tsa_compare_plot}},
#'   \code{\link{condition_IDs}}, \code{\link{well_IDs}}
#'
#' @examples
#' if (interactive()) {
#'     data("example_tsar_data")
#'     shiny::runApp(graph_tsar(example_tsar_data))
#' }
#'
graph_tsar <- function(tsar_data = data.frame()) {
    source("~/Desktop/TSAR/R/interface_graph.R")
    source("~/Desktop/TSAR/R/observers_graph.R")
    source("~/Desktop/TSAR/R/outputs_graph.R")

    ui <- graphpage(tsar_data)

    server <- function(input, output, session) {
        datelist <- reactiveVal(NULL)
        options(shiny.maxRequestSize = 30 * 1024^2)
        dated <- reactiveVal(FALSE)
        compare <- reactiveVal(NULL)
        graph_tsar_data <- reactiveVal(tsar_data)

        hide_p(input, output)
        toggle_p(input, output)
        initialize(input, output, graph_tsar_data)
        build_dates(input, output)
        save_dates(input, output, dated, datelist)
        merge_update(input, output, dated, graph_tsar_data, datelist, session)
        build_boxplot(input, output, graph_tsar_data)
        build_compare(input, output, graph_tsar_data, compare)
        view_compare(input, output, compare)
        build_curves(input, output, graph_tsar_data)
        render_condition(input, output, graph_tsar_data)
        render_well(input, output, graph_tsar_data)
        render_tm(input, output, graph_tsar_data)
        render_deltatm(input, output, graph_tsar_data)
        render_message(input, output)
        closegraph(input, output)
    }

    app <- shinyApp(ui = ui, server = server)
}
