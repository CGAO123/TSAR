#' Analyze to Normalize
#'
#' The analyze_norm function allows users to process analysis through an UI
#'     interface. Function wraps together all functions with in TSA_analysis
#'     family and read_write_analysis family.
#'
#' @importFrom openxlsx read.xlsx
#' @import shiny
#' @importFrom rhandsontable rHandsontableOutput rhandsontable
#' @importFrom plotly ggplotly subplot layout plotlyOutput renderPlotly
#' @import ggplot2
#'
#' @export
#' @return shiny application
#'
#' @param raw_data The raw data for analysis.
#'
#' @seealso \code{\link[TSAR]{gam_analysis}}, \code{\link{read_tsar}},
#'   \code{\link{write_tsar}}, \code{\link{join_well_info}}
#'
#' @examples
#' if (interactive()) {
#'     data("qPCR_data1")
#'     shiny::runApp(analyze_norm(qPCR_data1))
#' }
#'
analyze_norm <- function(raw_data) {
    ui <- fluidPage(
        analyzepage(raw_data, deparse(substitute(raw_data)))
    )

    server <- function(input, output) {
        analysis <- reactiveVal(NULL)
        analyzed_done <- reactiveVal(FALSE)
        imported <- reactiveVal(FALSE)
        output_data <- reactiveVal(NULL)
        well_info <- reactiveVal(NULL)
        inFile <- reactiveVal(NULL)
        data <- reactive({
            req(input$excelFile)
            inFile(input$excelFile$datapath)
            imported(TRUE)
            if (!is.null(inFile())) {
                read.xlsx(inFile(), sheet = 1)
            }
        })

        render_data(input, output, raw_data)
        render_data_title(input, output, "Current Data: raw_data")
        preview_model(input, output, raw_data)
        perform_analysis(
            input, output, raw_data, analysis,
            output_data, analyzed_done
        )
        join_condition(
            input, output, analysis, output_data,
            imported, analyzed_done, well_info, inFile
        )
        write_file(input, output, analyzed_done, output_data)
        preview_output(
            input, output, analyzed_done, imported,
            output_data, analysis, inFile, well_info
        )
        manual_well(input, output)
        set_condition(
            input, output, imported, analyzed_done,
            inFile, well_info, analysis, output_data
        )
        preview_condition(input, output, well_info, data)
        hide(input, output)
        save_change(input, output, imported, well_info)
        save_analysis(input, output, output_data)
        stop_analyze(input, output)
    }
    app <- shinyApp(ui = ui, server = server)
}
