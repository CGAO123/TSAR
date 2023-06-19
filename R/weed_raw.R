#' Weed Raw
#'
#' The weed_raw function allows users to interact with a screening graph and select
#' curves to weed out before entering analysis.
#'
#' @import plotly
#' @import shiny
#' @import shinyjs
#' @importFrom jsonlite toJSON
#' @export
#'
#' @param raw_data The raw data for screening.
#' @param tobechecked The curves to be checked during screening.
#'
#' #examples weed_raw(raw_data, tobechecked = c("A11", "A12"))
#'
weed_raw <- function(raw_data,
                     checkrange = NULL,
                     checklist = NULL) {

    shinyApp(
        ui = fluidPage(
            useShinyjs(),
            plotlyOutput("distPlot"),
            verbatimTextOutput("info"),
            actionButton("myButton", "Copy Selected"),
            verbatimTextOutput("copiedMessage"),
            actionButton("removeButton", "Remove Data"),
            verbatimTextOutput("removedMessage"),
            actionButton("refreshButton", "Refresh Screening"),
            verbatimTextOutput("refreshedMessage"),
            actionButton("stopButton", "Close Window")
        ),
        server = function(input, output) {
            clicked_points_legend_text <- NULL

            clicked_points <- reactiveValues(data = NULL, legend_text = NULL)

            output$distPlot <- renderPlotly({
                gg1 <- screen(raw_data, checkrange = checkrange, checklist = checklist)
                plotly::ggplotly(gg1, source = "Plot1")
            })

            observeEvent(event_data("plotly_click", source = "Plot1"), {
                d <- event_data("plotly_click", source = "Plot1")
                gg1 <- screen(raw_data, checkrange = checkrange, checklist = checklist)
                legend_text <- gg1$data$Well.Position[gg1$data$Fluorescence == d$y]

                clicked_points$data <- rbind(clicked_points$data, d)
                clicked_points$legend_text <- c(clicked_points$legend_text, legend_text)
                clicked_points_legend_text <<- c(clicked_points_legend_text, legend_text)
            })

            output$info <- renderPrint({
                cat("Selected Curve: ", clicked_points$legend_text)
            })

            observeEvent(input$myButton, {
                clicked_points <- isolate(paste0("'", gsub(",", "','", unique(clicked_points_legend_text)), "'")
                    )
                jscode <- sprintf(
                    "var message = %s;
                     var tempInput = $('<input>');
                     $('body').append(tempInput);
                     tempInput.val(message).select();
                     document.execCommand('copy');
                     tempInput.remove();
                     Shinyjs.showAlert('Copied!', type = 'success');",
                    jsonlite::toJSON(clicked_points)
                )
                runjs(jscode)
                output$copiedMessage <- renderPrint({
                    cat("Successfully Copied: ",
                        "Review using screen() and remove data with caution.")
                })
            })

            # Remove selected data
            observeEvent(input$removeButton, {
                raw_data <<- remove_raw(raw_data,
                                       removelist = clicked_points_legend_text)
                output$removedMessage <- renderPrint({
                    cat("Successfully Removed: ",
                        "Review new data with screen() in console or within window.")
                })
            })

            # Remove selected data
            observeEvent(input$refreshButton, {
                output$distPlot <- renderPlotly({
                    gg1 <- screen(raw_data, checkrange = checkrange, checklist = checklist)
                    plotly::ggplotly(gg1, source = "Plot1")
                })
                output$refreshedMessage <- renderPrint({
                    cat("Successfully Refreshed: ",
                        "All edits to dataframe are temporary. ",
                        "Copy wells and call function remove_raw() in console or script
                        to store change permanently")
                })
                output$removedMessage <- renderPrint({
                    message(NULL)
                })
                output$copiedMessage <- renderPrint({
                    message(NULL)
                })
                output$info <- renderPrint({
                    cat("Selected Curve: ", clicked_points$legend_text)
                })
            })

            # Terminate the Shiny app
            observeEvent(input$stopButton, {
                stopApp()
            })
        }
    )
}
