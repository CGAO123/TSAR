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
#' #myApp <- weed_raw(raw_data, checklist = c("A11", "A12"))
#' #shiny::runApp(myApp)
#'
weed_raw <- function(raw_data,
                     checkrange = NULL,
                     checklist = NULL) {

        ui <- fluidPage(
            useShinyjs(),
            fluidRow(
                column(width = 4, h3("Current Raw Dataset: ")),
                h3(dataName <- deparse(substitute(raw_data)))
            ),
            plotlyOutput("distPlot"),
            verbatimTextOutput("info"),
            fluidRow(
                column(width=2,
                       actionButton("myButton", "Copy Selected Wells")),
                column(width=4,
                       actionButton("removeCall",
                                    "Copy Selected in Full Function Call")),
            ),
            verbatimTextOutput("copiedMessage"),
            br(),
            fluidRow(
                column(width=2,
                       actionButton("removeButton", "Remove Data")),
                column(width=2,
                       actionButton("viewRemovedButton", "View Selected"))
            ),
            verbatimTextOutput("removedMessage"),
            verbatimTextOutput("viewRemovedMessage"),
            br(),
            actionButton("refreshButton", "Refresh Screening"),
            verbatimTextOutput("refreshedMessage"),
            br(),
            actionButton("stopButton", "Close Window"),



        )
        server <- function(input, output) {
            clicked_points_legend_text <- NULL

            clicked_points <- reactiveValues(data = NULL, legend_text = NULL)

            output$distPlot <- plotly::renderPlotly({
                gg1 <- TSAR::screen(raw_data,
                              checkrange = checkrange,
                              checklist = checklist)
                plotly::ggplotly(gg1, source = "Plot1")
            })

            shiny::observeEvent(event_data("plotly_click",
                                            source = "Plot1"), {
                d <- event_data("plotly_click", source = "Plot1")
                gg1 <- TSAR::screen(raw_data,
                              checkrange = checkrange,
                              checklist = checklist)
                legend_text <- gg1$data$Well.Position[gg1$data$Fluorescence
                                                      == d$y]

                clicked_points$data <- rbind(clicked_points$data, d)
                clicked_points$legend_text <- c(clicked_points$legend_text,
                                                legend_text)
                clicked_points_legend_text <<- c(clicked_points_legend_text,
                                                 legend_text)
            })

            output$info <- renderPrint({
                cat("Selected Curve: ", clicked_points$legend_text)
            })

            shiny::observeEvent(input$myButton, {
                clicked_points <- gsub(", ", "','",
                                       toString(unique(
                                           clicked_points_legend_text)))
                tobecopied <- paste("'", clicked_points, "'",
                                    sep = "")
                jscode <- sprintf(
                    "var message = %s;
                     var tempInput = $('<input>');
                     $('body').append(tempInput);
                     tempInput.val(message).select();
                     document.execCommand('copy');
                     tempInput.remove();
                     Shinyjs.showAlert('Copied!', type = 'success');",
                    jsonlite::toJSON(tobecopied)
                )
                shinyjs::runjs(jscode)
                output$copiedMessage <- renderPrint({
                    cat("Successfully Copied: ",
                        "Review using screen() and remove data with caution.")
                })
            })

            shiny::observeEvent(input$removeCall, {
                clicked_points <- gsub(", ", "','",
                                       toString(unique(
                                           clicked_points_legend_text)))
                tobecopied <- paste("remove_raw(",
                                    dataName,
                                    ", removelist = c('", clicked_points, "'))",
                                    sep = "")
                jscode <- sprintf(
                    "var message = %s;
                     var tempInput = $('<input>');
                     $('body').append(tempInput);
                     tempInput.val(message).select();
                     document.execCommand('copy');
                     tempInput.remove();
                     Shinyjs.showAlert('Copied!', type = 'success');",
                    jsonlite::toJSON(tobecopied)
                )
                shinyjs::runjs(jscode)
                output$copiedMessage <- renderPrint({
                    cat("Successfully Copied: ",
                        "Paste function call to script and run directly to",
                        "remove selected wells.")
                })
            })

            # Remove selected data
            shiny::observeEvent(input$removeButton, {
                raw_data <<- remove_raw(raw_data,
                                       removelist = clicked_points_legend_text)
                output$removedMessage <- renderPrint({
                    cat("Successfully Removed: ",
                "Review new data with screen() in console or within window.")
                })
            })

            # Remove selected data
            shiny::observeEvent(input$refreshButton, {
                output$distPlot <- plotly::renderPlotly({
                    gg1 <- TSAR::screen(raw_data,
                                  checkrange = checkrange,
                                  checklist = checklist)
                    plotly::ggplotly(gg1, source = "Plot1")
                })
                output$refreshedMessage <- renderPrint({
                    cat("Successfully Refreshed: ",
                        "All edits to dataframe are temporary. ",
                        "Copy wells and call function remove_raw() in console ",
                        "or script to store change permanently")

                })
                output$removedMessage <- renderPrint({
                    message(NULL)
                })
                output$copiedMessage <- renderPrint({
                    message(NULL)
                })
                output$info <- renderPrint({
                    cat("Selected Curve: ", unique(clicked_points$legend_text))
                })
            })

            # View selected data
            shiny::observeEvent(input$viewRemovedButton, {
                output$distPlot <- plotly::renderPlotly({
                    gg1 <- TSAR::screen(raw_data, checklist =
                                      unique(clicked_points$legend_text))
                    plotly::ggplotly(gg1, source = "Plot1")
                })
                output$viewRemovedMessage <- renderPrint({
                    cat("Viewing Selected Curves Only: ",
                        "Click remove if selections are correct. Else refresh",
                        "screening to select more or close-reopen window",
                        "to reselect.")
                })
                output$removedMessage <- renderPrint({
                    message(NULL)
                })
                output$copiedMessage <- renderPrint({
                    message(NULL)
                })
                output$refreshedMessage <- renderPrint({
                    message(NULL)
                })
                output$info <- renderPrint({
                    cat("Selected Curve: ", unique(clicked_points$legend_text))
                })
            })

            # Terminate the Shiny app
            shiny::observeEvent(input$stopButton, {
                stopApp()
            })
        }
    shinyApp(ui = ui, server = server)
}
