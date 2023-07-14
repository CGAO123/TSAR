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
#' # myApp <- weed_raw(raw_data, checklist = c("A11", "A12"))
#' # shiny::runApp(myApp)
#'
weed_raw <- function(raw_data,
                     checkrange = NULL,
                     checklist = NULL) {
    ui <- fluidPage(
        useShinyjs(),
        tags$style(
            HTML(
                "
              .sticky-panel {
                position: sticky;
                top: 0;
                z-index: 100;
                background-color: white;
              }
              "
            )
        ),
        fluidRow(
            column(
                width = 4,
                checkboxInput("dialogueToggle",
                    "Hide All Hints and Messages",
                    value = FALSE
                )
            ),
            column(
                width = 4,
                actionButton(
                    "clear",
                    "Clear all Selected"
                )
            )
        ),
        fluidRow(
            column(width = 4, h3("Current Raw Dataset: ")),
            h3(dataName <- deparse(substitute(raw_data)))
        ),
        div(
            class = "sticky-panel",
            plotlyOutput("distPlot")
        ),
        verbatimTextOutput("info"),
        fluidRow(
            column(width = 3, h4("Select by Grid")),
            column(
                width = 2,
                h5(" "),
                materialSwitch("toggle_button", "Show Grid",
                    value = TRUE, right = TRUE
                )
            )
        ),
        div(
            id = "grid-panel",
            style = "display: none;",
            fluidRow(
                column(
                    width = 12,
                    div(
                        id = "grid-container",
                        style = "display: grid; grid-template-columns:
                                repeat(12, 50px); grid-gap: 0px; grid-",
                        lapply(1:96, function(i) {
                            row <- ceiling(i / 12)
                            col <- i %% 12
                            if (col == 0) col <- 12
                            col_label <- sprintf("%02d", col)
                            div(
                                id = paste0("cell-", i),
                                class = "grid-cell",
                                style = "background-color: white;
                                       height: 30px; cursor: pointer;
                                       display: flex; align-items:
                                       center; justify-content: center;
                                       font-size: 12px;",
                                paste0(LETTERS[row], col_label)
                            )
                        })
                    )
                )
            )
        ),
        verbatimTextOutput("highlighted_list"),
        h4("Copy Selections"),
        fluidRow(
            column(
                width = 2,
                actionButton("myButton", "Copy Well IDs")
            ),
            column(
                width = 4,
                actionButton(
                    "removeCall",
                    "Copy in remove_raw() Call"
                )
            ),
        ),
        br(),
        fluidRow(
            column(
                width = 6,
                h4("Refresh Plot"),
                fluidRow(
                    column(
                        width = 4,
                        actionButton(
                            "viewRemovedButton",
                            "View Selected"
                        )
                    ),
                    column(
                        width = 2,
                        actionButton(
                            "refreshButton",
                            "Refresh Screening"
                        )
                    )
                )
            ),
            column(
                width = 5,
                h4("Edit Data"),
                fluidRow(
                    column(
                        width = 2,
                        actionButton(
                            "removeButton",
                            "Remove Selected"
                        )
                    )
                )
            )
        ),
        br(),
        actionButton("stopButton", "Close Window"),
    )
    server <- function(input, output) {
        clicked_points_legend_text <- NULL
        clicked_points <- reactiveValues(legend_text = NULL)
        showModalFlag <- FALSE
        highlighted_cells <- reactiveVal(NULL)
        unhighlighted_cells <- reactiveVal(NULL)
        gg1 <- NULL


        shiny::observe({
            for (i in 1:96) {
                shinyjs::runjs(
                    sprintf("$('#cell-%d').click(function() {
                if ($(this).css('background-color') === 'rgb(255, 255, 255)') {
                $(this).css('background-color', 'lightgrey');
                var cellId = $(this).attr('id');
                var label = $(this).text().trim();
                Shiny.onInputChange('highlighted_cells',
                {id: cellId, label: label});
                } else {
                $(this).css('background-color', 'white');
                var cellId = $(this).attr('id');
                var label = $(this).text().trim();
                Shiny.onInputChange('highlighted_cells', null);
                Shiny.onInputChange('unhighlighted_cells',
                {id: cellId, label: label});
                }
                });", i)
                )
            }
        })

        shiny::observeEvent(input$clear, {
            clicked_points_legend_text <- NULL
            clicked_points <- reactiveValues(legend_text = NULL)
            highlighted_cells <- reactiveVal(NULL)
            unhighlighted_cells <- reactiveVal(NULL)
            output$highlighted_list <- renderPrint({
                cat("Selected by Grid: ", highlighted_cells())
            })
            output$info <- renderPrint({
                cat("Selected Curve: ", clicked_points$legend_text)
            })
        })

        shiny::observeEvent(input$unhighlighted_cells, {
            cell_id <- input$unhighlighted_cells$id
            cell_label <- input$unhighlighted_cells$label
            if (cell_label %in% unlist(highlighted_cells())) {
                current_values <- highlighted_cells()
                updated_values <- current_values[current_values
                != cell_label]
                highlighted_cells(updated_values)
                output$highlighted_list <- renderPrint({
                    cat("Selected by Grid: ", highlighted_cells())
                })
            }
        })

        shiny::observeEvent(input$highlighted_cells, {
            if (!is.null(input$highlighted_cells)) {
                cell_id <- input$highlighted_cells$id
                cell_label <- input$highlighted_cells$label
                if (!is.null(cell_id)) {
                    highlighted_cells(c(
                        highlighted_cells(),
                        cell_label
                    ))
                    output$highlighted_list <- renderPrint({
                        cat("Selected by Grid: ", highlighted_cells())
                    })
                }
            } else {
                if (cell_label %in% unlist(highlighted_cells())) {
                    current_values <- highlighted_cells()
                    updated_values <- current_values[current_values
                    != cell_label]
                    highlighted_cells(updated_values)
                    output$highlighted_list <- renderPrint({
                        cat("Selected by Grid: ", highlighted_cells())
                    })
                }
            }
        })



        shiny::observeEvent(input$toggle_button, {
            shinyjs::toggle("grid-panel")
        })

        shiny::observeEvent(input$dialogueToggle, {
            showModalFlag <<- input$dialogueToggle
        })

        output$distPlot <- plotly::renderPlotly({
            gg1 <<- TSAR::screen(raw_data,
                checkrange = checkrange,
                checklist = checklist
            )
            plotly::ggplotly(gg1, source = "Plot1")
        })

        shiny::observeEvent(event_data("plotly_click",
            source = "Plot1"
        ), {
            d <- event_data("plotly_click", source = "Plot1")
            legend_text <- gg1$data$Well.Position[gg1$data$Fluorescence
            == d$y]

            if ((legend_text %in% clicked_points_legend_text) ||
                (legend_text %in% clicked_points$legend_text)) {
                clicked_points_legend_text <<-
                    clicked_points_legend_text[names(clicked_points_legend_text)
                    != legend_text]
                clicked_points$legend_text <- clicked_points$legend_text[
                    clicked_points$legend_text != legend_text
                ]
            } else {
                clicked_points_legend_text <<- c(
                    clicked_points_legend_text,
                    legend_text
                )
                clicked_points$legend_text <- c(
                    clicked_points$legend_text,
                    legend_text
                )
            }
        })

        output$info <- renderPrint({
            cat("Selected Curve: ", clicked_points$legend_text)
        })

        shiny::observeEvent(input$myButton, {
            clicked_points <- gsub(
                ", ", "','",
                toString(unique(
                    c(
                        clicked_points_legend_text,
                        highlighted_cells()
                    )
                ))
            )
            tobecopied <- paste("'", clicked_points, "'",
                sep = ""
            )
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
            if (!showModalFlag) {
                showModal(modalDialog(
                    title = "Successfully Copied",
                    "Review using screen() and remove data with caution."
                ))
            }
        })

        shiny::observeEvent(input$removeCall, {
            clicked_points <- gsub(
                ", ", "','",
                toString(unique(
                    c(
                        clicked_points_legend_text,
                        highlighted_cells()
                    )
                ))
            )
            tobecopied <- paste("remove_raw(",
                dataName,
                ", removelist = c('", clicked_points, "'))",
                sep = ""
            )
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
            if (!showModalFlag) {
                showModal(modalDialog(
                    title = "Successfully Copied",
                    "Paste function call to script and run directly to
                        remove selected wells."
                ))
            }
        })

        # Remove selected data
        shiny::observeEvent(input$removeButton, {
            raw_data <<- remove_raw(raw_data,
                removelist = c(
                    clicked_points_legend_text,
                    highlighted_cells()
                )
            )
            if (!showModalFlag) {
                showModal(modalDialog(
                    title = "Successfully Removed",
                    "Review new data with screen() in console or
                        click 'Refresh Screening' to view change within window."
                ))
            }
        })

        # Remove selected data
        shiny::observeEvent(input$refreshButton, {
            output$distPlot <- plotly::renderPlotly({
                gg1 <- TSAR::screen(raw_data,
                    checkrange = checkrange,
                    checklist = checklist
                )
                plotly::ggplotly(gg1, source = "Plot1")
            })
            if (!showModalFlag) {
                showModal(modalDialog(
                    title = "Successfully Refreshed",
                    "All edits to dataframe are temporary.
                        Copy wells and call function remove_raw( ) in console
                        or script to store change permanently"
                ))
            }
            output$info <- renderPrint({
                cat("Selected Curve: ", unique(clicked_points$legend_text))
            })
        })

        # View selected data
        shiny::observeEvent(input$viewRemovedButton, {
            output$distPlot <- plotly::renderPlotly({
                gg1 <- TSAR::screen(raw_data,
                    checklist =
                        unique(c(
                            clicked_points$legend_text,
                            highlighted_cells()
                        ))
                )
                plotly::ggplotly(gg1, source = "Plot1")
            })
            if (!showModalFlag) {
                if ((length(clicked_points_legend_text) == 0) &&
                    (is.null(highlighted_cells()))) {
                    showModal(modalDialog(
                        "Error: No valid Well variable was found.
                        Make sure it is named 'Well.Position' or 'Well'"
                    ))
                } else {
                    showModal(modalDialog(
                        title = "Viewing Selected Curves Only",
                        "Click remove if selections are correct. Else
                            refresh screening to select more or unselect curve."
                    ))
                }
            }
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
