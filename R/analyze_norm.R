#' Analyze Norm
#'
#' The analyze_norm function allows users to process analysis through an UI
#'   interface. Function wraps together all functions with in TSA_analysis
#'   family and read_write_analysis family.
#'
#' @importFrom openxlsx read.xlsx
#' @import shiny
#' @import rhandsontable
#' @import dplyr
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
#' # analyze_norm(raw_data)
#'
analyze_norm <- function(raw_data) {
    ui <- fluidPage(
        tags$head(tags$style(HTML("
            .h3-style {
              font-family: sans-serif;
              font-size: 26px;
              font-weight: 500;
            }
          "))),
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
        checkboxInput("dialogueToggle",
            "Hide All Hints and Messages",
            value = FALSE
        ),
        fluidRow(
            class = "sticky-panel",
            column(
                width = 6,
                br(),
                div(
                    class = "h3-style",
                    textOutput("table_title")
                ),
                div(
                    style = "font-size: 11px;",
                    dataTableOutput("table")
                )
            ),
            column(
                width = 6,
                h3("Fit Model"),
                plotOutput("Plot")
            )
        ),
        h3("Test fit"),
        fluidRow(
            column(
                width = 2,
                selectInput("well",
                    label = "Select Well:",
                    choices = c(unique(raw_data$Well.Position))
                )
            ),
            column(
                width = 3,
                numericInput("num",
                    label = "Enter y column number:",
                    value = 5
                )
            ),
            column(
                br(),
                width = 2,
                actionButton("model_fit", "View Model Fit")
            )
        ),
        h3("Derivative Analysis"),
        fluidRow(
            column(
                width = 3,
                selectInput("keep",
                    label = "Keep all data",
                    choices = c(TRUE, FALSE), selected = TRUE
                )
            ),
            column(
                width = 3,
                selectInput("fit",
                    label = "Return fit summary: ",
                    choices = c(TRUE, FALSE), selected = FALSE
                )
            ),
            column(
                width = 3,
                selectInput("smooth",
                    label = "Raw data smoothed?",
                    choices = c(TRUE, FALSE), selected = TRUE
                )
            )
        ),
        fluidRow(
            column(
                width = 12,
                checkboxGroupInput("checkGroup",
                    label = "Keep variables:",
                    choices = c(names(raw_data)),
                    inline = TRUE,
                    selected = c(
                        "Well.Position",
                        "Temperature",
                        "Fluorescence",
                        "Normalized"
                    )
                )
            ),
            column(
                width = 3,
                numericInput("num_a",
                    label = "Enter y column number:", value = 5
                )
            ),
            column(
                br(),
                width = 4,
                actionButton("analyze", "Analyze all Wells")
            )
        ),
        h3("Input Well Conditions"),
        fluidRow(
            column(
                width = 3,
                fileInput("excelFile", "Upload Well Information.xlsx")
            ),
            column(
                width = 2, br(),
                actionButton("previewBtn", "Preview")
            ),
            column(
                width = 1, br(),
                actionButton("hideBtn", "Hide")
            ),
            column(
                width = 2, br(),
                actionButton("blank", "Manual Input")
            ),
            column(
                width = 2, br(),
                actionButton("saveBtn", "Save Changes")
            ),
            column(
                width = 1, br(),
                actionButton("join", "Set Conditions")
            ),
        ),
        div(
            style = "font-size: 11px;",
            rHandsontableOutput("excelTable")
        ),
        h3("Save Data Locally"),
        fluidRow(
            column(
                width = 5,
                radioButtons("radio",
                    label = "Choose dataset",
                    choices = list(
                        "Only Tm" = 0,
                        "Fluorescence and Model" = 1,
                        "Both" = 2
                    ),
                    inline = TRUE,
                    selected = 2
                )
            ),
            column(
                width = 2,
                radioButtons("file_type",
                    label = "file type: ",
                    choices = list("txt", "csv"),
                    inline = TRUE,
                    selected = "csv"
                )
            ),
            column(
                width = 3,
                selectInput("withCondition",
                    label = "With Conditions: ",
                    choices = c(TRUE, FALSE), selected = FALSE
                )
            )
        ),
        fluidRow(
            column(
                width = 5,
                textInput("name",
                    label = "Enter Name: ",
                    value = paste(deparse(substitute(raw_data)),
                        Sys.Date(),
                        sep = "_"
                    )
                )
            ),
            column(
                br(),
                width = 2,
                actionButton("write", "Save File")
            ),
            column(
                br(),
                width = 2,
                actionButton("preview", "Preview Output")
            )
        ),
        br(),
        actionButton("stopButton", "Close Window")
    )

    server <- function(input, output) {
        well_option <- "A01"
        y_col_option <- 5
        keep_option <- TRUE
        fit_option <- FALSE
        smooth_option <- TRUE
        y_col_option_a <- 5
        selection_option <- c(
            "Well.Position", "Temperature",
            "Fluorescence", "Normalized"
        )
        analysis <- c()
        write_option <- 2
        file_type_option <- "txt"
        analyzed_done <- FALSE
        name_option <- paste(deparse(substitute(raw_data)),
            Sys.Date(),
            sep = "_"
        )
        joined <- c()
        imported <- FALSE
        withCondition_option <- TRUE
        output_data <- c()
        well_info <- data.frame()
        inFile <- NULL
        showModalFlag <- FALSE

        shiny::observeEvent(input$dialogueToggle, {
            showModalFlag <<- input$dialogueToggle
        })

        output$table <- renderDataTable(
            {
                data.frame(raw_data)
            },
            options = list(pageLength = 7)
        )
        output$table_title <- renderText({
            "Current Data: raw_data"
        })

        shiny::observeEvent(input$well, {
            well_option <<- as.character(input$well)
        })
        shiny::observeEvent(input$num, {
            y_col_option <<- input$num
        })

        shiny::observeEvent(input$model_fit, {
            test <- filter(raw_data, raw_data$Well.Position == well_option)
            test <- normalize(test, fluo = y_col_option)
            model <- model_gam(test, x = test$Temperature, y = test$Normalized)
            test <- model_fit(test, model = model)
            gg <- view_model(test)
            output$Plot <- renderPlot({
                gg + theme(aspect.ratio = 0.7, legend.position = "bottom") +
                    guides(color = guide_legend(nrow = 2, byrow = TRUE))
            })
        })

        shiny::observeEvent(input$keep, {
            keep_option <<- input$keep
        })
        shiny::observeEvent(input$fit, {
            fit_option <<- input$fit
        })
        shiny::observeEvent(input$smooth, {
            smooth_option <<- input$smooth
        })
        shiny::observeEvent(input$num_a, {
            y_col_option_a <<- input$num_a
        })

        shiny::observe({
            selection_option <<- input$checkGroup
        })

        shiny::observeEvent(input$analyze, {
            analysis <<- gam_analysis(raw_data,
                fluo = y_col_option_a,
                keep = keep_option,
                fit = fit_option,
                smoothed = smooth_option,
                selections = c(
                    selection_option,
                    "Normalized"
                )
            )
            output$table <- renderDataTable(
                {
                    data.frame(read_tsar(analysis, code = 2))
                },
                options = list(pageLength = 7)
            )
            output$table_title <- renderText({
                "Analyzed Data: norm_data"
            })
            analyzed_done <<- TRUE
            if (!showModalFlag) {
                showModal(modalDialog(
                    title = "Analysis is complete",
                    "Proceede to inputting conditions and saving data
                    before closing window."
                ))
            }
            output_data <<- read_tsar(analysis, code = write_option)
        })

        shiny::observe({
            write_option <<- input$radio
            file_type_option <<- input$file_type
            name_option <<- input$name
        })
        shiny::observeEvent(input$withCondition, {
            withCondition_option <<- input$withCondition
            if (imported == TRUE && withCondition_option == TRUE) {
                output_data <<- join_well_info(
                    file_path = inFile$datapath,
                    file = well_info,
                    analysis_file =
                        read_tsar(analysis,
                            code = write_option
                        ),
                    type = "by_template"
                )
            } else {
                if (analyzed_done == FALSE) {
                    output_data <- c()
                } else {
                    output_data <<- read_tsar(analysis, code = write_option)
                }
            }
        })

        shiny::observeEvent(input$write, {
            if (analyzed_done == FALSE) {
                if (!showModalFlag) {
                    showModal(modalDialog(
                        title = "Analysis is Incomplete!",
                        "Please analyze all data before saving."
                    ))
                }
            } else {
                write_tsar(output_data,
                    name = name_option,
                    file = file_type_option
                )
                if (!showModalFlag) {
                    showModal(modalDialog(
                        title = "Successfully Saved",
                        "Check your working directory for data file."
                    ))
                }
            }
        })
        shiny::observeEvent(input$preview, {
            if (analyzed_done == FALSE) {
                if (!showModalFlag) {
                    showModal(modalDialog(
                        title = "Analysis is Incomplete!",
                        "Please analyze all data before saving."
                    ))
                }
            } else {
                if (imported == FALSE && withCondition_option == TRUE) {
                    if (!showModalFlag) {
                        showModal(modalDialog(
                            title = "Conditions are not specified!",
                            "Please input condition information first
                            or set 'With Conditions' as fasle."
                        ))
                    }
                } else {
                    if (!showModalFlag) {
                        showModal(modalDialog(
                            title = "Hint :)",
                            "If you are saving data for tsar_graphing
                            functions, make sure to save all fluorescence
                            data (i.e. 'both') to output compare plots and
                            conditions plot."
                        ))
                    }
                    if (imported == TRUE && withCondition_option == TRUE) {
                        output_data <<- join_well_info(
                            file_path = inFile$datapath,
                            file = well_info,
                            analysis_file = read_tsar(analysis,
                                code = write_option
                            ),
                            type = "by_template"
                        )
                    } else {
                        if (analyzed_done == FALSE) {
                            output_data <- c()
                        } else {
                            output_data <<- read_tsar(analysis,
                                code = write_option
                            )
                        }
                    }
                    output$table <- renderDataTable(
                        {
                            data.frame(output_data)
                        },
                        options = list(pageLength = 7)
                    )
                    output$table_title <- renderText({
                        "Output Data: tsar_data"
                    })
                }
            }
        })

        data <- reactive({
            req(input$excelFile)
            inFile <<- input$excelFile
            imported <<- TRUE
            if (!is.null(inFile)) {
                read.xlsx(inFile$datapath, sheet = 1)
            }
        })
        shiny::observeEvent(input$blank, {
            output$excelTable <- renderRHandsontable({
                rhandsontable(data.frame(head(Well_Information_Template, 9)))
            })
        })

        shiny::observeEvent(input$join, {
            if ((imported == FALSE) || (analyzed_done == FALSE)) {
                if ((analyzed_done == FALSE) && (!showModalFlag)) {
                    showModal(modalDialog(
                        title = "Analysis is Incomplete!",
                        "Please analyze all data before saving."
                    ))
                } else {
                    if (!showModalFlag) {
                        showModal(modalDialog(
                            title = "Conditions are not specified!",
                            "Please input condition information first
                            or set 'With Conditions' as fasle."
                        ))
                    }
                }
            } else {
                joined <<- join_well_info(
                    file_path = inFile$datapath,
                    file = well_info,
                    analysis_file =
                        read_tsar(analysis,
                            code = write_option
                        ),
                    type = "by_template"
                )
                output$table <- renderDataTable(
                    {
                        data.frame(joined)
                    },
                    options = list(pageLength = 7)
                )
                if (!showModalFlag) {
                    showModal(modalDialog(
                        title = "Conditions Saved",
                        "Proceede to preview output and save data."
                    ))
                }
                output$table_title <- renderText({
                    "Complete Data: tsar_data"
                })
            }
        })
        shiny::observeEvent(input$previewBtn, {
            output$excelTable <- renderRHandsontable({
                if (length(well_info) != 0) {
                    rhandsontable(data.frame(head(well_info, 9)))
                } else if (!is.null(data())) {
                    rhandsontable(data.frame(head(data(), 9)))
                }
            })
        })
        shiny::observeEvent(input$hideBtn, {
            output$excelTable <- renderRHandsontable({
                NULL
            })
        })
        shiny::observeEvent(input$saveBtn, {
            imported <<- TRUE
            well_info <<- hot_to_r(input$excelTable)
            if (!showModalFlag) {
                showModal(modalDialog(
                    title = "Success",
                    "Changes saved successfully."
                ))
            }
        })

        shiny::observeEvent(input$stopButton, {
            stopApp()
        })
    }
    shinyApp(ui = ui, server = server)
}
