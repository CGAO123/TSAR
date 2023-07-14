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
#' graph_tsar()
#'
graph_tsar <- function(tsar_data = data.frame()) {
    ui <- fluidPage(
        useShinyjs(),
        tags$style(
            HTML(
                ".sticky-panel {
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
            column(
                width = 3,
                actionButton(
                    "toggleButton",
                    "Upload and Merge Data"
                )
            ),
            column(width = 2, uiOutput("hideBtn"))
        ),
        shinyjs::hidden(
            div(
                id = "myPanel",
                h3("Merge Replicate Trials"),
                fluidRow(
                    column(
                        width = 6,
                        fileInput("file",
                            label = "Upload All Analysis Files",
                            multiple = TRUE
                        )
                    ),
                    column(
                        width = 6,
                        actionButton("generate", "Merge and Save Data")
                    )
                ),
                div(
                    style = "font-size: 11px;",
                    tableOutput("table")
                ),
                uiOutput("date_boxes"),
                actionButton("save_dates", "Save Dates"),
                verbatimTextOutput("output")
            )
        ),
        br(),
        div(
            class = "sticky-panel",
            plotOutput("Plot")
        ),
        verbatimTextOutput("Plot_Message"),
        h3("Boxplot"),
        fluidRow(
            column(
                width = 2,
                selectInput("Color",
                    label = "Color:",
                    choices = c("Protein", "Ligand")
                )
            ),
            column(
                width = 2,
                selectInput("Label",
                    label = "Label:",
                    choices = c("Protein", "Ligand"),
                    selected = "Ligand"
                )
            ),
            column(
                width = 2,
                selectInput("Legend",
                    label = "Separate legend:",
                    choices = c(TRUE, FALSE), selected = FALSE
                )
            ),
            column(
                width = 3,
                selectInput("Control",
                    label = "Control Condition",
                    choices = c("NA", condition_IDs(tsar_data))
                )
            ),
            column(
                br(),
                width = 2,
                actionButton("Boxplot", "Generate Boxplot")
            )
        ),
        h3("Compare Plot"),
        fluidRow(
            column(
                width = 2,
                selectInput("y_axis",
                    label = "Graph y as:",
                    choices = c("Fluorescence", "RFU"),
                    selected = "RFU"
                )
            ),
            column(
                width = 2,
                selectInput("show_tm",
                    label = "Show Tm:",
                    choices = c(TRUE, FALSE)
                )
            ),
            column(
                width = 2,
                selectInput("title_by",
                    label = "Title by:",
                    choices = c("ligand", "protein", "both"),
                    selected = "both"
                )
            ),
            column(
                width = 3,
                selectInput("Control_s",
                    label = "Control Condition",
                    choices = c(condition_IDs(tsar_data))
                )
            ),
            column(
                br(),
                width = 2,
                actionButton("Compareplot", "Generate Compare Plots")
            ),
            column(width = 2, uiOutput("plot_select"))
        ),
        h3("Condition Plot"),
        fluidRow(
            column(
                width = 2,
                selectInput("y_axis_c",
                    label = "Graph y as: ",
                    choices = c("Fluorescence", "RFU"),
                    selected = "RFU"
                )
            ),
            column(
                width = 2,
                selectInput("show_tm_c",
                    label = "Show Tm: ",
                    choices = c(TRUE, FALSE)
                )
            ),
            column(
                width = 2,
                selectInput("separate_legend",
                    label = "Separate legend: ",
                    choices = c(TRUE, FALSE), selected = FALSE
                )
            ),
            column(
                width = 3,
                selectInput("Selected_condition",
                    label = "Select condition: ",
                    choices = c(condition_IDs(tsar_data))
                )
            ),
            column(
                br(),
                width = 2,
                actionButton("curves", "Graph Selected Curves")
            )
        ),
        fluidRow(
            column(
                width = 2,
                selectInput("smooth",
                    label = "Smooth curve: ",
                    choices = c(TRUE, FALSE)
                )
            ),
            column(
                width = 2,
                selectInput("average",
                    label = "Show average: ",
                    choices = c(TRUE, FALSE)
                )
            ),
            column(
                width = 2,
                numericInput("num", label = "Shift Tm Label: ", value = 7.5)
            )
        ),
        br(),
        h4("helper functions: "),
        fluidRow(
            column(
                width = 2,
                actionButton("condition", "List Conditions IDs")
            ),
            column(
                width = 2,
                actionButton("well", "List Well IDs")
            ),
        ),
        verbatimTextOutput("Condition_ID"),
        verbatimTextOutput("Well_ID"),
        br(),
        actionButton("stopButton", "Close Window")
    )

    server <- function(input, output, session) {
        color_option <- "Protein"
        label_option <- "Ligand"
        legend_option <- FALSE
        control_option_b <- NA
        control_option_s <- NA
        y_axis_option <- "RFU"
        show_tm_option <- TRUE
        title_by_option <- "both"
        selected_curves <- c()
        y_axis_c_option <- "RFU"
        smooth_option <- TRUE
        average_option <- TRUE
        show_tm_c_option <- TRUE
        nudge <- 7.5
        filepath <- c()
        namelist <- c()
        datelist <- c()
        options(shiny.maxRequestSize = 30 * 1024^2)
        dated <- FALSE
        compare <- NULL
        separate_legend_option <- FALSE
        plot_selected_option <- NULL
        showModalFlag <- FALSE

        shiny::observeEvent(input$dialogueToggle, {
            showModalFlag <<- input$dialogueToggle
        })

        shiny::observeEvent(input$hideBtn, {
            shinyjs::hide(id = "myPanel")
            output$hideBtn <- renderUI({
                NULL
            })
        })


        shiny::observeEvent(input$toggleButton, {
            shinyjs::show(id = "myPanel")
            output$hideBtn <- renderUI({
                actionBttn(
                    inputId = "hideBtn", label = "hide",
                    style = "minimal", color = "success", size = "xs"
                )
            })
        })

        observe({
            filepath <<- input$file$datapath
            namelist <<- input$file$name
            if (length(tsar_data) == 0) {
                if (!showModalFlag) {
                    showModal(modalDialog(
                        title = "No data input :(",
                        "No data input: Please upload analysis files to merge
                        or close window \nand call function with data
                        included as parameter. e.g. graph_tsar(tsar_data)"
                    ))
                }
            }
        })


        observeEvent(input$file, {
            output$date_boxes <- renderUI({
                date_boxes <- lapply(namelist, function(i) {
                    dateInput(
                        inputId = paste0("Date for file ", i),
                        label = paste0("Date for file ", i)
                    )
                })
                do.call(tagList, date_boxes)
            })
        })

        observeEvent(input$save_dates, {
            # Save the input values of the date boxes as a list of strings
            saved_dates <- sapply(namelist, function(i) {
                as.character(input[[paste0("Date for file ", i)]])
            })
            # Print the saved dates
            datelist <<- saved_dates
            if (length(datelist) > 0) {
                if (!showModalFlag) {
                    showModal(modalDialog(
                        title = "Dates saved",
                        "Confirm in the textbox below if saved dates are
                        correct and proceede to merging data."
                    ))
                    output$output <- renderPrint({
                        saved_dates
                    })
                    dated <<- TRUE
                }
            } else {
                if (!showModalFlag) {
                    showModal(modalDialog(
                        title = ":(",
                        "Please upload analysis files first before
                        setting dates."
                    ))
                }
            }
        })

        observeEvent(input$generate, {
            if ((dated == FALSE) && (!showModalFlag)) {
                showModal(modalDialog(
                    title = "Dates are not saved",
                    "please review and save dates of experiment!"
                ))
            } else {
                tsar_data <<- merge_norm(
                    data = filepath,
                    name = namelist,
                    date = datelist
                )
                tsar_data <<- na.omit(tsar_data)
                output$table <- renderTable({
                    data.frame(head(tsar_data))
                })
                output$Plot_Message <- renderPrint({
                    cat(
                        "Select one of the following graph options and",
                        "click generate.",
                        "\nGraphing takes few seconds to load, please wait :)"
                    )
                })

                updateSelectInput(session, "Control",
                    label = "Control Condition",
                    choices = c("NA", condition_IDs(tsar_data))
                )
                updateSelectInput(session, "Control_s",
                    label = "Control Condition",
                    choices = c(condition_IDs(tsar_data))
                )
                updateSelectInput(session, "Selected_condition",
                    label = "Select condition: ",
                    choices = c(condition_IDs(tsar_data))
                )
            }
        })

        observeEvent(input$Color, {
            color_option <<- as.character(input$Color)
        })
        observeEvent(input$Label, {
            label_option <<- as.character(input$Label)
        })
        observeEvent(input$Legend, {
            legend_option <<- as.character(input$Legend)
        })
        observeEvent(input$Control, {
            if (input$Control == "NA") {
                control_option_b <<- NA
            } else {
                control_option_b <<- as.character(input$Control)
            }
        })

        observeEvent(input$Boxplot, {
            if (length(tsar_data) == 0) {
                if (!showModalFlag) {
                    showModal(modalDialog(
                        title = "No data input",
                        "No data input: Please upload analysis files to merge
                        or close window \nand call function with data
                        included as parameter. e.g. graph_tsar(tsar_data)"
                    ))
                }
            } else {
                box <- TSA_boxplot(tsar_data,
                    color_by = color_option,
                    label_by = label_option,
                    separate_legend = legend_option,
                    control_condition = control_option_b
                )
                output$Plot <- renderPlot({
                    if (legend_option == FALSE) {
                        box + theme(text = element_text(size = 18))
                    } else {
                        box[[1]] <- box[[1]] + theme(
                            text = element_text(size = 18)
                        )
                        ggarrange(
                            plotlist = box,
                            font.label = list(size = 20)
                        )
                    }
                })
            }
        })

        observeEvent(input$y_axis, {
            y_axis_option <<- as.character(input$y_axis)
        })
        observeEvent(input$show_tm, {
            show_tm_option <<- input$show_tm
        })
        observeEvent(input$title_by, {
            title_by_option <<- as.character(input$title_by)
        })
        observeEvent(input$Control_s, {
            control_option_s <<- as.character(input$Control_s)
        })

        observeEvent(input$Compareplot, {
            if (length(tsar_data) == 0) {
                if (!showModalFlag) {
                    showModal(modalDialog(
                        title = "No data input",
                        "No data input: Please upload analysis files to merge
                        or close window \nand call function with data
                        included as parameter. e.g. graph_tsar(tsar_data)"
                    ))
                }
            } else {
                compare <<- tsa_compare_plot(tsar_data,
                    y = y_axis_option,
                    title_by = title_by_option,
                    show_Tm = show_tm_option,
                    control_condition =
                        control_option_s
                )

                output$Plot <- renderPlot({
                    ggarrange(plotlist = compare, common.legend = TRUE)
                })
                output$plot_select <- renderUI({
                    selectInput("plot_selected",
                        label = "View Only:",
                        choices = c("Select Option..", names(compare))
                    )
                })
            }
        })

        observeEvent(input$plot_selected, {
            plot_selected_option <<- input$plot_selected
            if (plot_selected_option != "Select Option..") {
                output$Plot <- renderPlot({
                    compare[[plot_selected_option]]
                })
            }
        })

        observeEvent(input$Selected_condition, {
            selected_curves <<- input$Selected_condition
        })
        observeEvent(input$y_axis_c, {
            y_axis_c_option <<- as.character(input$y_axis_c)
        })
        observeEvent(input$smooth, {
            smooth_option <<- input$smooth
        })
        observeEvent(input$average, {
            average_option <<- input$average
        })
        observeEvent(input$show_tm_c, {
            show_tm_c_option <<- input$show_tm_c
        })
        observeEvent(input$separate_legend, {
            separate_legend_option <<- input$separate_legend
        })
        observeEvent(input$num, {
            nudge <<- input$num
        })

        observeEvent(input$curves, {
            if (length(tsar_data) == 0) {
                if (!showModalFlag) {
                    showModal(modalDialog(
                        title = "No data input",
                        "No data input: Please upload analysis files to merge
                        or close window \nand call function with data
                        included as parameter. e.g. graph_tsar(tsar_data)"
                    ))
                }
            } else {
                selected_curves <- filter(
                    tsar_data,
                    tsar_data$condition_ID ==
                        selected_curves
                )
                curve_graph <- TSA_wells_plot(selected_curves,
                    show_Tm = show_tm_c_option,
                    y = y_axis_c_option,
                    show_average = average_option,
                    smooth = smooth_option,
                    Tm_label_nudge = nudge,
                    separate_legend =
                        separate_legend_option
                )
                output$Plot <- renderPlot({
                    if (separate_legend_option == FALSE) {
                        curve_graph + theme(text = element_text(size = 18))
                    } else {
                        curve_graph[[1]] <- curve_graph[[1]] +
                            theme(text = element_text(size = 18))
                        ggarrange(plotlist = curve_graph)
                    }
                })
            }
        })

        observeEvent(input$condition, {
            output$Condition_ID <- renderPrint({
                condition_IDs(tsar_data)
            })
        })

        observeEvent(input$well, {
            output$Well_ID <- renderPrint({
                well_IDs(tsar_data)
            })
        })

        output$Plot_Message <- renderPrint({
            cat(
                "Select one of the following graph options and click generate.",
                "\nGraphing takes few seconds to load, please wait :)"
            )
        })

        observeEvent(input$stopButton, {
            stopApp()
        })
    }

    graph <- shinyApp(ui = ui, server = server)
}
