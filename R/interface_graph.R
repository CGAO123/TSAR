graphpage <- function(tsar_data) {
    fluidPage(
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
        shiny::checkboxInput("dialogueToggle",
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
            column(width = 2, shiny::uiOutput("hideBtn"))
        ),
        shinyjs::hidden(
            div(
                id = "myPanel",
                h3("Merge Replicate Trials"),
                fluidRow(
                    column(
                        width = 6,
                        shiny::fileInput("file",
                            label = "Upload All Analysis Files",
                            multiple = TRUE
                        )
                    ),
                    column(
                        width = 3,
                        shiny::actionButton("generate", "Merge and Save Data")
                    ),
                    column(
                        width = 3,
                        actionButton(
                            "savetolocal",
                            "Save to R"
                        )
                    ),
                ),
                div(
                    style = "font-size: 11px;",
                    shiny::tableOutput("table")
                ),
                shiny::uiOutput("date_boxes"),
                shiny::actionButton("save_dates", "Save Dates"),
                shiny::verbatimTextOutput("output")
            )
        ),
        br(),
        div(
            id = "bplot",
            class = "sticky-panel",
            shiny::plotOutput("Plot")
        ),
        shinyjs::hidden(
            div(
                class = "sticky-panel",
                id = "alternativeplot",
                plotly::plotlyOutput("altplot")
            )
        ),
        verbatimTextOutput("Plot_Message"),
        h3("Boxplot"),
        shiny::checkboxInput(
            "makeplotly", "Graph Interactive (legend cannot be separate)",
            value = FALSE, width = "600px"
        ),
        fluidRow(
            column(
                width = 2,
                shiny::selectInput("Color",
                    label = "Color:",
                    choices = c("Protein", "Ligand")
                )
            ),
            column(
                width = 2,
                shiny::selectInput("Label",
                    label = "Label:",
                    choices = c("Protein", "Ligand"),
                    selected = "Ligand"
                )
            ),
            column(
                width = 2,
                shiny::selectInput("Legend",
                    label = "Separate legend:",
                    choices = c(TRUE, FALSE), selected = FALSE
                )
            ),
            column(
                width = 3,
                shiny::selectInput("Control",
                    label = "Control Condition",
                    choices = c("NA", condition_IDs(tsar_data))
                )
            ),
            column(
                br(),
                width = 2,
                shiny::actionButton("Boxplot", "Generate Boxplot")
            )
        ),
        h3("Compare Plot"),
        fluidRow(
            column(
                width = 2,
                shiny::selectInput("y_axis",
                    label = "Graph y as:",
                    choices = c("Fluorescence", "RFU"),
                    selected = "RFU"
                )
            ),
            column(
                width = 2,
                shiny::selectInput("show_tm",
                    label = "Show Tm:",
                    choices = c(TRUE, FALSE)
                )
            ),
            column(
                width = 2,
                shiny::selectInput("title_by",
                    label = "Title by:",
                    choices = c("ligand", "protein", "both"),
                    selected = "both"
                )
            ),
            column(
                width = 3,
                shiny::selectInput("Control_s",
                    label = "Control Condition",
                    choices = c(condition_IDs(tsar_data))
                )
            ),
            column(
                br(),
                width = 2,
                shiny::actionButton("Compareplot", "Generate Compare Plots")
            ),
            column(width = 2, uiOutput("plot_select"))
        ),
        h3("Condition Plot"),
        fluidRow(
            column(
                width = 2,
                shiny::selectInput("y_axis_c",
                    label = "Graph y as: ",
                    choices = c("Fluorescence", "RFU"),
                    selected = "RFU"
                )
            ),
            column(
                width = 2,
                shiny::selectInput("show_tm_c",
                    label = "Show Tm: ",
                    choices = c(TRUE, FALSE)
                )
            ),
            column(
                width = 2,
                shiny::selectInput("separate_legend",
                    label = "Separate legend: ",
                    choices = c(TRUE, FALSE), selected = FALSE
                )
            ),
            column(
                width = 3,
                shiny::selectInput("Selected_condition",
                    label = "Select condition: ",
                    choices = c(condition_IDs(tsar_data))
                )
            ),
            column(
                br(),
                width = 2,
                shiny::actionButton("curves", "Graph Selected Curves")
            )
        ),
        fluidRow(
            column(
                width = 2,
                shiny::selectInput("smooth",
                    label = "Smooth curve: ",
                    choices = c(TRUE, FALSE)
                )
            ),
            column(
                width = 2,
                shiny::selectInput("average",
                    label = "Show average: ",
                    choices = c(TRUE, FALSE)
                )
            ),
            column(
                width = 2,
                shiny::numericInput("num",
                    label = "Shift Tm Label: ",
                    value = 7.5
                )
            )
        ),
        h3("Derivative Comparisons"),
        fluidRow(
            column(
                width = 2,
                shiny::selectInput("frame_by",
                    label = "Compare by: ",
                    choices = c("NA", subset(
                        names(tsar_data),
                        !names(tsar_data) %in% c(
                            "Temperature",
                            "Fluorescence",
                            "norm_deriv"
                        )
                    )),
                    selected = "NA"
                )
            ),
            column(
                br(),
                width = 2,
                shiny::actionButton("build_deriv", "Compare Derivatives")
            )
        ),
        hr(),
        h4("helper functions: "),
        fluidRow(
            column(
                width = 4,
                shiny::selectInput("remove_condition",
                    label = "Remove Condition_ID:",
                    multiple = TRUE,
                    choices = condition_IDs(tsar_data)
                )
            ),
            column(
                width = 4,
                shiny::selectInput("remove_well",
                    label = "Remove Well_ID:",
                    multiple = TRUE,
                    choices = well_IDs(tsar_data)
                )
            ),
            column(
                width = 2,
                br(),
                shiny::actionButton("Remove", "Remove Selected")
            ),
            column(
                width = 2,
                br(),
                shiny::actionButton("Restore", "Restore Data")
            )
        ),
        fluidRow(
            column(
                width = 2,
                shiny::actionButton("condition", "List Conditions IDs")
            ),
            column(
                width = 2,
                shiny::actionButton("well", "List Well IDs")
            )
        ),
        br(),
        shiny::verbatimTextOutput("Condition_ID"),
        shiny::verbatimTextOutput("Well_ID"),
        br(),
        fluidRow(
            column(
                width = 4,
                shiny::selectInput("control_tm",
                    label = "List Delta Tm:",
                    choices = c(
                        "N/A",
                        condition_IDs(tsar_data)
                    )
                )
            ),
            column(
                br(),
                width = 2,
                shiny::actionButton("tm", "List Tm")
            ),
        ),
        shiny::tableOutput("tmlist"),
        br(),
        shiny::actionButton("stopButton", "Close Window")
    )
}
