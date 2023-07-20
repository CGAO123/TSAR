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
        hr(),
        fluidRow(
            column(
                width = 6,
                h4("helper functions: "),
                fluidRow(
                    column(
                        width = 5,
                        actionButton("condition", "List Conditions IDs")
                    ),
                    column(
                        width = 4,
                        actionButton("well", "List Well IDs")
                    ),
                    column(
                        width = 2,
                        actionButton("tm", "List Tm")
                    ),
                )
            ),
            column(
                width = 4,
                br(),
                selectInput("control_tm",
                    label = "List Delta Tm:",
                    choices = c(
                        "Select Control: ",
                        condition_IDs(tsar_data)
                    )
                )
            )
        ),
        verbatimTextOutput("Condition_ID"),
        verbatimTextOutput("Well_ID"),
        tableOutput("tmlist"),
        br(),
        actionButton("stopButton", "Close Window")
    )
}
