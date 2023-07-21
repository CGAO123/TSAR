analyzepage <- function(raw_data, data_name) {
    fluidPage(
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
                    label = "Keep variables: (keep x and y)",
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
                    value = paste(data_name,
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
}
