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
        shiny::checkboxInput("dialogueToggle",
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
                    shiny::textOutput("table_title")
                ),
                div(
                    style = "font-size: 10px;",
                    shiny::dataTableOutput("table")
                )
            ),
            column(
                width = 6,
                h3("Fit Model"),
                plotly::plotlyOutput("Plot")
            )
        ),
        h3("Test fit"),
        fluidRow(
            column(
                width = 2,
                shiny::selectInput("well",
                    label = "Select Well:",
                    choices = c(unique(raw_data$Well.Position))
                )
            ),
            column(
                width = 3,
                shiny::numericInput("num",
                    label = "Enter y column number:",
                    value = 5
                )
            ),
            column(
                br(),
                width = 2,
                shiny::actionButton("model_fit", "View Model Fit")
            )
        ),
        h3("Derivative Analysis"),
        fluidRow(
            column(
                width = 3,
                shiny::selectInput("keep",
                    label = "Keep all data",
                    choices = c(TRUE, FALSE), selected = TRUE
                )
            ),
            column(
                width = 3,
                shiny::selectInput("fit",
                    label = "Return fit summary: ",
                    choices = c(TRUE, FALSE), selected = FALSE
                )
            ),
            column(
                width = 3,
                shiny::selectInput("smooth",
                    label = "Raw data smoothed?",
                    choices = c(TRUE, FALSE), selected = TRUE
                )
            )
        ),
        fluidRow(
            column(
                width = 12,
                shiny::checkboxGroupInput("checkGroup",
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
                shiny::numericInput("num_a",
                    label = "Enter y column number:", value = 5
                )
            ),
            column(
                br(),
                width = 4,
                shiny::actionButton("analyze", "Analyze all Wells")
            )
        ),
        h3("Input Well Conditions"),
        fluidRow(
            column(
                width = 3,
                shiny::fileInput("excelFile", "Upload Well Information.xlsx")
            ),
            column(
                width = 2, br(),
                shiny::actionButton("previewBtn", "Preview")
            ),
            column(
                width = 1, br(),
                shiny::actionButton("hideBtn", "Hide")
            ),
            column(
                width = 2, br(),
                shiny::actionButton("blank", "Manual Input")
            ),
            column(
                width = 2, br(),
                shiny::actionButton("saveBtn", "Save Changes")
            ),
            column(
                width = 1, br(),
                shiny::actionButton("join", "Set Conditions")
            ),
        ),
        div(
            style = "font-size: 11px;",
            rhandsontable::rHandsontableOutput("excelTable")
        ),
        h3("Save Data Locally"),
        fluidRow(
            column(
                width = 5,
                shiny::radioButtons("radio",
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
                shiny::radioButtons("file_type",
                    label = "file type: ",
                    choices = list("txt", "csv"),
                    inline = TRUE,
                    selected = "csv"
                )
            ),
            column(
                width = 3,
                shiny::selectInput("withCondition",
                    label = "With Conditions: ",
                    choices = c(TRUE, FALSE), selected = FALSE
                )
            )
        ),
        fluidRow(
            column(
                width = 5,
                shiny::textInput("name",
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
                shiny::actionButton("preview", "Preview Output")
            ),
            column(
                br(),
                width = 2,
                shiny::actionButton("write", "Save to Directory")
            ),
            column(
                br(),
                width = 2,
                shiny::actionButton("savelocal", "Save to R")
            )
        ),
        br(),
        shiny::actionButton("stopButton", "Close Window")
    )
}
