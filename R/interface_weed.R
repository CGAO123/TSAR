mainpage <- function(raw_data, data_name) {
    fluidPage(
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
                shiny::checkboxInput("dialogueToggle",
                    "Hide All Hints and Messages",
                    value = FALSE
                )
            ),
            column(
                width = 4,
                shiny::actionButton(
                    "clear",
                    "Clear all Selected"
                )
            )
        ),
        fluidRow(
            column(width = 4, h3("Current Raw Dataset: ")),
            h3(data_name)
        ),
        div(
            class = "sticky-panel",
            plotly::plotlyOutput("distPlot")
        ),
        verbatimTextOutput("info"),
        fluidRow(
            column(width = 3, h4("Select by Grid")),
            column(
                width = 2,
                h5(" "),
                shinyWidgets::materialSwitch("toggle_button", "Show Grid",
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
                                repeat(12, 50px); grid-gap: 0px;",
                        lapply(seq_len(96), function(i) {
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
        h4("Copy Edited"),
        fluidRow(
            column(
                width = 3,
                shiny::actionButton("myButton", "Copy Selected Wells")
            ),
            column(
                width = 4,
                shiny::actionButton(
                    "removeCall",
                    "Copy in remove_raw() Call"
                )
            ),
            column(
                width = 3,
                actionButton(
                    "savetoR",
                    "Save changes to R"
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
                        shiny::actionButton(
                            "viewRemovedButton",
                            "View Selected"
                        )
                    ),
                    column(
                        width = 2,
                        shiny::actionButton(
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
                        shiny::actionButton(
                            "removeButton",
                            "Remove Selected"
                        )
                    )
                )
            )
        ),
        br(),
        actionButton("stopButton", "Close Window"),
        div(
            id = "hidden",
            shiny::checkboxGroupInput("extractedname",
                label = "name",
                choices = list(data_name),
                selected = data_name
            )
        )
    )
}
