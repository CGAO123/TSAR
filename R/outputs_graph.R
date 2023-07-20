render_message <- function(input, output) {
    output$Plot_Message <- renderPrint({
        cat(
            "Select one of the following graph options and click generate.",
            "\nGraphing takes few seconds to load, please wait :)"
        )
    })
}

tohide <- function(input, output) {
    output$hideBtn <- renderUI({
        NULL
    })
}

totoggle <- function(input, output) {
    output$hideBtn <- renderUI({
        actionBttn(
            inputId = "hideBtn", label = "hide",
            style = "minimal", color = "success", size = "xs"
        )
    })
}

build_dateboxes <- function(input, output) {
    output$date_boxes <- renderUI({
        date_boxes <- lapply(input$file$name, function(i) {
            dateInput(
                inputId = paste0("Date for file ", i),
                label = paste0("Date for file ", i)
            )
        })
        do.call(tagList, date_boxes)
    })
}

build_table <- function(input, output, graph_tsar_data) {
    output$table <- renderTable({
        data.frame(head(graph_tsar_data()))
    })
}

render_box <- function(input, output, box) {
    output$Plot <- renderPlot({
        if (input$Legend == FALSE) {
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

render_selected_compare <- function(input, output, compare) {
    output$Plot <- renderPlot({
        compare()[[input$plot_selected]]
    })
}

print_condition <- function(input, output, graph_tsar_data) {
    output$Condition_ID <- renderPrint({
        condition_IDs(graph_tsar_data())
    })
}

print_well <- function(input, output, graph_tsar_data) {
    output$Well_ID <- renderPrint({
        well_IDs(graph_tsar_data())
    })
}

print_tm <- function(input, output, graph_tsar_data) {
    output$tmlist <- renderTable({
        TSA_Tms(graph_tsar_data())
    })
}

print_deltatm <- function(input, output, graph_tsar_data) {
    output$tmlist <- renderTable({
        Tm_difference(graph_tsar_data(),
            control_condition = input$control_tm
        )
    })
}

print_dates <- function(input, output, saved_dates) {
    output$output <- renderPrint({
        saved_dates
    })
}
