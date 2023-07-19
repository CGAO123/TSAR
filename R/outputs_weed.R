print_plot <- function(input, output, dataset, checkrange, checklist, gg1) {
    output$distPlot <- plotly::renderPlotly({
        gg1 <<- TSAR::screen(dataset(),
            checkrange = checkrange,
            checklist = checklist
        )
        plotdata <<- plotly::ggplotly(gg1, source = "plotdata")
        plotdata
        plotly::event_register(plotdata, "plotly_click")
    })
}
print_click <- function(input, output, clicked_points) {
    output$info <- renderPrint({
        cat("Selected Curve: ", clicked_points$legend_text)
    })
}
print_grid <- function(input, output, highlighted_cells) {
    output$highlighted_list <- renderPrint({
        cat("Selected by Grid: ", highlighted_cells())
    })
}
