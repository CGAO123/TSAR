print_plot <- function(input, output, dataset, checkrange, checklist, gg1) {
    output$distPlot <- plotly::renderPlotly({
        gg1_p <- TSAR::screen(dataset(),
            checkrange = checkrange,
            checklist = checklist
        )
        plotdata <- plotly::ggplotly(gg1_p, source = "plotdata")
        gg1(plotdata)
        plotly::event_register(gg1(), "plotly_click")
    })
}
print_click <- function(input, output, clicked_points) {
    output$info <- renderText({
        c("Selected Curve: ", clicked_points$legend_text)
    })
}
print_grid <- function(input, output, highlighted_cells) {
    output$highlighted_list <- renderText({
        c("Selected by Grid: ", highlighted_cells())
    })
}
