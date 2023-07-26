render_data <- function(input, output, raw_data) {
    output$table <- renderDataTable(
        {
            data.frame(raw_data)
        },
        options = list(pageLength = 7)
    )
}

render_data_title <- function(input, output, title) {
    output$table_title <- renderText({
        title
    })
}

build_view <- function(input, output, gg) {
    gg[[1]] <- ggplotly(gg[[1]], width = 500)
    gg[[2]] <- ggplotly(gg[[2]], width = 500)
    gg[[1]] <- plotly::layout(gg[[1]],
        yaxis = list(tickfont = list(size = 8), showgrid = TRUE)
    )
    gg[[2]] <- plotly::layout(gg[[2]],
        yaxis = list(tickfont = list(size = 8), showgrid = TRUE)
    )
    output$Plot <- renderPlotly({
        sub <- plotly::subplot(gg[[1]], gg[[2]],
            nrows = 2, shareX = TRUE,
            shareY = TRUE
        )
        plotly::layout(sub,
            legend = list(
                orientation = "h", x = 0, y = 1.15,
                font = list(size = 8)
            ),
            xaxis = list(tickfont = list(size = 8), showgrid = TRUE)
        )
    })
}

build_well <- function(input, output, hide = FALSE) {
    output$excelTable <- rhandsontable::renderRHandsontable({
        if (hide == TRUE) {
            NULL
        } else {
            rhandsontable(data.frame(head(well_information_template, 9)))
        }
    })
}
