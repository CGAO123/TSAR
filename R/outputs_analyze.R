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
    output$Plot <- renderPlot({
        ggarrange(
            plotlist = gg,
            ncol = 1, common.legend = TRUE
        )
    })
}

build_well <- function(input, output, hide = FALSE) {
    output$excelTable <- renderRHandsontable({
        if (hide == TRUE) {
            NULL
        } else {
            rhandsontable(data.frame(head(Well_Information_Template, 9)))
        }
    })
}
