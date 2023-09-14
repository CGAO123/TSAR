observe_grid <- function(input, output,
                         highlighted_cells, unhighlighted_cells) {
    shiny::observe({
        for (i in seq_len(96)) {
            shinyjs::hide("hidden")
            shinyjs::runjs(
                sprintf("$('#cell-%d').click(function() {
                        if ($(this).css('background-color') ===
                        'rgb(255, 255, 255)') {
                        $(this).css('background-color', 'lightgrey');
                        var cellId = $(this).attr('id');
                        var label = $(this).text().trim();
                        Shiny.onInputChange('highlighted_cells',
                        {id: cellId, label: label});
                        } else {
                        $(this).css('background-color', 'white');
                        var cellId = $(this).attr('id');
                        var label = $(this).text().trim();
                        Shiny.onInputChange('highlighted_cells', null);
                        Shiny.onInputChange('unhighlighted_cells',
                        {id: cellId, label: label});
                        }
                        });", i)
            )
        }
    })
}
clear_selection <- function(input, output, clicked_points,
                            highlighted_cells, unhighlighted_cells) {
    shiny::observeEvent(input$clear, {
        clicked_points$legend_text <- NULL
        highlighted_cells(NULL)
        unhighlighted_cells(NULL)
        print_grid(input, output, highlighted_cells)
        print_click(input, output, clicked_points)
    })
}
unhighlight_grid <- function(input, output, highlighted_cells) {
    shiny::observeEvent(input$unhighlighted_cells, {
        cell_id <- input$unhighlighted_cells$id
        cell_label <- input$unhighlighted_cells$label
        if (cell_label %in% unlist(highlighted_cells())) {
            current_values <- highlighted_cells()
            updated_values <- current_values[current_values
            != cell_label]
            highlighted_cells(updated_values)
            print_grid(input, output, highlighted_cells)
        }
    })
}
highlight_grid <- function(input, output, highlighted_cells) {
    shiny::observeEvent(input$highlighted_cells, {
        if (!is.null(input$highlighted_cells)) {
            cell_id <- input$highlighted_cells$id
            cell_label <- input$highlighted_cells$label
            if (!is.null(cell_id)) {
                highlighted_cells(c(
                    highlighted_cells(),
                    cell_label
                ))
                print_grid(input, output, highlighted_cells)
            }
        } else {
            if (cell_label %in% unlist(highlighted_cells())) {
                current_values <- highlighted_cells()
                updated_values <- current_values[current_values
                != cell_label]
                highlighted_cells(updated_values)
                print_grid(input, output, highlighted_cells)
            }
        }
    })
}
toggle_grid <- function(input, output) {
    shiny::observeEvent(input$toggle_button, {
        shinyjs::toggle("grid-panel")
    })
}
register_click <- function(input, output, clicked_points, gg1) {
    shiny::observeEvent(event_data("plotly_click", source = "plotdata"), {
        d <- event_data("plotly_click", source = "plotdata")
        pd <- gg1()
        legend_text <- pd[["x"]][["data"]][[d$curveNumber + 1]][["name"]]


        if (
            (legend_text %in% clicked_points$legend_text)) {
            clicked_points$legend_text <- clicked_points$legend_text[
                clicked_points$legend_text != legend_text
            ]
        } else {
            clicked_points$legend_text <- c(
                clicked_points$legend_text,
                legend_text
            )
        }
    })
}
copy_click <- function(input, output, highlighted_cells, clicked_points) {
    shiny::observeEvent(input$myButton, {
        clicked_points <- gsub(
            ", ", "','",
            toString(unique(
                c(
                    clicked_points$legend_text,
                    highlighted_cells()
                )
            ))
        )
        tobecopied <- paste("'", clicked_points, "'",
            sep = ""
        )
        jscode <- sprintf(
            "var message = %s;
                         var tempInput = $('<input>');
                         $('body').append(tempInput);
                         tempInput.val(message).select();
                         document.execCommand('copy');
                         tempInput.remove();
                         Shinyjs.showAlert('Copied!', type = 'success');",
            jsonlite::toJSON(tobecopied)
        )
        shinyjs::runjs(jscode)
        if (!input$dialogueToggle) {
            showModal(modalDialog(
                title = "Successfully Copied",
                "Review using screen() and remove data with caution."
            ))
        }
    })
}
copy_click_full <- function(input, output, highlighted_cells, clicked_points) {
    shiny::observeEvent(input$removeCall, {
        clicked_points <- gsub(
            ", ", "','",
            toString(unique(
                c(
                    clicked_points$legend_text,
                    highlighted_cells()
                )
            ))
        )
        tobecopied <- paste("remove_raw(",
            input$extractedname,
            ", removelist = c('", clicked_points, "'))",
            sep = ""
        )
        jscode <- sprintf(
            "var message = %s;
                         var tempInput = $('<input>');
                         $('body').append(tempInput);
                         tempInput.val(message).select();
                         document.execCommand('copy');
                         tempInput.remove();
                         Shinyjs.showAlert('Copied!', type = 'success');",
            jsonlite::toJSON(tobecopied)
        )
        shinyjs::runjs(jscode)
        if (!input$dialogueToggle) {
            showModal(modalDialog(
                title = "Successfully Copied",
                "Paste function call to script and run directly to
                            remove selected wells."
            ))
        }
    })
}
saved_weeded <- function(input, output, dataset) {
    shiny::observeEvent(input$savetoR, {
        assign("new_raw_data", dataset(), envir = .GlobalEnv)
        if (!input$dialogueToggle) {
            showModal(modalDialog(
                title = "Data Saved Successfully",
                "Data saved to global environment of r console.
                Check for variable 'tsar_data'"
            ))
        }
    })
}
stop_window <- function(input, output, dataset) {
    shiny::observeEvent(input$stopButton, {
        stopApp()
    })
}
remove_selected <- function(input, output, dataset, checkrange, checklist, gg1,
                            highlighted_cells, clicked_points) {
    shiny::observeEvent(input$removeButton, {
        new_data <- remove_raw(dataset(),
            removelist = c(
                clicked_points$legend_text,
                highlighted_cells()
            )
        )
        dataset(new_data)
        if (!input$dialogueToggle) {
            showModal(modalDialog(
                title = "Successfully Removed",
                "Review new data within window."
            ))
        }
        print_plot(input, output, dataset, checkrange, checklist, gg1)
    })
}
refresh <- function(input, output, dataset, checkrange, checklist, gg1) {
    shiny::observeEvent(input$refreshButton, {
        print_plot(input, output, dataset, checkrange, checklist, gg1)
        if (!input$dialogueToggle) {
            showModal(modalDialog(
                title = "Successfully Refreshed",
                "All edits to dataframe are temporary. Close window using the
                close buttom will store new dataset under new_raw_data
                in the global environment. User may also copy wells and
                run remove_raw( ) in console to store change permanently."
            ))
        }
    })
}
view_selected <- function(input, output, dataset, checkrange, checklist,
                          highlighted_cells, clicked_points, gg1) {
    shiny::observeEvent(input$viewRemovedButton, {
        output$distPlot <- plotly::renderPlotly({
            gg1_p <- TSAR::screen(dataset(),
                checklist =
                    unique(c(
                        clicked_points$legend_text,
                        highlighted_cells()
                    ))
            )
            plotdata <- plotly::ggplotly(gg1_p, source = "plotdata")
            gg1(plotdata)
            plotly::event_register(gg1(), "plotly_click")
        })
        if (!input$dialogueToggle) {
            if ((is.null(clicked_points$legend_text)) &&
                (is.null(highlighted_cells()))) {
                showModal(modalDialog(
                    "Error: No valid Well variable was found.
                        Make sure it is named 'Well.Position' or 'Well'"
                ))
            } else {
                showModal(modalDialog(
                    title = "Viewing Selected Curves Only",
                    "Click remove if selections are correct. Interact with
                    graph or grid to select more or unselect curve. Else use
                    `Refrsh Screen` to return to home graph."
                ))
            }
        }
        print_grid(input, output, highlighted_cells)
        print_click(input, output, clicked_points)
    })
}
