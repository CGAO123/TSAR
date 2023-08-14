hide_p <- function(input, output) {
    shiny::observeEvent(input$hideBtn, {
        shinyjs::hide(id = "myPanel")
        tohide(input, output)
    })
}
toggle_p <- function(input, output) {
    shiny::observeEvent(input$toggleButton, {
        shinyjs::show(id = "myPanel")
        totoggle(input, output)
    })
}
initialize <- function(input, output, graph_tsar_data) {
    shiny::observe({
        if (length(graph_tsar_data()) == 0) {
            if (!input$dialogueToggle) {
                showModal(modalDialog(
                    title = "No data input :(",
                    "No data input: Please upload analysis files to merge
                            or close window \nand call function with data
                            included as parameter. e.g. graph_tsar(tsar_data)"
                ))
            }
        }
    })
}
build_dates <- function(input, output) {
    shiny::observeEvent(input$file, {
        build_dateboxes(input, output)
    })
}
save_dates <- function(input, output, dated, datelist) {
    shiny::observeEvent(input$save_dates, {
        saved_dates <- vapply(input$file$name, function(i) {
            as.character(input[[paste0("Date for file ", i)]])
        }, FUN.VALUE = character(1))
        datelist(saved_dates)
        if (!is.null(datelist)) {
            if (!input$dialogueToggle) {
                showModal(modalDialog(
                    title = "Dates saved",
                    "Confirm in the textbox below if saved dates are
                            correct and proceede to merging data."
                ))
                print_dates(input, output, saved_dates)
                dated(TRUE)
            }
        } else {
            if (!input$dialogueToggle) {
                showModal(modalDialog(
                    title = ":(",
                    "Please upload analysis files first before
                            setting dates."
                ))
            }
        }
    })
}
saved_merged <- function(input, output, graph_tsar_data) {
    shiny::observeEvent(input$savetolocal, {
        assign("tsar_data", graph_tsar_data(), envir = .GlobalEnv)
        if (!input$dialogueToggle) {
            showModal(modalDialog(
                title = "Data Saved Successfully",
                "Data saved to global environment of r console.
                Check for variable 'tsar_data'"
            ))
        }
    })
}
merge_update <- function(input, output, dated, graph_tsar_data,
                         datelist, session) {
    shiny::observeEvent(input$generate, {
        if ((dated() == FALSE) && (!input$dialogueToggle)) {
            showModal(modalDialog(
                title = "Dates are not saved",
                "please review and save dates of experiment!"
            ))
        } else {
            tsar_data_p <- merge_norm(
                data = input$file$datapath,
                name = input$file$name,
                date = datelist()
            )
            tsar_data_p <- na.omit(tsar_data_p)
            graph_tsar_data(tsar_data_p)
            build_table(input, output, graph_tsar_data)
            render_message(input, output)

            updateSelectInput(session, "Control",
                label = "Control Condition",
                choices = c("NA", condition_IDs(graph_tsar_data()))
            )
            updateSelectInput(session, "Control_s",
                label = "Control Condition",
                choices = c(condition_IDs(graph_tsar_data()))
            )
            updateSelectInput(session, "Selected_condition",
                label = "Select condition: ",
                choices = c(condition_IDs(graph_tsar_data()))
            )
            updateSelectInput(session, "control_tm",
                label = "List Delta Tm:",
                choices = c(
                    "Select Control: ",
                    condition_IDs(graph_tsar_data())
                )
            )
            updateSelectInput(session, "remove_condition",
                label = "Select By Condition_ID:",
                choices = condition_IDs(graph_tsar_data())
            )
            updateSelectInput(session, "remove_well",
                label = "Select By Well_ID:",
                choices = well_IDs(graph_tsar_data())
            )
        }
    })
}
build_boxplot <- function(input, output, graph_tsar_data) {
    shiny::observeEvent(input$Boxplot, {
        req(input$Boxplot)
        if (length(graph_tsar_data()) == 0) {
            if (!input$dialogueToggle) {
                showModal(modalDialog(
                    title = "No data input",
                    "No data input: Please upload analysis files to merge
                            or close window \nand call function with data
                            included as parameter. e.g. graph_tsar(tsar_data)"
                ))
            }
        } else {
            req(input$Boxplot)
            if (input$Control == "NA") {
                control_option_b <- NA
            } else {
                control_option_b <- as.character(input$Control)
            }
            box <- TSA_boxplot(graph_tsar_data(),
                color_by = input$Color,
                label_by = input$Label,
                separate_legend = input$Legend,
                control_condition = control_option_b
            )
            render_box(input, output, box)
        }
    })
}
build_compare <- function(input, output, graph_tsar_data, compare) {
    shiny::observeEvent(input$Compareplot, {
        if (length(graph_tsar_data()) == 0) {
            if (!input$dialogueToggle) {
                showModal(modalDialog(
                    title = "No data input",
                    "No data input: Please upload analysis files to merge
                            or close window \nand call function with data
                            included as parameter. e.g. graph_tsar(tsar_data)"
                ))
            }
        } else {
            compare_p <- TSA_compare_plot(graph_tsar_data(),
                y = input$y_axis,
                title_by = input$title_by,
                show_Tm = input$show_tm,
                control_condition = input$Control_s
            )
            compare(compare_p)
            redner_compare(input, output, compare)
        }
    })
}
view_compare <- function(input, output, compare) {
    shiny::observeEvent(input$plot_selected, {
        if (input$plot_selected != "Select Option..") {
            render_selected_compare(input, output, compare)
        }
    })
}
build_curves <- function(input, output, graph_tsar_data) {
    shiny::observeEvent(input$curves, {
        if (length(graph_tsar_data()) == 0) {
            if (!input$dialogueToggle) {
                showModal(modalDialog(
                    title = "No data input",
                    "No data input: Please upload analysis files to merge
                            or close window \nand call function with data
                            included as parameter. e.g. graph_tsar(tsar_data)"
                ))
            }
        } else {
            selected_curves <- subset(
                graph_tsar_data(),
                condition_ID ==
                    input$Selected_condition
            )
            curve_graph <- TSA_wells_plot(selected_curves,
                show_Tm = input$show_tm_c,
                y = input$y_axis_c,
                show_average = input$average,
                smooth = input$smooth,
                Tm_label_nudge = input$num,
                separate_legend = input$separate_legend
            )
            render_selected_condition(input, output, curve_graph)
        }
    })
}
build_derivatives <- function(input, output, graph_tsar_data) {
    shiny::observeEvent(input$build_deriv, {
        render_derivative(input, output, graph_tsar_data)
    })
}

render_condition <- function(input, output, graph_tsar_data) {
    shiny::observeEvent(input$condition, {
        print_condition(input, output, graph_tsar_data)
    })
}
render_well <- function(input, output, graph_tsar_data) {
    shiny::observeEvent(input$well, {
        print_well(input, output, graph_tsar_data)
    })
}
render_tm <- function(input, output, graph_tsar_data) {
    shiny::observeEvent(input$tm, {
        if (input$control_tm == "N/A") {
            print_tm(input, output, graph_tsar_data)
        } else {
            print_deltatm(input, output, graph_tsar_data)
        }
    })
}
remove_selected_graph <- function(input, output, graph_tsar_data) {
    shiny::observeEvent(input$Remove, {
        cur <- graph_tsar_data()
        cur <- subset(cur, !condition_ID %in% input$remove_condition)
        cur <- subset(cur, !well_ID %in% input$remove_well)
        graph_tsar_data(cur)
        if (!input$dialogueToggle) {
            showModal(modalDialog(
                title = "Selection Removed",
                "All selected condition_ID and well_ID are removed.
                To revert to original data, click 'Restore Removal'."
            ))
        }
    })
}
restore_removal <- function(input, output, graph_tsar_data, stock_tsar_data) {
    shiny::observeEvent(input$Restore, {
        graph_tsar_data(stock_tsar_data())
        if (!input$dialogueToggle) {
            showModal(modalDialog(
                title = "All removals restored",
                "Data restored to original status."
            ))
        }
    })
}
closegraph <- function(input, output) {
    shiny::observeEvent(input$stopButton, {
        stopApp()
    })
}
