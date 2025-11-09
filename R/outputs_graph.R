render_message <- function(input, output) {
    output$Plot_Message <- renderText({
        c(
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
        shinyWidgets::actionBttn(
            inputId = "hideBtn", label = "hide",
            style = "minimal", color = "success", size = "xs"
        )
    })
}

build_dateboxes <- function(input, output) {
    output$date_boxes <- renderUI({
        date_boxes <- lapply(input$file$name, function(i) {
            shiny::dateInput(
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

render_derivative <- function(input, output, graph_tsar_data) {
    shinyjs::hide("bplot")
    shinyjs::show("alternativeplot")
    output$altplot <- renderPlotly({
        df <- graph_tsar_data()
        # Compute derivative if missing, using RFU if available, else Normalized, else Fluorescence
        if (!"norm_deriv" %in% names(df)) {
            y_col <- if ("RFU" %in% names(df)) {
                "RFU"
            } else if ("Normalized" %in% names(df)) {
                "Normalized"
            } else if ("Fluorescence" %in% names(df)) {
                "Fluorescence"
            } else NA_character_
            if (is.na(y_col)) {
                stop("No intensity column found to compute derivative (expected 'RFU', 'Normalized', or 'Fluorescence').")
            }
            if (!"well_ID" %in% names(df) && "Well.Position" %in% names(df)) {
                df$well_ID <- df$Well.Position
            }
            by_well <- split(df, df$well_ID)
            parts <- lapply(by_well, function(w) {
                # ensure numeric Temperature
                w$Temperature <- suppressWarnings(as.numeric(w$Temperature))
                o <- order(w$Temperature)
                w <- w[o, ]
                dy <- diff(as.numeric(w[[y_col]]))
                dx <- diff(as.numeric(w$Temperature))
                deriv <- rep(NA_real_, nrow(w))
                if (length(dy) > 0 && length(dx) == length(dy)) {
                    dd <- dy / dx
                    dd[!is.finite(dd)] <- NA_real_
                    deriv[seq_along(dd)] <- dd
                }
                w$norm_deriv <- deriv
                w
            })
            df <- do.call(rbind, parts)
        }
        # Ensure Tm column exists for label mapping inside view_deriv
        if (!"Tm" %in% names(df)) {
            df$Tm <- NA_real_
        }
        view_deriv(df, frame_by = input$frame_by)
    })
}

render_box <- function(input, output, box) {
    if (input$Legend == FALSE) {
        if (input$makeplotly == TRUE) {
            shinyjs::hide("bplot")
            shinyjs::show("alternativeplot")
            output$altplot <- renderPlotly({
                boxp <- box + labs(y = "Tm (degree Celsius)")
                boxp <- plotly::ggplotly(boxp, originalData = TRUE)
                plotly::layout(boxp,
                    yaxis = list(title_font = list(size = 18)),
                    xaxis = list(title_font = list(size = 18))
                )
            })
        } else {
            shinyjs::show("bplot")
            shinyjs::hide("alternativeplot")
            output$Plot <- renderPlot({
                box + theme(text = element_text(size = 18))
            })
        }
    } else {
        shinyjs::hide("alternativeplot")
        shinyjs::show("bplot")
        output$Plot <- renderPlot({
            box[[1]] <- box[[1]] + theme(
                text = element_text(size = 18)
            )
            ggarrange(
                plotlist = box,
                font.label = list(size = 20)
            )
        })
    }
}

redner_compare <- function(input, output, compare) {
    shinyjs::show("bplot")
    shinyjs::hide("alternativeplot")
    output$Plot <- renderPlot({
        ggarrange(plotlist = compare(), common.legend = TRUE)
    })
    output$plot_select <- renderUI({
        selectInput("plot_selected",
            label = "View Only:",
            choices = c("Select Option..", names(compare()))
        )
    })
}

render_selected_compare <- function(input, output, compare) {
    shinyjs::show("bplot")
    shinyjs::hide("alternativeplot")
    output$Plot <- renderPlot({
        compare()[[input$plot_selected]]
    })
}

render_selected_condition <- function(input, output, curve_graph) {
    shinyjs::show("bplot")
    shinyjs::hide("alternativeplot")
    output$Plot <- renderPlot({
        sep_flag <- tolower(as.character(input$separate_legend)) == "true"
        if (!isTRUE(sep_flag)) {
            # curve_graph is a ggplot
            curve_graph + theme(text = element_text(size = 18))
        } else {
            # curve_graph is a list
            curve_graph[[1]] <- curve_graph[[1]] + theme(text = element_text(size = 18))
            ggpubr::ggarrange(plotlist = curve_graph)
        }
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

dummy_plot <- function(input, output) {
    waiting_data <- data.frame(
        x = 0.5, y = 0.5,
        message = "Awaiting graph :)"
    )
    output$Plot <- renderPlot({
        ggplot(waiting_data, aes(x = x, y = y)) +
            geom_point(alpha = 0) +
            geom_text(aes(label = message), size = 18) +
            theme_void()
    })
}
