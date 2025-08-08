preview_model <- function(input, output, raw_data) {
    shiny::observeEvent(input$model_fit, {
        test <- subset(raw_data, Well.Position == as.character(input$well))
        test <- normalize(test, fluo = input$num)
        # Choose modeling method based on UI selection
        if (!is.null(input$model_used) && tolower(input$model_used) == "beta") {
            # Fit beta-spline model centered at Tm and augment for view_model()
            test <- model_beta(
                norm_data = test,
                x = "Temperature",
                y = "Normalized"
            )
            # model_beta already returns augmented df with fitted and norm_deriv
            # Ensure required columns exist for plotting
            if (!"fitted" %in% names(test)) {
                # fallback: if attribute stores model, predict
                fit_attr <- attr(test, "beta_model")
                if (!is.null(fit_attr)) {
                    test$fitted <- stats::predict(fit_attr, newdata = test)
                }
            }
        } else {
            model <- model_gam(test, x = test$Temperature, y = test$Normalized)
            test <- model_fit(test, model = model)
        }
        gg <- view_model(test)
        build_view(input, output, gg)
    })
}
perform_analysis <- function(input, output, raw_data, analysis,
                             output_data, analyzed_done) {
    shiny::observeEvent(input$analyze, {
        # convert selectInput values ("TRUE"/"FALSE") to logical
        .to_bool <- function(x) {
            if (is.logical(x)) return(x)
            if (is.null(x)) return(FALSE)
            tolower(as.character(x)) == "true"
        }
        keep_flag    <- .to_bool(input$keep)
        fit_flag     <- .to_bool(input$fit)
        smoothed_flag<- .to_bool(input$smooth)

        analysis_p <- gam_analysis(raw_data,
            fluo_col = input$num_a,
            keep = keep_flag,
            fit = fit_flag,
            smoothed = smoothed_flag,
            selections = c(
                input$checkGroup,
                "Normalized"
            )
        )
        d <- read_tsar(analysis_p, output_content = 2)
        analysis(analysis_p)
        render_data(input, output, d)
        render_data_title(input, output, "Analyzed Data: norm_data")
        analyzed_done(TRUE)
        if (!input$dialogueToggle) {
            shiny::showModal(shiny::modalDialog(
                title = "Analysis is complete",
                "Proceede to inputting conditions and saving data
                        before closing window."
            ))
        }
        output_data_p <- read_tsar(analysis(), output_content = input$radio)
        output_data(output_data_p)
    })
}
join_condition <- function(input, output, analysis, output_data, imported,
                           analyzed_done, well_info, inFile) {
    shiny::observeEvent(input$withCondition, {
    if (imported() == TRUE && tolower(as.character(input$withCondition)) == "true") {
            output_data_p <- join_well_info(
                file_path = inFile(),
                file = well_info(),
                analysis_file =
                    read_tsar(analysis(),
                        output_content = input$radio
                    ),
                type = "by_template"
            )
            output_data(output_data_p)
        } else {
            if (analyzed_done() == FALSE) {
                output_data(NULL)
            } else {
                output_data_p <- read_tsar(analysis(),
                    output_content = input$radio
                )
                output_data(output_data_p)
            }
        }
    })
}
write_file <- function(input, output, analyzed_done, output_data) {
    shiny::observeEvent(input$write, {
        if (analyzed_done() == FALSE) {
            if (!input$dialogueToggle) {
                shiny::showModal(shiny::modalDialog(
                    title = "Analysis is Incomplete!",
                    "Please analyze all data before saving."
                ))
            }
        } else {
            write_tsar(output_data(),
                name = input$name,
                file = input$file_type
            )
            if (!input$dialogueToggle) {
                shiny::showModal(shiny::modalDialog(
                    title = "Successfully Saved",
                    "Check your working directory for data file."
                ))
            }
        }
    })
}
preview_output <- function(input, output, analyzed_done, imported, output_data,
                           analysis, inFile, well_info) {
    shiny::observeEvent(input$preview, {
        if (analyzed_done() == FALSE) {
            if (!input$dialogueToggle) {
                shiny::showModal(shiny::modalDialog(
                    title = "Analysis is Incomplete!",
                    "Please analyze all data before saving."
                ))
            }
        } else {
            if (imported() == FALSE && tolower(as.character(input$withCondition)) == "true") {
                if (!input$dialogueToggle) {
                    shiny::showModal(shiny::modalDialog(
                        title = "Conditions are not specified!",
                        "Please input condition information first
                                or set 'With Conditions' as fasle."
                    ))
                }
            } else {
                if (!input$dialogueToggle) {
                    shiny::showModal(shiny::modalDialog(
                        title = "Hint :)",
                        "If you are saving data for tsar_graphing
                                functions, make sure to save all fluorescence
                                data (i.e. 'both') to output compare plots and
                                conditions plot."
                    ))
                }
                if (imported() == TRUE && tolower(as.character(input$withCondition)) == "true") {
                    output_data_p <- join_well_info(
                        file_path = inFile(),
                        file = well_info(),
                        analysis_file = read_tsar(analysis(),
                            output_content = input$radio
                        ),
                        type = "by_template"
                    )
                    output_data(output_data_p)
                } else {
                    if (analyzed_done() == FALSE) {
                        output_data(NULL)
                    } else {
                        output_data_p <- read_tsar(analysis(),
                            output_content = input$radio
                        )
                        output_data(output_data_p)
                    }
                }
                render_data(input, output, output_data())
                render_data_title(input, output, "Output Data: tsar_data")
            }
        }
    })
}
manual_well <- function(input, output) {
    shiny::observeEvent(input$blank, {
        build_well(input, output)
    })
}
set_condition <- function(input, output, imported, analyzed_done, inFile,
                          well_info, analysis, output_data) {
    shiny::observeEvent(input$join, {
        if ((imported() == FALSE) || (analyzed_done() == FALSE)) {
            if ((analyzed_done() == FALSE) && (!input$dialogueToggle)) {
                shiny::showModal(shiny::modalDialog(
                    title = "Analysis is Incomplete!",
                    "Please analyze all data before saving."
                ))
            } else {
                if (!input$dialogueToggle) {
                    shiny::showModal(shiny::modalDialog(
                        title = "Conditions are not specified!",
                        "Please input condition information first
                                or set 'With Conditions' as fasle."
                    ))
                }
            }
        } else {
            output_data_p <- join_well_info(
                file_path = inFile(),
                file = well_info(),
                analysis_file =
                    read_tsar(analysis(),
                        output_content = input$radio
                    ),
                type = "by_template"
            )
            output_data(output_data_p)
            if (!input$dialogueToggle) {
                shiny::showModal(shiny::modalDialog(
                    title = "Conditions Saved",
                    "Proceede to preview output and save data."
                ))
            }
            render_data(input, output, output_data())
            render_data_title(input, output, "Complete Data: tsar_data")
        }
    })
}

preview_condition <- function(input, output, well_info, data) {
    shiny::observeEvent(input$previewBtn, {
        output$excelTable <- rhandsontable::renderRHandsontable({
            if (!is.null(well_info())) {
                rhandsontable::rhandsontable(data.frame(head(well_info(), 9)))
            } else if (!is.null(data())) {
                rhandsontable::rhandsontable(data.frame(head(data(), 9)))
            }
        })
    })
}
hide <- function(input, output) {
    shiny::observeEvent(input$hideBtn, {
        build_well(input, output, TRUE)
    })
}
save_change <- function(input, output, imported, well_info) {
    shiny::observeEvent(input$saveBtn, {
        imported(TRUE)
        well_info(rhandsontable::hot_to_r(input$excelTable))
        if (!input$dialogueToggle) {
            shiny::showModal(shiny::modalDialog(
                title = "Success",
                "Changes saved successfully."
            ))
        }
    })
}
save_analysis <- function(input, output, output_data) {
    shiny::observeEvent(input$savelocal, {
        assign(input$name, output_data(), envir = .GlobalEnv)
        if (!input$dialogueToggle) {
            shiny::showModal(shiny::modalDialog(
                title = "Successfully Saved",
                "Check your R environment for data file."
            ))
        }
    })
}
stop_analyze <- function(input, output) {
    shiny::observeEvent(input$stopButton, {
    shiny::stopApp()
    })
}
