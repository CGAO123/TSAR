test_that("Test remove_raw", {
    data("qPCR_data1")
    output <- remove_raw(qPCR_data1, removelist = c("A01", "D11"))
    result <- ("A01" %in% output$Well.Position)
    expect_equal(result, FALSE)
})

test_that("Test merge_norm for correct merging", {
    data("qPCR_data1")
    result1 <- gam_analysis(qPCR_data1, smoothed = TRUE, fluo = 5)
    data("well_information")
    norm_data <- join_well_info(
        file_path = NULL, file = well_information,
        read_tsar(result1, output_content = 2), type = "by_template"
    )
    norm_data <- na.omit(norm_data)
    data("qPCR_data2")
    result2 <- gam_analysis(qPCR_data1, smoothed = TRUE, fluo = 5)
    norm_data2 <- join_well_info(
        file_path = NULL, file = well_information,
        read_tsar(result2, output_content = 2), type = "by_template"
    )
    norm_data2 <- na.omit(norm_data2)
    tsar_data <- merge_norm(
        data = list(norm_data, norm_data2),
        name = c("Thermal Shift_162.eds.csv", "Thermal Shift_168.eds.csv"),
        date = c("20230203", "20230209")
    )
    result <- ("well_ID" %in% names(tsar_data)) &&
        ("condition_ID" %in% names(tsar_data))
    expect_equal(result, TRUE)
})

test_that("Test join_well_info", {
    data("qPCR_data1")
    analysis <- gam_analysis(qPCR_data1, smoothed = TRUE, fluo = 5)
    data("well_information")
    output <- join_well_info(
        file_path = NULL, file = well_information,
        read_tsar(analysis, output_content = 2), type = "by_template"
    )
    result <- ("Protein" %in% names(output)) && ("Ligand" %in% names(output))
    expect_equal(result, TRUE)
})

test_that("Test read_tsar", {
    data("qPCR_data1")
    analysis <- gam_analysis(qPCR_data1, smoothed = TRUE, fluo = 5)
    data("well_information")
    output <- read_tsar(analysis, output_content = 2)
    result <- ("tm" %in% names(output))
    expect_equal(result, TRUE)
})

test_that("Test gam_analysis", {
    data("qPCR_data1")
    analysis <- gam_analysis(qPCR_data1, smoothed = TRUE, fluo = 5)
    result <- ("tm" %in% names(analysis[[1]]))
    expect_equal(result, TRUE)
})

test_that("Test normalize", {
    data("qPCR_data1")
    test <- subset(qPCR_data1, Well.Position == "A01")
    output <- normalize(test)
    result <- ("Normalized" %in% names(output))
    expect_equal(result, TRUE)
})

test_that("Test model_fit", {
    data("qPCR_data1")
    test <- subset(qPCR_data1, Well.Position == "A01")
    test <- normalize(test)
    gammodel <- model_gam(test, x = test$Temperature, y = test$Normalized)
    output <- model_fit(test, model = gammodel)
    result <- ("norm_deriv" %in% names(output))
    expect_equal(result, TRUE)
})

test_that("Test Tm_est", {
    data("qPCR_data1")
    test <- subset(qPCR_data1, Well.Position == "A01")
    test <- normalize(test)
    gammodel <- model_gam(test, x = test$Temperature, y = test$Normalized)
    fit <- model_fit(test, model = gammodel)
    result <- Tm_est(fit)
    expect_equal(result, 53.736416)
})

test_that("Test condition_IDs", {
    data("example_tsar_data")
    output <- condition_IDs(example_tsar_data)
    result <- output[1]
    expect_equal(result, "CA FL_DMSO")
})

test_that("Test well_IDs", {
    data("example_tsar_data")
    output <- well_IDs(example_tsar_data)
    result <- output[1]
    expect_equal(result, "A01_CA FL_DMSO_20230203")
})

test_that("Test TSA_proteins", {
    data("example_tsar_data")
    output <- TSA_proteins(example_tsar_data)
    result <- output[1]
    expect_equal(result, "CA FL")
})

test_that("Test TSA_ligands", {
    data("example_tsar_data")
    output <- TSA_ligands(example_tsar_data)
    result <- ("4-ABA" %in% output)
    expect_equal(result, TRUE)
})

test_that("Test Tm_difference", {
    data("example_tsar_data")
    control <- condition_IDs(example_tsar_data)[1]
    output <- Tm_difference(example_tsar_data, control_condition = control)
    result <- output$delta_Tm[1]
    expect_equal(result, 0)
})

test_that("Test TSA_Tms", {
    data("example_tsar_data")
    output <- TSA_Tms(example_tsar_data)
    result <- ("Avg_Tm" %in% names(output))
    expect_equal(result, TRUE)
})

test_that("Test TSA_average", {
    data("example_tsar_data")
    output <- TSA_average(example_tsar_data,
        y = "Fluorescence", digits = 1,
        avg_smooth = TRUE, sd_smooth = TRUE
    )
    result <- ("sd" %in% names(output)) && ("sd_max" %in% names(output))
    expect_equal(result, TRUE)
})

test_that("Test ", {
    input <- c(0, 1, 3)
    result <- rescale(input)
    expect_equal(result, c(0.0000000, 0.33333333, 1.0000000))
})
