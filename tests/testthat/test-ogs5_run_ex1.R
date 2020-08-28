context("minimal working example for whole package")

test_that("all input files exist in the directory", {

    written_file_extensions <- list.files(tmp) %>%
        stringr::str_remove("ex1.") # retain only suffixes such as bc, ic, ...
        expect_true(all(names(ex1$input) %in% written_file_extensions))
})

test_that("ex1 gives the correct results", {
    # run ogs5 simulation, ex1 is in tests/testthat/temp
    ogs5_run(ogs5_obj = ex1, ogs_exe = "../../inst/ogs/ogs_5.76",
             run_path = NULL,
             log_output = TRUE,
             log_path = paste0(tmp, "/log"))

    # read tecplot results
    tec_df <- ogs5_read_many_tecplots(filepath = tmp,
                                      geo_object = "domain")
    # load original result
    tracer_sol <- unlist(read.table(file = "../../data/ex1_tracer_result.txt"))
    # # stored via
    # write.table(tec_df$Tracer, file="tests/testthat/tracer_sol.txt",
    #             row.names = FALSE, col.names = FALSE)

    # test results
    expect_equal(as.numeric(tec_df$Tracer), as.numeric(tracer_sol))

})
