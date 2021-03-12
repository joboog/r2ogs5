#context("minimal working example for whole package")

test_that("all input files exist in the directory", {

    written_file_extensions <- list.files(paste0(tmp, "/ex1")) %>%
        stringr::str_remove("ex1.") # retain only suffixes such as bc, ic, ...
        expect_true(all(names(ex1$input) %in% written_file_extensions))
})

test_that("running ex1 gives the correct results", {

    # run ogs5 simulation, ex1 is in tests/testthat/temp
    ogs_exe <- check_ogs5_bin("ogs_fem")
    skip_if_ogs5_exe_missing(ogs_exe)

    ogs5_run(ogs5_obj = ex1,
             ogs_exe = ogs_exe,
             run_path = NULL,
             log_output = TRUE,
             log_path = paste0(tmp, "/log"))

    # read tecplot results
    tec_df_ex1 <- ogs5_read_many_tecplots(filepath = paste0(tmp, "/ex1"),
                                      geo_object = "domain")
    # load original result
    tracer_sol <- unlist(read.table(
                            file = paste0(bm_prefix,
                                          "extdata/ex1_tracer_result.txt")))


    # test results
    expect_equal(as.numeric(tec_df_ex1$Tracer), as.numeric(tracer_sol))

})
