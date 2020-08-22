context("minimal working example for whole package")



test_that("ex1 gives the correct results", {
    # run ogs5 simulation, ex1 is in tests/testthat/temp
    ogs5_run(ogs5_obj = ex1, ogs_exe = "../../inst/ogs/ogs_5.76",
             run_path = NULL,
             log_output = TRUE,
             log_path = "/tmp/ex1/log")

    # read tecplot results
    tec_df <- ogs5_read_many_tecplots(filepath = "/tmp/ex1",
                                      geo_object = "domain")
    # load original result
    tracer_sol <- unlist(read.table(file = "tracer_sol.txt"))
    # # stored via
    # write.table(tec_df$Tracer, file="tests/testthat/tracer_sol.txt",
    #             row.names = FALSE, col.names = FALSE)

    # test results
    expect_equal(as.numeric(tec_df$Tracer), as.numeric(tracer_sol))

})
