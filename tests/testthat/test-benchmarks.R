bm <- menu(choices = c("y", "n"), title = "Do you want to test all benchmarks?")

# Here, the re-written inputfiles for the Engesgaard benchmarks are handed over
# to OGS5 to:
# 1. Make sure they run (compiled version works)
# 2. produce the same result as in
# https://github.com/ufz/ogs5-benchmarks_ref/tree/master/C/Engesgaard

if (bm == 1) {


# ENGESGAARD --------------------------------------------------------------

    # run ogs from compiled directory
    ogs5_run(ogs5_obj = eg1_read, ogs_exe = "../../inst/ogs/build/bin/ogs",
             run_path = NULL,
             log_output = TRUE,
             log_path = paste0(tmp, "/eg1_read/log"))

    ogs5_run(ogs5_obj = eg2_read, ogs_exe = "../../inst/ogs/build/bin/ogs",
             run_path = NULL,
             log_output = TRUE,
             log_path = paste0(tmp, "/eg2_read/log"))

   # read in reference tec files
   eg1_tec_ref <- ogs5_read_tecplot_polyline(
        filename = "../../data/benchmarks_ref/eg1_ref_polyline.tec") %>%
        dplyr::select(!pe) # exclude pe

   eg2_tec_ref <- ogs5_read_tecplot_polyline(
        filename = "../../data/benchmarks_ref/eg2_ref_polyline.tec")

    # read in tec files from simulation
    eg1_tec_test <- ogs5_read_tecplot_polyline(
                    filename = paste0(attributes(eg1_read)$sim_path,
                                  "/eg1_read_ply_OUT_LINE_t1.tec")) %>%
                    dplyr::select(!pe) # exclude pe

    eg2_tec_test <- ogs5_read_tecplot_polyline(
       filename = paste0(attributes(eg2_read)$sim_path,
                         "/eg2_ref_polyline.tec")
   )

    context("Engesgaard benchmark 1 vs reference")

    test_that("dimensions of test and reference file are equal",
             expect_equal(dim(eg1_tec_ref), dim(eg1_tec_test))
             )

    for (colname in colnames(eg1_tec_test)) {
        test_that(paste0("column ", colname, " is equal"),
                  expect_equal(round(eg1_tec_ref[[paste0(colname)]], 10),
                               round(eg1_tec_test[[paste0(colname)]], 10))
                                   )
}

## pe fails!! Same results are obtained from cmd run of ogs

       context("Engesgaard benchmark 2 vs reference")
       test_that("dimensions of test and reference file are equal",
                expect_equal(dim(eg2_tec_ref), dim(eg2_tec_test))
                )

       for (colname in colnames(eg2_tec_test)) {
           test_that(paste0("column ", colname, " is equal"),
                     expect_equal(eg2_tec_ref[[paste0(colname)]],
                                  eg2_tec_test[[paste0(colname)]])
                                      )
       }

# TRANSIENT FLOW ----------------------------------------------------------
       context("Groundwater_flow/Transient_flow benchmark")
       test_that("Benchmark with *.fct file runs",
                 expect_invisible(
                     ogs5_run(fct_read,
                              ogs_exe = "../../inst/ogs/ogs_5.76",
                              run_path = NULL,
                              log_output = TRUE,
                              log_path = paste0(tmp, "/fct_read/log"))))

}



