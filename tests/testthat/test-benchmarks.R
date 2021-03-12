bm <- menu(choices = c("Engesgaard",       #1
                       "Transient_flow",   #2
                       "PETSC",            #3
                       "McWhorter",        #4
                       "all",              #5
                       "none"),            #6
           title = "Which benchmarks do you want to run?")

# Here, the re-written inputfiles for the Engesgaard benchmarks are handed over
# to OGS5 to:
# 1. Make sure they run (compiled version works)
# 2. produce the same result as in
# https://github.com/ufz/ogs5-benchmarks_ref/tree/master/C/Engesgaard
if (bm == 5) {
    bm <- c(1, 2, 3, 4)
}

# Engsesgaard/2Kin/slow_kin_pqc -----------------------------------------------
if (1 %in% bm) {

    context("Engesgaard benchmark 1 vs reference")
    # run ogs from compiled directory
    ogs5_run(ogs5_obj = eg1_read, ogs_exe = "../../inst/ogs/ogs_ipqc",
             run_path = NULL,
             log_output = TRUE,
             log_path = paste0(tmp, "/eg1_read/log"))

    # read in reference tec files
    eg1_tec_ref <- ogs5_read_tecplot_polyline(
        filepath = paste0(bm_prefix,
                          "extdata/ogs5_benchmarks_ref/eg1_ref_polyline.tec")
                    ) %>%
                    dplyr::select(!pe) # exclude pe

    # read in tec files from simulation
    eg1_tec_test <- ogs5_read_tecplot_polyline(
                    filepath = paste0(attributes(eg1_read)$sim_path,
                                  "/eg1_read_ply_OUT_LINE_t1.tec")) %>%
                    dplyr::select(!pe) # exclude pe


    test_that("dimensions of test and reference file are equal",
             expect_equal(dim(eg1_tec_ref), dim(eg1_tec_test))
             )

    for (colname in colnames(eg1_tec_test)) {
        test_that(paste0("column ", colname, " is equal"),
                  expect_equal(round(eg1_tec_ref[[paste0(colname)]], 10),
                               round(eg1_tec_test[[paste0(colname)]], 10))
                                   )
    }
# Engsesgaard/2Kin/slow_kin_pqc_krc ----------------------------------------
## pe fails!! Same results are obtained from cmd run of ogs

    # ogs5_run(ogs5_obj = eg2_read, ogs_exe = "../../inst/ogs/ogs_ipqc",
    #          run_path = NULL,
    #          log_output = TRUE,
    #          log_path = paste0(tmp, "/eg2_read/log"))
    #
    # eg2_tec_ref <- ogs5_read_tecplot_polyline(
    #     filepath = "../../inst/extdata/ogs5_benchmarks_ref/eg2_ref_polyline.tec")
    #
    # eg2_tec_test <- ogs5_read_tecplot_polyline(
    #     filepath = paste0(attributes(eg2_read)$sim_path,
    #                       "/eg2_read_ply_OUT_LINE_t1.tec"))
    #
    # message("Engesgaard benchmark 2 vs reference")
    # test_that("dimensions of test and reference file are equal",
    #             expect_equal(dim(eg2_tec_ref), dim(eg2_tec_test))
    #             )
    #
    # for (colname in colnames(eg2_tec_test)) {
    #        test_that(paste0("column ", colname, " is equal"),
    #                  expect_equal(eg2_tec_ref[[paste0(colname)]],
    #                               eg2_tec_test[[paste0(colname)]])
    #                                   )
    # }
}

# TRANSIENT FLOW benchmark--------------------------------------------------
if (2 %in% bm) {

    context("Groundwater_flow/Transient_flow benchmark")
    ogs5_run(fct_read,
             ogs_exe = "../../inst/ogs/ogs_fem",
             run_path = NULL,
             log_output = TRUE,
             log_path = paste0(tmp, "/fct_read/log"))

    test_that("tec file is produced",
                expect_true(file.exists(
                    paste0(attributes(fct_read)$sim_path,
                           "/fct_read_domain_GROUNDWATER_FLOW_quad.tec")
                ))
              )


}

# ConcreteCrack benchmark -----------------------------------------------------
if (3 %in% bm)  {

    context("PETSc/ConcreteCrack")
    test_that("simulation with PETSC runs",
           expect_invisible(
               ogs5_run(cct_read,
                    ogs_exe = "../../inst/ogs/ogs_petsc-gems",
                    run_path = NULL,
                    log_output = TRUE,
                    log_path = paste0(tmp, "/cct_read/log"),
                    use_mpi = TRUE,
                    number_of_cores = 4)
               ))

    test_that("vtk output is produced",
              expect_true(file.exists(
                  paste0(attributes(cct_read)$sim_path, "/decal0000.vtk")))
    )
}


# McWhorter benchmark -----------------------------------------------------
if (4 %in% bm) {
    context("MPI/McWhorter")
    ogs5_run(ddc_read,
             ogs_exe = "../../inst/ogs/ogs_mpi",
             run_path = NULL,
             log_output = TRUE,
             log_path = paste0(tmp, "/ddc_read/log"),
             use_mpi = TRUE,
             number_of_cores = 4)
    test_that("tec output is produced",
              expect_true(file.exists(
                  paste0(attributes(ddc_read)$sim_path,
                         "/ddc_read_time_POINT4.tec")))
    )
}

