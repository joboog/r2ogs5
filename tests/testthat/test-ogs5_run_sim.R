
test_that("ogs5_run returns errors", {
    expect_error(
        ogs5_run(cct_read,
             ogs_exe = "../../inst/ogs/ogs_petsc-gems",
             run_path = "../../examples/benchmarks/ConcreteCrack",
             log_output = FALSE,
             use_mpi = 2)
        )
    expect_error(
        ogs5_run(cct_read,
                 ogs_exe = "../../inst/ogs/ogs_petsc-gems",
                 run_path = "../../examples/benchmarks/ConcreteCrack",
                 log_output = FALSE,
                 use_mpi = TRUE,
                 number_of_cores = 42.5)
    )
    expect_error(
        ogs5_run(cct_read,
                 ogs_exe = "trash",
                 run_path = "../../examples/benchmarks/ConcreteCrack",
                 log_output = FALSE,
                 use_mpi = FALSE)
    )
})