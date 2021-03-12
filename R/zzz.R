.onLoad <- function(libname, pkgname){

    op <- options()
    op.r2ogs5 <- list(
        # Default paths
        r2ogs5.default_sim_path = "",
        r2ogs5.default_ogs5_benchmark_path = "",
        r2ogs5.default_ogs5_bin = "",
        r2ogs5.default_ogs5_bin_path = "",

        # External file reference tags

        # Reticulate setting
        r2ogs5.use_python = ""
    )

    toset <- !(names(op.r2ogs5) %in% names(op))
    if (any(toset)) options(op.r2ogs5[toset])

    test_config <- TRUE

    if (test_config) {
        options(
            r2ogs5.default_sim_path = "/home/boog/ufz/11_r2ogs/r2ogs/tmp/",
            r2ogs5.default_ogs5_benchmark_path =
                "/home/boog/ufz/11_r2ogs/r2ogs5-benchmarks/ogs5_benchmarks/",
            r2ogs5.default_ogs5_bin =
                "/home/boog/ufz/11_r2ogs/r2ogs/inst/ogs/ogs_fem",
            r2ogs5.default_ogs5_bin_path =
                "/home/boog/ufz/11_r2ogs/r2ogs/inst/ogs/",
            r2ogs5.use_python =
                "/home/boog/software/miniconda3/envs/py37_r2ogs/bin/python"
        )

        reticulate::use_virtualenv(unlist(options("r2ogs5.use_python")))
    }

    invisible()
}


.onAttach <- function(libname, pkgname){

    packageStartupMessage(
        paste("To make the r2ogs5 work seamlessly yo need to set the package",
              "options for overriding the default e.g. \n",
              "'options([\"r2ogs5.default_ogs5_bin\"])'.",
              "To set options, use \n",
              "'options(\"<option_name>\" = <option_value>)'"))
}
