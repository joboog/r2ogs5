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

    # test_config <- TRUE
    #
    # if (test_config) {
    #     options(
    #         r2ogs5.default_sim_path = "/home/boog/ufz/11_r2ogs/r2ogs/tmp/",
    #         r2ogs5.default_ogs5_benchmark_path =
    #             "/home/boog/ufz/11_r2ogs/r2ogs5-benchmarks/ogs5_benchmarks/",
    #         r2ogs5.default_ogs5_bin = search_ogs5_bin_path(
    #                                     "ogs_fem", return_ogs5_bin = TRUE),
    #             #"/home/boog/ufz/11_r2ogs/r2ogs/inst/ogs/ogs_fem",
    #         r2ogs5.default_ogs5_bin_path = search_ogs5_bin_path("ogs_fem"),
    #             #"/home/boog/ufz/11_r2ogs/r2ogs/inst/ogs/",
    #         r2ogs5.use_python =
    #             "/home/boog/software/miniconda3/envs/py37_r2ogs/bin/python"
    #     )
    #
    #     reticulate::use_virtualenv(unlist(options("r2ogs5.use_python")))
    # }
    invisible()
}


.onAttach <- function(libname, pkgname){

    packageStartupMessage(
        paste("To make r2ogs5 work seamlessly you should check the ",
              "package options and maybe override the defaults e.g. \n",
              "'options([\"r2ogs5.default_ogs5_bin\"])'.",
              "To set options, use \n",
              "'options(\"<option_name>\" = <option_value>)'"),
        appendLF = TRUE)

    # check if there is a configuraton file to overwrite the options
    # usually 'config.yml' under 'getwd()'
    # if no config file present, try to update some options by
    # looking into OS specific default path for executables
    if (!(update_options_from_config())) {

        packageStartupMessage(
            paste("r2ogs now tries to set the options:",
                   "'r2ogs5.default_sim_path'",
                   "'r2ogs5.default_ogs5_bin' and",
                   "'r2ogs5.default_ogs5_bin_path' by searching your OS.\n"))
        options(
            r2ogs5.default_sim_path = tempdir(),
            r2ogs5.default_ogs5_bin = search_ogs5_bin_path("ogs_fem",
                                                        return_ogs5_bin = TRUE),
            r2ogs5.default_ogs5_bin_path = search_ogs5_bin_path("ogs_fem")
        )
    }
}
