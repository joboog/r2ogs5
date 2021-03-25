#' update_options_from_config
#' @description Tries to update package options from a user-defined
#' configuration file.
#' @param config *character* Name of the configuration environment. Default is:
#' 'default'.
#' @param file *character* Name of the configuration file. Default is:
#' 'config.yml'.
#'
#' @return *boolean* TRUE if configuration was update, FALSE if not.
#' @export
update_options_from_config <- function(
                            config= Sys.getenv("R_CONFIG_ACTIVE", "default"),
                            file = Sys.getenv("R_CONFIG_FILE", "config.yml")) {

    # read from config file if present
    if (!(file.exists(file))) {
        message(paste0("A configuration file to override the default package",
                       " options does not exist."))
        return(invisible(FALSE))
    }

    # read config, exit function if problems
    config <- tryCatch(
                config::get(config = config, file = file, use_parent = FALSE),
                error = function(e) {
                    message(paste0("A package configuration file '",
                                   file, "' could not be read."))
                    return(NULL)})

    if (!is.null(config)) {

        # set options from config
        options(
            r2ogs5.default_sim_path = config$r2ogs5.default_sim_path,
            r2ogs5.default_ogs5_benchmark_path =
                                        config$r2ogs5.default_ogs5_benchmark_path,
            r2ogs5.default_ogs5_bin = config$r2ogs5.default_ogs5_bin,
            r2ogs5.default_ogs5_bin_path = config$r2ogs5.default_ogs5_bin_path,
            r2ogs5.use_python = config$r2ogs5.use_python)

        return(invisible(TRUE))
    }
    else return(invisible(FALSE))
}
