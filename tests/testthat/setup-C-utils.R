#' check_ogs5_bin
#' @description Checks wether provided ogs_exe exists in default directory and
#'   call 'search_ogs5_bin_path' to check default search paths.
#' @param ogs_exe *character* Name of ogs executable.
#'
#' @return ogs executable name if found or path to ogs exectuable if found.
check_ogs5_bin <- function(ogs_exe) {

    # check if ogs_exe is in default path
    ogs5_bin_path <- paste0(unlist(options("r2ogs5.default_ogs5_bin_path")),
                            ogs_exe)
    if(file.exists(ogs5_bin_path)) {
        return(ogs_exe)
    }
    # now search for ogs5_exe in default search path
    else {
        ogs5_bin_path <- search_ogs5_bin_path(ogs_exe, return_ogs5_bin = TRUE)
        return(ogs5_bin_path)
    }
}

skip_if_ogs5_exe_missing <- function(ogs_exe){
    if (is.null(ogs_exe)) {
        skip(paste0("ogs executable '", ogs_exe, "' does not exist"))
    }
}
