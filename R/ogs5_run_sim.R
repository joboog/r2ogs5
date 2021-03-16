# this file contains code to execute simulation runs on different OS

#' ogs5_run
#' @description Run **ogs5** simulation via call to OS.
#' @param ogs5_obj *ogs5* simulation object.
#' @param ogs_exe *character* Path to **ogs5** executable.
#' @param run_path *character* Path to directory where the simulation should be executed.
#'   Default: use 'attributes(ogs5_obj)$sim_path'
#' @param log_output *logical* Should a log file be written?
#' @param log_path *character* Path of directory to write log file.
#' @param use_mpi *logical* Execute parallelized simulation using MPI.
#' @param number_of_cores *integer* Use number of computing cores if 'use_mpi=TRUE'.
#' @param wait *logical* if 'TRUE', Freeze R-session until simulation is finished.
#' @export
#' @examples
#' \dontrun{
#'  ogs5_run(ogs5_obj = sim1,
#'           ogs_exe = "inst/ogs/ogs_5.76",
#'           run_path = NULL,
#'           log_output = TRUE,
#'           log_path = "examples/tmp/ex1/log")
#' }
ogs5_run <- function(ogs5_obj = list(),
                     ogs_exe = NULL,
                     run_path = NULL,
                     log_output = TRUE,
                     log_path = NULL,
                     use_mpi = FALSE,
                     number_of_cores = NULL,
                     wait = TRUE){

  # validate input
  valid_ogs5(ogs5_obj)

  # check ogs_exe
  if (is.null(ogs_exe)){
    ogs_exe <- unlist(options("r2ogs5.default_ogs5_bin"))
  }
  else if (dir.exists(ogs_exe)) { # if is dir

    stop("You did not specifiy the full path of the ogs executable.",
         call. = FALSE)
  }
  else if (!(dir.exists(ogs_exe)) & !(file.exists(ogs_exe))) {

    # check if provided ogs_exe exist in default path
    ogs_exe <- paste0(
                unlist(options("r2ogs5.default_ogs5_bin_path")), ogs_exe)

    if (!(file.exists(ogs_exe))){
      stop(paste0("The provided ogs executable does not exist. ",
                  "Even not at 'r2ogs5.default_ogs5_bin_path'"),
           call. = FALSE)
    }
  }

  # check run_path
  if (is.null(run_path)){
    run_path <- paste0(attributes(ogs5_obj)$sim_path)
  }
  if (!(dir.exists(run_path))) dir.create(run_path, recursive = TRUE)

  # check log
  if (log_output == TRUE) {
    if (is.null(log_path)){
      log_path <- paste0(attributes(ogs5_obj)$sim_path)
    } else{
      if (!(dir.exists(log_path))) dir.create(log_path, recursive = TRUE)
    }
    logfile <- paste0(" > ", log_path, "/", attributes(ogs5_obj)$sim_name,".log")
  } else logfile <- ""

  # check mpi settings
  if (!is.logical(use_mpi)) {
    stop("use_mpi must be logical")
  }
  if (use_mpi) {
    if ((number_of_cores %% 1 != 0) &
        number_of_cores > parallel::detectCores()) {
        stop("number_of_cores needs to be an integer that does not
             exceed the available number of CPUs")
    } else {
      message("number_of_cores has to agree with the partitioning of the mesh!")
      ogs_exe <- paste0("mpirun --oversubscribe -np ",
                        number_of_cores, " ", ogs_exe)
    }
  }

  # run ogs5 sim
  command_to_os <- paste0(ogs_exe, " ",
                          run_path,"/", attributes(ogs5_obj)$sim_name," ",
                          logfile)

  system(command = command_to_os, wait = wait)
}


#' search_ogs5_bin_path
#' @description Search paths to ogs5 binary or ogs5 bin folder. Includes
#'   'PATH' environment variable. Returns first match of provided 'ogs_exe'.
#' @param ogs_exe *character* Name of the ogs5 executable.
#' @param return_ogs5_bin *boolean* If TRUE returns the path to the ogs5
#'   binary instead of the bin folder path.
#'
#' @return *character* of path to ogs5 bin folder or ogs5 binary
#' @export
search_ogs5_bin_path <- function(ogs_exe="ogs", return_ogs5_bin=FALSE){

  # set defautl search path
  search_paths <- Sys.getenv("PATH") %>% stringr::str_split(":") %>% `[[`(1)

  if (Sys.info()["sysname"] == "Linux") {

    search_paths <- search_paths %>% c(c("inst/ogs/", "../../inst/ogs/"))
  }
  else if (Sys.info()["sysname"] == "Windows") {
    search_paths <- search_paths %>% c(c("C:/Programme/OpenGeoSys/bin/",
                                        "C:/Programs/OpenGeoSys/bin/"))

    if (!(stringr::str_detect(ogs_exe, ".exe"))) {
      ogs_exe <- paste0(ogs_exe, ".exe")
    }
  }

  for (search_path in search_paths) {

    files <- list.files(search_path)
    if (any(ogs_exe %in% files)) {
      ifelse(return_ogs5_bin==FALSE,
             return(search_path),
             return(paste0(search_path, ogs_exe)))
    }
  }
}
