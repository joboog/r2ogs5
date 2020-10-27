# this file contains code to execute simulation runs on different OS


# run simulation ----------------------------------------------------------

ogs5_run <- function(ogs5_obj = list(),
                     ogs_exe = NULL, run_path = NULL,
                     log_output = TRUE,
                     log_path = NULL,
                     use_mpi = FALSE,
                     number_of_cores = NULL,
                     wait = TRUE){

  # validate input
  valid_ogs5(ogs5_obj)

  # check paths
  if (is.null(ogs_exe)){
    stop("You did not specifiy the full name of the ogs executable.",
         call. = FALSE)
  }
  if (!(file.exists(ogs_exe))){
    stop("The provided ogs executable does not exist.", call. = FALSE)
  }

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
      warning("number_of_cores has to agree with the partitioning of the mesh!")
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
