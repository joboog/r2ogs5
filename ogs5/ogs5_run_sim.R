# this file contains code to execute simulation runs on different OS


# run simulation ----------------------------------------------------------

ogs5_run <- function(ogs5_obj = list(), 
                     exe_path = NULL, run_path = NULL, 
                     log_output = TRUE,
                     log_path = NULL,
                     wait = TRUE){
  
  # validate input
  valid_ogs5(ogs5_obj)

  # check paths
  if (is.null(exe_path)){
    exe_path <- paste0(attributes(ogs5_obj)$sim_path)
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
  
  # run ogs5 sim
  command_to_os <- paste0(exe_path,"/ogs* ", 
                          run_path,"/", attributes(ogs5_obj)$sim_name," ",
                          logfile)
  
  system(command = command_to_os, wait = wait)
}
