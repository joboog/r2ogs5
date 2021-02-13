
cal_simulation_error <- function(df,
                                  exp_data,
                                  ogs5_obj,
                                  outbloc_names,
                                  ogs_exe,
                                  log_output = TRUE,
                                  run_path = attributes(ogs5_obj)$sim_path,
                                  fetch_output_fun,
                                  target_function,
                                  ensemble_path = NULL) {

    # general purpose function to sequentially call ogs5 runs
    # takes value(s) to evaluate by ogs, runs a simulation and calls
    # f(), that has to be user defined and with simulation results

    # the data frame theta should contain the columns file_ext, mkey, skey.

    # TODO: Sanity check for mkey, skey, file_ext


    # === change input parameters ===

    n_samples <- ncol(df) - 6
    # check if ensemble
    is_ens <- n_samples > 1

    if (is_ens) {
        ens1 <- cal_change_parameters(ogs5_obj, par_df = df, ensemble_path)
    } else {
        ogs5_obj <- cal_change_parameters(ogs5_obj, par_df = df, ensemble_path)
    }


    if (is_ens) {
        ### === ensemble run ###
        error <- rep(NA, n_samples)

        # prepare parallel run
        '%dopar%' <- foreach::'%dopar%'
        cl <- parallel::makeForkCluster(2)
        doParallel::registerDoParallel(cl)

        foreach::foreach(i = seq_along(ens1)) %dopar% {
            # Write files
            ogs5_write_inputfiles(ens1[[i]],
                                  type = "all",
                                  folderpath = attributes(ens1[[i]])$sim_path)
            # Run
            r2ogs::ogs5_run(ens1[[i]],
                            ogs_exe = ogs_exe,
                            log_output = TRUE,
                            log_path = attributes(ens1[[i]])$sim_path,
                            wait = TRUE)
        }
        parallel::stopCluster(cl)

        for (i in seq_along(ens1)) {
            # get output
            tryCatch(
                ens1[[i]] <- ogs5_get_output_specific(ens1[[i]],
                                                     outbloc_names),
                error = {
                    # if error occurs, wait and try again!
                    Sys.sleep(1)
                    ens1[[i]] <- ogs5_get_output_specific(ens1[[i]],
                                                         outbloc_names)
                }
            )

            # call f
            error[i] <- target_function(ens1[[i]], exp_data)
        }

    } else {
        ### === sequential run ###
        ogs5_write_inputfiles(ogs5_obj = ogs5_obj,
                              type = unique(df["file_ext"]),
                              folderpath = attributes(ogs5_obj)$sim_path)

        # === run ogs ===
        ogs5_run(
            ogs5_obj = ogs5_obj,
            ogs_exe = ogs_exe,
            log_output = log_output,
            log_path = attributes(ogs5_obj)$sim_path,
            run_path = run_path,
            wait = TRUE
        )

        # === read output ===
        tryCatch(
            ogs5_obj <- ogs5_get_output_specific(ogs5_obj,
                                                 outbloc_names),
            error = {
                # if error occurs, wait and try again!
                Sys.sleep(1)
                ogs5_obj <- ogs5_get_output_specific(ogs5_obj,
                                                     outbloc_names)
            }
        )
        # === call f ===
        # user specified function
        error <- target_function(ogs5_obj, exp_data)
    }

    return(error)
}

cal_change_parameters <- function(ogs5_obj, par_df, ensemble_path) {

    # TODO; sanity check for par_df check if in ogs5 object

    n_samples <- ncol(par_df) - 6
    # check if ensemble
    is_ens <- n_samples > 1

    if (is_ens) {
        # create ensemble
        ens1 <- create_ens(base_sim = ogs5_obj,
                           parameter_tbl = par_df[, c(7:ncol(par_df))],
                           name = "initial_ensemble",
                           path = ensemble_path)
        message(paste("Preparing ensemble run of", ncol(par_df) - 6))
    }

    for (v in seq_len(n_samples)) { # every sample

        for (k in seq_len(nrow(par_df))) { # every parameter

            # for sequential runs, v is only 1.
            # For the initial, sample, it will be as long as the sample size

            ogs5_obj$input[[par_df[k, "file_ext"]]][[par_df[k, "mkey"]]][[par_df[k, "skey"]]] <-

                paste0(par_df[k, "spec"], " ", par_df[k, v + 6])

        }
        if (is_ens) {
            ens1 <- ens_add_ogs5(x = ens1, ogs5_obj = ogs5_obj)
        }
    }
    if (is_ens) {
        return(ens1)
    } else {
        return(ogs5_obj)
    }
}

cal_create_calibration_set <- function(range_list) {

    # creates a dataframe out of a list of vectors provided by the user.
    # It makes sure, that the right values are changed in the ogs object.

    set <- data.frame(matrix(nrow = length(range_list),
                             ncol = 6)) %>%
        'colnames<-' (c("file_ext", "mkey", "skey", "spec", "min", "max"))

    for (k in 1:nrow(set)) {
        set[k, 1:3] <- range_list[[k]][1] %>%
            stringr::str_split("\\$") %>%
            unlist
        set[k, "spec"] <- range_list[[k]][2]
        set[k, c("min", "max")] <- range_list[[k]][3:4]
        set$min <- as.numeric(as.character(set$min))
        set$max <- as.numeric(as.character(set$max))
    }
    return(set)
}

cal_sample_parameters <- function(calibration_set,
                                   n_samples = d * 3,
                                   interval_01 = FALSE,
                                   scale_which = NULL,
                                   scale_fun = I,
                                   unscale_fun = I) {
    d <- nrow(calibration_set)

    smpl <- t(lhs::randomLHS(n = n_samples, d))
    smpl <- as.data.frame(cbind(calibration_set, smpl))

    if (interval_01) {
        return(smpl)
    } else {
        return(from01(smpl,
                      scale_which = scale_which,
                      scale_fun = scale_fun,
                      unscale_fun = unscale_fun))
    }

}



# optim on ogst5_run_seq --------------------------------------------------

# functionto be called by optim
optim_ogs <- function(param,
                      calibration_set,
                      exp_data,
                      ogs5_obj,
                      ogs_exe,
                      log_output = TRUE,
                      run_path = attributes(ogs5_obj)$sim_path,
                      fetch_output_fun,
                      ensemble_path) {

    # parameters need to be in the same order as listed in the calibration_set
    df  <- cbind(calibration_set, v = param)

    error <- ogs5_simulation_error(df,
                                   exp_data,
                                   ogs5_obj,
                                   ogs_exe,
                                   log_output,
                                   run_path,
                                   fetch_output_fun,
                                   ensemble_path)
    return(error)
}
