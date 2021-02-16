
#' Simulation error for calibration purposes
#'
#' @description Helper function for [cal_bayesOpt()] that evaluates one or
#' several parameters values by modifying the *ogs5* input slot, writing input
#' files and running simulations. The output is retrieved via
#' \code{\link{ogs5_get_output_specific}} and fed into the user-specified
#' `target_function` to calculate the simulation error.
#'
#' @param par_df *tibble* with parameters such as described in \code{\link{cal_bayesOpt}}
#' @param exp_data experimental data
#' @param ogs5_obj *ogs5*
#' @param outbloc_names *character vector* names of the blocs specified in the
#'  **.out* file that should be used for calibration. The argument will be passed
#'  to the function \code{\link{ogs5_get_output_specific}}.
#' @param ogs_exe *character* path to the ogs executable
#' @param log_output *logical* Should a log file be written?
#' @param log_path *character* path of directory to write log file, default is `run_path`.
#' @param run_path *character* path where the simulation should be run, default is
#' *sim_path* from the `ogs5_obj`.
#' @param target_function *function* specified by the user that should exist in
#' the global environment and be of the form ```f <- function(ogs_obj, exp_data) { ... return(sim_error) }```.
#' @param ensemble_path *character* path where ensemble for initial parameters
#' should be written and run.
#'
#' @return An *ogs5* or *ens* class object depending if one set of parameters or
#' several were run.
#' @export
#'
#' @examples r2ogs/examples/bayesOpt_example.R
cal_simulation_error <- function(par_df,
                                  exp_data,
                                  ogs5_obj,
                                  outbloc_names,
                                  ogs_exe,
                                  log_output = TRUE,
                                  log_path = NULL,
                                  run_path = attributes(ogs5_obj)$sim_path,
                                  target_function,
                                  ensemble_path = NULL) {

    # general purpose function to sequentially call ogs5 runs
    # takes value(s) to evaluate by ogs, runs a simulation and calls
    # f(), that has to be user defined and with simulation results

    # the data frame theta should contain the columns file_ext, mkey, skey.

    # TODO: Sanity check for mkey, skey, file_ext


    # === change input parameters ===

    n_samples <- ncol(par_df) - 6
    # check if ensemble
    is_ens <- n_samples > 1

    if (is_ens) {
        ens1 <- cal_change_parameters(ogs5_obj, par_df = par_df, ensemble_path)
    } else {
        ogs5_obj <- cal_change_parameters(ogs5_obj, par_df = par_df, ensemble_path)
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
                              type = unique(par_df["file_ext"]),
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

#' change input parameters of an *ogs5* object
#'
#' @param ogs5_obj *ogs5*
#' @param par_df *tibble* with parameters such as described in \code{\link{cal_bayesOpt}}
#' @param ensemble_path *character* path where ensemble for several parameters
#' should be written and run.
#'
#' @return An *ogs5* or *ens* object with changed input parameters.
#' @export
#'
#' @examples
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

#' Helper function to create a calibration set for ogs5 input parameters
#'
#' @param range_list *list* of vectors containing parameter locations in
#' an *ogs5*-object and its range.
#'
#' @return *data.frame* that can be handed over to [cal_sample_parameters()]
#' @export
#'
#' @examples \dontrun{
#' # define range list
#' range_list <- list(
#'     c("mmp$MEDIUM_PROPERTIES1$PERMEABILITY_TENSOR", "ISOTROPIC", 1.0e-4, 1.0e-2),
#'     c("mmp$MEDIUM_PROPERTIES2$PERMEABILITY_TENSOR", "ISOTROPIC", 1.0e-9, 1.0e-4),
#'     c("mmp$MEDIUM_PROPERTIES3$PERMEABILITY_TENSOR", "ISOTROPIC", 1.0e-7, 1.0e-3),
#'     c("mmp$MEDIUM_PROPERTIES4$PERMEABILITY_TENSOR", "ISOTROPIC", 1.0e-7, 1.0e-3)
#' )
#' # convert into a calibration set
#' calibration_set <- cal_create_calibration_set(range_list)
#' calibration_set
#' }
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

#' sample parameters with the Latin Hypercube method
#'
#' @description LHD samples are drawn from a uniform (0, 1) distribution via
#' \code{\link[lhs]{randomLHS}} and then transformed (if desired) to the
#' respective range via \code{\link[stats]{qunif}} and the minimum- and maximum
#' values specified in `calibration_set`.
#'
#' @param calibration_set *tibble* with columns *file_ext*, *mkey, skey, spec*,
#'  *min*, and *max*. Best created via [cal_create_calibration_set()].
#' @param n_samples *integer* for the number of samples for each parameter
#' @param interval_01 *logical* should the sample be from the (0,1) interval?
#' @param scale_which *character* that identifies the parameters in
#' `calibration_set` that should be scaled. Default is *NULL*, then all parameters
#' will be scaled according to *scale_fun*.
#' @param scale_fun *function* that allows sampling from a scaled distribution, e.g.
#' if the values are on a *log* scale. Default is `I()` i.e. no transformation.
#' @param unscale_fun *function* inverse of the previous function to transform
#' parameters back after sampling. Default is `I()` as well.
#'
#' @return
#' @export
#'
#' @examples \dontrun{# define range list
#' range_list <- list(
#'     c("mmp$MEDIUM_PROPERTIES1$PERMEABILITY_TENSOR", "ISOTROPIC", 1.0e-4, 1.0e-2),
#'     c("mmp$MEDIUM_PROPERTIES2$PERMEABILITY_TENSOR", "ISOTROPIC", 1.0e-9, 1.0e-4),
#'     c("mmp$MEDIUM_PROPERTIES3$PERMEABILITY_TENSOR", "ISOTROPIC", 1.0e-7, 1.0e-3),
#'     c("mmp$MEDIUM_PROPERTIES4$PERMEABILITY_TENSOR", "ISOTROPIC", 1.0e-7, 1.0e-3)
#' )
#' # convert into a calibration set
#' calibration_set <- cal_create_calibration_set(range_list)
#' calibration_set
#'
#' # sample starting parameters from calibration set
#' init <- cal_sample_parameters(calibration_set,
#'                               n_samples = 4,
#'                               interval_01 = FALSE,
#'                               scale_fun = log10,
#'                               unscale_fun = function(x) 10**x,
#' )}
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
                      ensemble_path) {

    # parameters need to be in the same order as listed in the calibration_set
    df  <- cbind(calibration_set, v = param)

    error <- ogs5_simulation_error(df,
                                   exp_data,
                                   ogs5_obj,
                                   ogs_exe,
                                   log_output,
                                   run_path,
                                   ensemble_path)
    return(error)
}
