
#' Bayesian Optimization for OGS5 models
#'
#' @description Implementation of the Bayesian Optimization algorithm to find an
#' approximate minimum of a user specified function, usually the mean squared error
#' between simulation results and experiment.
#'
#' @param par_init *tibble* with initial parameters to start calibration.
#' It needs to follow a strict format and is best created with the function
#' [cal_sample_parameters()]. See details for more.
#' @param kappa *character* or *numeric* that determines the tuning parameter that weights
#' exploitation versus exploration. If set to default`"log_t"` then
#' \eqn{\kappa = 0.2dlog(2d)}.
#' @param max_it *numeric* maximum number of BO iterations.
#' @param exp_data calibration data that will be used in the target function.
#' @param ogs5_obj *ogs5* base simulation object.
#' @param outbloc_names *character vector* names of the blocs specified in the
#'  **.out* file that should be used for calibration. The argument will be passed
#'  to the function [ogs5_get_output_specific()].
#' @param ogs_exe *character* path to the ogs executable.
#' @param target_function *function* specified by the user that should exist in
#' the global environment and be of the form ```f <- function(ogs_obj, exp_data) { ... return(sim_error) }```.
#' @param ensemble_path *character* path where ensemble for initial parameters
#' should be written and run.
#' @param scale_which *character* that identifies the parameters in
#' `par_init` that should be scaled. Default is *NULL*, then all parameters
#' will be scaled according to *scale_fun*.
#' @param scale_fun *function* that allows sampling from a scaled distribution, e.g.
#' if the values are on a *log* scale. Default is `I()` i.e. no transformation.
#' @param unscale_fun *function* inverse of the previous function to transform
#' parameters back after sampling. Default is `I()` as well.
#'
#' @details Bayesian optimization as proposed by *Mockus et al. (1978)* with the
#' Lower Confidence Bound acquisition function to balance Exploration and Exploitation
#' as in *P. Auer (2002)*. In brief, a Gaussian Process model implementation by
#' *Macdonald et al. (2015)* is used as a surrogate model to explore the parameter
#' space in search for points where the model is either uncertain and/or predicts
#' a minimum. If `kappa` is set to `"log_t"`, the weight on Exploration is incremented
#' logarithmically with every iteration. The Algorithm stops, if the relative variability
#' of prediction errors from the surrogate model (relative regret variance) is
#' small enough or the specified maximum number of iteration (`max_it`) is reached.
#'
#' The `par_init` data frame should look like this:
#'
#' |file_ext | mkey | skey | spec | min | max | 1 | 2 | ... |
#' |---------|------|------|------|-----|-----|---|---|-----|
#' | mmp     | MEDIUM_PROPERTIES1 | POROSITY | 1 | 0.25 | 0.45 | 0.4 | 0.3 | ...|
#' | mmp     | Medium_PROPERTIES2 | ... |
#'
#' If a parameter has two values that should be calibrated, simply two identical
#' parameter specifications (with different min/max values) should be provided
#' in adjacent rows.
#'
#' The initial parameter set can be created with the functions
#' [cal_create_calibration_set()] and [cal_sample_parameters()].
#'
#' Analogous to the `calibration_set`, every row is a parameter with its location
#' in the `ogs5` object specified in the first few columns.
#' Additional specifications to the parameter can be added in *spec*,
#' but it can also left blank. Following, *min* and *max* should be indicated
#' and thereafter  the starting values for the parameter.
#'
#' @references Toward global optimization
#' *Mockus, Jonas and Tiesis, Vytautas and Zilinskas, Antanas,*
#' **1978** volume 2, chapter Bayesian Methods for Seeking the Extremum, pp. 117–128.
#'
#' Using Confidence Bounds for Exploitation-Exploration Trade-offs
#' *P. Auer* **2002** Journal of Machine Learning Research 3, 397-422
#'
#' GPfit: An R package for fitting a Gaussian process model to deterministic simulator outputs
#' *Macdonald, Blake and Ranjan, Pritam and Chipman, Hugh*
#' **2012** Journal of Statistical Software, 64, issue 12
#'
#' @return A *list* with
#'  \describe{
#'    \item{gp_model}{The final Gaussian-Process model object (library `GPfit`)}
#'    \item{values}{Parameters from *(0, 1)* used as dependent variables in *gp_model*}
#'    \item{sim_errors}{Simulation errors used as independent variable in *gp_model*}
#'    \item{min}{Parameter where the smallest simulation error was found.}
#'    \item{regret}{Vector of squared differences between meta model prediction and
#'    subsequent simulation for the queried parameters.}
#'  }
#'
#' @export
#'
#' @examples r2ogs/examples/bayesOpt_example.R
#'
cal_bayesOpt <- function(par_init,
                          max_it,
                          kappa = "log_t",
                          delta = 0.1,
                          exp_data,
                          ogs5_obj,
                          outbloc_names,
                          ogs_exe,
                          target_function,
                          ensemble_path,
                          ensemble_cores,
                          scale_which = NULL,
                          scale_fun = I,
                          unscale_fun = I) {


    # check if par_df is already in the unit intervall
    if (all(to01(par_init,
                 scale_which, scale_fun, unscale_fun)[, -c(1:6)] == 1)) {
        stop("par_init seems to be transformed to the unit interval already")
    }

    k <- switch(kappa,
                "log_t" = function (d, i) {
                    beta_i <- 2 * log((d * pi**2 * i**2) / (6 * delta))
                    return(sqrt(beta_i))
                },
                kappa)
    # if constat make function that returns constant
    if (!is.function(k)) {
        k <- function(d, i) {
            return(k)
        }
    }

    # initialize loop
    i <- 1
    d <- nrow(par_init)
    X <- NULL
    err <- NULL
    errs <- NULL
    regret <- NULL
    par_df <- par_init
    rel_var <- 99
    pred_mu <- NULL
    pred_sigma <- NULL

    while(i <= max_it) {

        # evaluate ogs simulation and calculate error with helper function
        err <- cal_simulation_error(par_df,
                                    exp_data = exp_data,
                                    ogs5_obj = ogs5_obj,
                                    outbloc_names = outbloc_names,
                                    ogs_exe = ogs_exe,
                                    target_function = target_function,
                                    ensemble_path = ensemble_path,
                                    ensemble_cores = ensemble_cores)
        # add to previous errors
        errs <- c(errs, err)

        #=== fit meta model and select next parameters ====

        # transform values into 0, 1 interval and transpose
        X <- rbind(X,
                   data.frame(t(
                       to01(par_df,
                            scale_which,
                            scale_fun,
                            unscale_fun)[7:ncol(par_df)])))

        if (i > 1) {
            # Warm start with previous values
            meta <- GPfit::GP_fit(X = X, # design matrix of parameters in the 0 1 interval
                                  Y = errs, # corresponding vector of simulation errors
                                  corr = list(type = "exponential",
                                              power = 1.95),
                                  # LHD, best points, clusters
                                  control = c(d * 20, d * 8, d),
                                  optim_start = beta)
        } else {
            meta <- GPfit::GP_fit(X = X, # design matrix of parameters in the 0 1 interval
                                  Y = errs, # corresponding vector of simulation errors
                                  corr = list(type = "exponential", power = 1.95))
        }

        # save parameters as starting values next model fitting
        beta <- meta$beta

        # sample from 0 1 interval
        x_new <- cal_sample_parameters(par_init[, c(1:6)],
                                       n_samples = 500 * d,
                                       interval_01 = TRUE)[, -c(1:6)] %>%
            t() %>% data.frame()

        # predict
        pred <- GPfit::predict.GP(meta,
                                  xnew = x_new)

        # evaluate acquisition function (could be another modular funciton)
        lcb <- pred$Y_hat - sqrt(pred$MSE) * k(d, i)
        print(k(d, i))
        # select value according to minimum lcb
        x_star <- x_new[which.min(lcb), ]
        # create ogs input from the new value
        par_df <- from01(cbind(par_init[, c(1:6)], t(x_star)),
                         scale_which, scale_fun, unscale_fun)

        # === Calculate stopping criterion ====
        # if (i > 1) {
        #     # evaluate prediction from last iteration with simulation error
        #     regret <- c(regret, (err - pred_mu)**2)
        # }

        # if (i > 4) {
        #     # update moving window and stopping criterion
        #     moving_window <- (length(regret) - 3):length(regret)
        #     rel_var <- var(regret[moving_window] / mean(regret[moving_window]))
        #     message(paste0("Iteration ", i,
        #                    "; Relative regret variance ", round(rel_var, 3)),
        #             "Normalized regret: ", regret[i-1] / pred_sigma)
        # }

        # store current prediction for next iteration
        pred_mu <- c(pred_mu, pred$Y_hat[which.min(lcb)])
        pred_sigma <- c(pred_sigma, sqrt(pred$MSE[which.min(lcb)]))

        i <- i + 1
    }

    mn <- from01(cbind(par_init[, c(1:6)],
                       best = t(X[which.min(errs), ])),
                 scale_which, scale_fun, unscale_fun)

    return(list(
        gp_model = meta,
        values = X,
        sim_errors = errs,
        min = mn,
        pred_mu = pred_mu,
        pred_sigma = pred_sigma
        #regret = regret
    ))
}


#' Transform to unit interval
#'
#' @param par_df *tibble* such as explained in [cal_bayesOpt()] with original values
#' @param scale_which *character* (optional) that identifies the parameters to be scaled
#' @param scale_fun *function* (optional) to scale the values, e.g. [log10()]
#' @param unscale_fun *function* (optional) inverse of `scale_fun`, e.g. [10**x]
#'
#' @return *tibble* with parameter specification and transformed values in the unit interval
#'
#' @examples
to01 <- function(par_df, scale_which = NULL, scale_fun = I, unscale_fun = I) {
    # converts to unit interval
    for (k in seq_len(nrow(par_df))) {

        if(!is.null(scale_which)) {
            # skip parameters that should not be scaled
            if (!any(stringr::str_detect(par_df[k, c(1:6)], scale_which))) {
                scale_fun <- I
                unscale_fun <- I
            }}
        par_df[k, -c(1:6)] <- par_df[k, -c(1:6)] %>%

            unlist %>%
            scale_fun %>% # e.g. log10()
            punif(min = scale_fun(par_df[[k, "min"]]),
                  max = scale_fun(par_df[[k, "max"]])) %>%
            t # tell tibble this is a row
    }
    return(par_df)
}

#' Transform from unit interval
#'
#' @param par_df *tibble* such as explained in [cal_bayesOpt()] with values in the unit interval
#' @param scale_which *character* (optional) that identifies the parameters to be scaled
#' @param scale_fun *function* (optional) to scale the values, e.g. [log10()]
#' @param unscale_fun *function* (optional) inverse of `scale_fun`, e.g. [10**x]
#'
#' @return *tibble* with parameter specification and transformed values in the actual parameter space
#'
#' @examples
from01 <- function(par_df, scale_which = NULL, scale_fun = I, unscale_fun = I) {
    # converts from unit interval
    for (k in seq_len(nrow(par_df))) {

        if(!is.null(scale_which)) {
            # skip parameters that should not be scaled
            if (!any(stringr::str_detect(par_df[k, c(1:6)], scale_which))) {
                scale_fun <- I
                unscale_fun <- I
            }}
        par_df[k, -c(1:6)] <- par_df[k, -c(1:6)] %>%

            unlist %>%
            qunif(min = scale_fun(par_df[[k, "min"]]),
                  max = scale_fun(par_df[[k, "max"]])) %>%
            unscale_fun %>%  # e.g. 10**x
            t # tell tibble this is a row
    }
    return(par_df)
}
