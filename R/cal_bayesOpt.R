
#' Bayesian Optimization for OGS5 models
#'
#' @description Implementation of the Bayesian Optimization algorithm to find an
#' approximate minimum of a user specified function, usually the mean squared error
#' between simulation results and experiment.
#'
#' @param par_init *tibble* with initial parameters to start calibration.
#' It needs to follow a strict format and is best created with the function
#' [cal_sample_parameters()]. See details for more.
#' @param kappa *character*, *numeric* or *function* that determines the tuning
#' parameter that weights exploitation versus exploration depending on dimension
#' and iteration (see details).
#' @param max_it *numeric* maximum number of BO iterations.
#' @param exp_data calibration data that will be used in the target function.
#' @param ogs5_obj *ogs5* base simulation object.
#' @param outbloc_names *character vector* names of the blocs specified in the
#'  **.out* file that should be used for calibration. The argument will be passed
#'  to the function [ogs5_get_output_specific()].
#' @param ogs_exe *character* path to the ogs executable.
#' @param target_function *function* specified by the user that should exist in
#' the global environment and be of the form
#' ```f <- function(ogs_obj, exp_data) { ... return(sim_error) }``` and return a
#'  single *numeric* value that represents the error from the current `ogs_obj`
#'   and `exp_data`.
#' @param ensemble_path *character* path where ensemble for initial parameters
#' should be written and run.
#' @param ensemble_cores *numeric* integer number for the number of cores used
#' during the initial ensemble run.
#' @param ensemble_name *character* name for the ensemble run to distinguish from
#' other (potentially running) ensembles. The name will appear in numbered folders
#' for every run of the ensemble.
#' @param scale_which (optional) *character* that identifies the parameters in
#' `par_init` that should be scaled. Default is *NULL*, then all parameters
#' will be scaled according to *scale_fun*.
#' @param scale_fun (optional) *function* that allows sampling from a scaled
#' distribution, e.g. `scale_fun = log10` if the values are on a *log* scale.
#'  Default is `I()` i.e. no transformation.
#' @param unscale_fun (optional) *function* inverse of the prev`ious function
#' (i.e. `unscale_fun = function(x) 10**x`) to transform parameters back after
#'  sampling. Default is `I()` as well.
#' @param BO_init (optional) object of class *BO* from a previous run to continue
#' optimization. No further arguments except `kappa`, `max_it`, `ogs_exe` and
#' the scaling functions (if necessary) have to be supplied, as everything else
#' will be taken from the `BO_init` class attributes.
#'
#' @details Bayesian Optimization as proposed by *Mockus et al. (1978)* with the
#' Lower Confidence Bound acquisition function to balance Exploration and
#' Exploitation as in *P. Auer (2002)*. In brief, a Gaussian Process model
#' implementation by *Macdonald et al. (2015)* is used as a surrogate model to
#' explore the parameter space in search for points where the model is either
#' uncertain and/or predicts a minimum. The Algorithm stops, if the specified
#' maximum number of iteration (`max_it`) is reached.
#'
#' The uncertainty paramter `kappa` is by default set to `"log_t"` which results in
#' \eqn{\kappa_i = (2 log(d \pi^2 i^2 / (6 * 0.1)))^1/2} which ensures the
#' unceratainty bound from the meta-model to be greater than the prediction error
#' with a probability of 0.9 after (*Sirinivas et al. (2010)*).
#'  Setting `kappa = "cooling"` results in
#' \eqn{\kappa_i = (2log(400d^22)) / (1 + exp(-4.6 + 9.2 * i / N))^1/2}
#' yielding a logistic decreasing function with turning point at \eqn{i = N/2}
#' converging to 0 towards the end of iterations (\eqn{N}) ensuring exploitation
#' of the meta-model. `kappa` can be set to any user specified function that
#' includes d and i as arguments, a positive constant or 0.
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
#' @references
#' Using Confidence Bounds for Exploitation-Exploration Trade-offs
#' *P. Auer* **2002** Journal of Machine Learning Research 3, 397-422
#'
#' GPfit: An R package for fitting a Gaussian process model to deterministic
#' simulator outputs
#' *Macdonald, Blake and Ranjan, Pritam and Chipman, Hugh*
#' **2012** Journal of Statistical Software, 64, issue 12
#'
#' Toward global optimization
#' *Mockus, Jonas and Tiesis, Vytautas and Zilinskas, Antanas,*
#' **1978** volume 2, chapter Bayesian Methods for Seeking the Extremum,
#' pp. 117–128.
#'
#' Gaussian process optimization in the bandit setting: No regret and
#' experimental design
#' *Sirinivas N., Krause A., Kakade S. et al.* **2010** ICML 2010-Proceedings,
#' 27th International Conference on Machine Learning, 1015-1022.
#'
#'
#' @return A *list* with
#'  \describe{
#'    \item{gp_model}{The final Gaussian-Process model object (library `GPfit`)}
#'    \item{values}{Parameters from *(0, 1)* used as dependent variables in *gp_model*}
#'    \item{sim_errors}{Simulation errors used as independent variable in *gp_model*}
#'    \item{min}{Parameter where the smallest simulation error was found.}
#'    \item{pred_mu}{Vector of predictions by the meta-model cast for every
#'    element in `sim_errors` previous to the target function evaluation.}
#'    \item{pred_sigma}{Vector of prediction variance for every element in
#'    `pred_mu`.}
#'  }
#'  The *BO*-class *attributes* include all the relevant further info and data
#'  such as the *ogs5* simulation object, experimental data and the target
#'  function.
#'
#' @export
#'
#' @examples \dontrun{link to vignette}
#'
cal_bayesOpt <- function(par_init,
                          max_it,
                          kappa = "log_t",
                          exp_data,
                          ogs5_obj,
                          outbloc_names,
                          ogs_exe,
                          target_function,
                          ensemble_path,
                          ensemble_cores,
                          ensemble_name,
                          scale_which = NULL,
                          scale_fun = I,
                          unscale_fun = I,
                          BO_init = NULL) {

    # store call
    cl <- match.call()
    # if kappa is constant make function that returns constant
    if (!is.function(kappa)) {
        # not a function
        if (kappa == "log_t") {
            k <-  function (d, i) {
                    beta_i <- 2 * log((d * pi**2 * i**2) / (6 * 0.1))
                    return(sqrt(beta_i))
            }
        } else if (kappa == "cooling") {
            k <- function(d, i) {
                return(sqrt(2*log(400*d**2)) /
                           (1 + exp(-4.6 + 9.2 * i / max_it)))
            }
        } else if (is.numeric(kappa) & length(kappa) == 1 & kappa >= 0) {
            # positive constant
            k <- function(d, i) {
                return(as.numeric(kappa))
            }
        } else {
            # none of the former
        stop("kappa must be either one of \"log_t\" or \"cooling\",
                a positive number or a function that accepts arguments d and i")
        }
    } else {
        # is already a user defined function
        k <- kappa
    }

    if (!is.null(BO_init)) {
        # An initial BO object is supplied, iterations are "continued"
        if (!class(BO_init) == "BO") stop("BO_init must be of class BO")
        # extract variables from previous run
        meta <- BO_init$gp_model
        par_init <- BO_init$min
        X <- BO_init$values
        errs <- BO_init$sim_errors
        pred_mu <- BO_init$pred_mu
        pred_sigma <- BO_init$pred_sigma
        ogs5_obj = attributes(BO_init)$sim_data$ogs5_obj
        exp_data = attributes(BO_init)$sim_data$exp_data
        ogs5_obj = attributes(BO_init)$sim_data$ogs5_obj
        outbloc_names = attributes(BO_init)$sim_data$outbloc_names
        target_function = attributes(BO_init)$sim_data$target_function

    } else {
        # check if par_df is already in the unit intervall
        if (all(to01(par_init,
                     scale_which, scale_fun, unscale_fun)[, -c(1:6)] == 1)) {
            stop("par_init seems to be transformed to the unit interval already")
        }
        X <- NULL
        pred_mu <- NULL
        pred_sigma <- NULL
        errs <- NULL
    }

    # initialize loop
    i <- 1
    d <- nrow(par_init)
    err <- NULL
    regret <- NULL
    par_df <- par_init

    while(i <= max_it) {

        if (i > 1 | is.null(BO_init)) {
            # No initial BO object is supplied

            # evaluate ogs simulation and calculate error with helper function
            err <- cal_simulation_error(par_df,
                                        exp_data = exp_data,
                                        ogs5_obj = ogs5_obj,
                                        outbloc_names = outbloc_names,
                                        ogs_exe = ogs_exe,
                                        target_function = target_function,
                                        ensemble_path = ensemble_path,
                                        ensemble_cores = ensemble_cores,
                                        ensemble_name = ensemble_name)
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
                t0 <- Sys.time()
                # Warm start with previous values
                meta <- GPfit::GP_fit(X = X,
                                      Y = errs,
                                      corr = list(type = "exponential",
                                                  power = 1.95),
                                      # LHD, best points, clusters (minimum 2)
                                      control = c(d * 10, d * 5,
                                                  max(2, ceiling(0.25*d))),
                                      optim_start = beta)
            } else {
                meta <- GPfit::GP_fit(X = X,
                                      Y = errs,
                                      corr = list(type = "exponential",
                                                  power = 1.95))
            }
        }

        # save parameters as starting values next model fitting
        beta <- meta$beta

        # sample from 0 1 interval
        x_new <- cal_sample_parameters(par_init[, c(1:6)],
                                       n_samples = 2**d + d*500 + 2000,
                                       interval_01 = TRUE)[, -c(1:6)] %>%
            t() %>% data.frame()

        # predict
        pred <- GPfit::predict.GP(meta,
                                  xnew = x_new)

        # evaluate acquisition function
        lcb <- pred$Y_hat - (sqrt(pred$MSE) * k(d, i) / (length(errs) - d - 1))
        # select d parameters according to minimum lcb
        xmin_sampled <- select_diff(x_new[order(lcb)[1:(d * 10)], ])


        # update optim function
        gp_lcb <- function(x) {
            x <- t(x)
            stopifnot(all(dim(x) == c(1, d)))
            pred <- GPfit::predict.GP(meta, xnew = x)
            lcb <- pred$Y_hat - sqrt(pred$MSE) * k(d, i)
            return(lcb)
        }
        xmin_optim <- xmin_sampled
        # inner multistart optim loop
        for (p_i in 1:nrow(xmin_sampled)) {
            op <- stats::optim(par = xmin_sampled[p_i, ],
                        fn = gp_lcb,
                        method = "L-BFGS-B",
                        lower = rep(0, d),
                        upper = rep(1, d)
            )
            op$par[which(op$par < 0)] <- 0
            op$par[which(op$par > 1)] <- 1
            xmin_optim[p_i, 1:d] <- op$par
            xmin_optim[p_i, d + 1] <- op$value
        }
        # select "global" lcb minimum
        x_star <- xmin_optim[which.min(xmin_optim[, d + 1]), 1:d]

        # create ogs input from the new value
        par_df <- from01(cbind(par_init[, c(1:6)], t(x_star)),
                         scale_which, scale_fun, unscale_fun)

        # store current prediction for next iteration
        pred <- GPfit::predict.GP(meta, xnew = x_star)
        pred_mu <- c(pred_mu, pred$Y_hat)
        pred_sigma <- c(pred_sigma, sqrt(pred$MSE))
        # pred_sigma <- c(pred_sigma, sqrt(pred$MSE[which.min(lcb)]))
        if (i > 1) {
            message(paste0("Iteration: ", i, ", error: ", round(err, 3),
                           " optim iterations: ", p_i,
                           " kappa: ", round(k(d, i))))
        }

        i <- i + 1
    }
    errs <- c(errs, cal_simulation_error(par_df,
                         exp_data = exp_data,
                         ogs5_obj = ogs5_obj,
                         outbloc_names = outbloc_names,
                         ogs_exe = ogs_exe,
                         target_function = target_function,
                         ensemble_path = ensemble_path,
                         ensemble_cores = ensemble_cores,
                         ensemble_name = ensemble_name))

    mn <- from01(cbind(par_init[, c(1:6)],
                       best = t(X[which.min(errs), ])),
                 scale_which, scale_fun, unscale_fun)

    X <- rbind(X, x_star)

    output <- list(
        gp_model = meta,
        values = X,
        sim_errors = errs,
        min = mn,
        pred_mu = pred_mu,
        pred_sigma = pred_sigma
    )

    return(structure(output,
                     class = "BO",
                     call = cl,
                     sim_data = list(
                         ogs5_obj = ogs5_obj,
                         exp_data = exp_data,
                         ogs5_obj = ogs5_obj,
                         outbloc_names = outbloc_names,
                         target_function = target_function
                     )))
}


#' Transform to unit interval
#'
#' @param par_df *tibble* such as explained in [cal_bayesOpt()] with original values
#' @param scale_which *character* (optional) that identifies the parameters to be scaled
#' @param scale_fun *function* (optional) to scale the values, e.g. `log10()`
#' @param unscale_fun *function* (optional) inverse of `scale_fun`, e.g. `10**x``
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
            stats::punif(min = scale_fun(par_df[[k, "min"]]),
                  max = scale_fun(par_df[[k, "max"]])) %>%
            t # tell tibble this is a row
    }
    return(par_df)
}

#' Transform from unit interval
#'
#' @param par_df *tibble* such as explained in [cal_bayesOpt()] with values in the unit interval
#' @param scale_which *character* (optional) that identifies the parameters to be scaled
#' @param scale_fun *function* (optional) to scale the values, e.g. `log10()`
#' @param unscale_fun *function* (optional) inverse of `scale_fun`, e.g. `10**x``
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
            stats::qunif(min = scale_fun(par_df[[k, "min"]]),
                  max = scale_fun(par_df[[k, "max"]])) %>%
            unscale_fun %>%  # e.g. 10**x
            t # tell tibble this is a row
    }
    return(par_df)
}

select_diff <- function(xmin_sampled) {
    sel_i <- 1
    for (i in 2:nrow(xmin_sampled)) {
        dif <- mean(t(abs(xmin_sampled[1, ] - xmin_sampled[i, ])))
        if (dif > 0.5) {
            sel_i <- c(sel_i, i)
        }
    }
    return(xmin_sampled[sel_i, ])
}
