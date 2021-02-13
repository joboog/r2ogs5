
ogs5_bayesOpt <- function(par_init,
                          kappa = "log_t",
                          N,
                          exp_data,
                          ogs5_obj,
                          ogs_exe,
                          fetch_output_fun,
                          target_function,
                          ensemble_path,
                          calibration_set) {


    # check if par_df is already in the unit intervall
    if (all(to01(par_init)[, -c(1:6)] == 1)) {
        stop("par_init seems to be transformed to the unit interval already")
    }
    # initialize loop
    t <- 1
    d <- nrow(par_init)
    X <- NULL
    err <- NULL
    errs <- NULL
    regret <- NULL
    pred_mu <- 0
    par_df <- par_init
    cum_r <- 1

    while(t < N & cum_r > 0.005) {

        # evaluate ogs simulation and calculate error with helper function
        err <- ogs5_simulation_error(par_df,
                                     exp_data = exp_data,
                                     ogs5_obj = ogs5_obj,
                                     ogs_exe = ogs_exe,
                                     fetch_output_fun = fetch_output,
                                     target_function = target_function,
                                     ensemble_path = ensemble_path)

        # check prediction from last iteration with the newly simulated value
        regret <- c(regret, (err - pred_mu)**2)
        # update moving window and stopping criterion
        moving_window <- (length(regret) - 2):length(regret)
        cum_r <- sum(regret[moving_window])

        # add to previous errors
        errs <- c(errs, err)

        # transform values into 0, 1 interval and transpose
        X <- rbind(X,
                   data.frame(t(to01(par_df)[7:ncol(par_df)])))

        meta <- GPfit::GP_fit(X = X, # design matrix of parameters in the 0 1 interval
                              Y = errs, # corresponding vector of simulation errors
                              corr = list(type = "exponential", power = 1.95))

        # sample from 0 1 interval
        # TODO: replace calibration_set with init_df info
        x_new <- ogs5_sample_parameters(calibration_set, 500 * d)[, -c(1:6)] %>%
            t() %>% data.frame()

        # predict
        pred <- GPfit::predict.GP(meta,
                                  xnew = x_new)
        k <- switch(kappa,
                    "log_t" = sqrt(0.2*d*log(2*t)),
                    kappa)

        # evaluate acquisition function (could be another modular funciton)
        lcb <- pred$Y_hat - sqrt(pred$MSE) * k
        # select value according to minimum lcb
        x_star <- x_new[which.min(lcb), ]
        # create ogs input from the new value
        par_df <- from01(cbind(calibration_set[, c(1:6)], t(x_star)))

        # save prediction to compare
        pred_mu <- pred$Y_hat[which.min(lcb)]
        message(paste0("Iteration ", t, "; Cumulative regret ",
                       round(cum_r, 3), "; Kappa ", round(k, 2)))
        t <- t + 1
    }

    mn <- from01(cbind(calibration_set[, c(1:6)], best = t(X[which.min(err), ])))

    return(list(
        gp_model = meta,
        values = X,
        sim_errors = errs,
        min = mn,
        regret = regret
    ))
}


to01 <- function(par_df) {
    # converts to unit interval
    for (k in seq_len(nrow(par_df))) {
        par_df[k, -c(1:6)] <- par_df[k, -c(1:6)] %>%

            unlist %>%
            punif(min = par_df[k, "min"],
                  max = par_df[k, "max"])
    }
    return(par_df)
}


from01 <- function(par_df) {
    # converts from unit interval
    for (k in seq_len(nrow(par_df))) {
        par_df[k, -c(1:6)] <- par_df[k, -c(1:6)] %>%

            unlist %>%
            qunif(min = par_df[k, "min"],
                  max = par_df[k, "max"])
    }
    return(par_df)
}
