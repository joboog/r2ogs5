
# === define simulation object ========
ogs5_obj <- create_ogs5(sim_name = "Ashi",
                        sim_id = 1L,
                        sim_path = "examples/gw1")

ogs5_obj <- input_add_blocs_from_file(ogs5_obj = ogs5_obj,
                                      sim_basename = "Ashi",
                                      filename = "all",
                                      file_dir = "inst/examples/groundwater")

ogs5_write_inputfiles(ogs5_obj, type = "all")
# run simulation
ogs5_run(ogs5_obj, ogs_exe = "inst/ogs/ogs_fem")

out_names <- paste0("OUTPUT", c(2:11, 13:16))
# fetch output
ogs5_obj <- ogs5_get_output_specific(ogs5_obj, outbloc_names = out_names)

# === experimental data ========
data("groundwater_exp")
groundwater

# === define function to calculate simulation error ========

f <- function(ogs5_obj, exp_data) {
    # target function to optimize
    # generally some sort of mean squared error

    # data cleaning; transformation to meet structure of exp_data
    ogs5_obj$output <- plyr::ldply(ogs5_obj$output,
                                   function(tbl) {
                                       return(tbl[[1]])
                                   }) %>%
        dplyr::filter(TIME == 1)
    ogs5_obj$output$well <- c(1:10, 12:15)

    if (!all(ogs5_obj$output$well == ogs5_obj$output$well)) {
        stop("data does not match")
    }
    se <- sapply(seq_along(exp_data$well), function(i) {
        ((exp_data$head[i] - ogs5_obj$output$HEAD[i])**2)
    })
    return(mean(se))
}

# check if function works
f(ogs5_obj, groundwater_exp) # should yield a single number

# === declare parameters to calibrate ========

# define range list
calibration_set <- cal_create_calibration_set(
    c("mmp$MEDIUM_PROPERTIES1$PERMEABILITY_TENSOR", "ISOTROPIC", 1.0e-4, 1.0e-2),
    c("mmp$MEDIUM_PROPERTIES2$PERMEABILITY_TENSOR", "ISOTROPIC", 1.0e-9, 1.0e-4),
    c("mmp$MEDIUM_PROPERTIES3$PERMEABILITY_TENSOR", "ISOTROPIC", 1.0e-7, 1.0e-3),
    c("mmp$MEDIUM_PROPERTIES4$PERMEABILITY_TENSOR", "ISOTROPIC", 1.0e-7, 1.0e-3)
)
calibration_set

# sample starting parameters from calibration set
init <- cal_sample_parameters(calibration_set,
                              n_samples = 4,
                              interval_01 = FALSE,
                              scale_fun = log10,
                              unscale_fun = function(x) 10**x,
                              )

options(r2ogs5.default_ogs5_bin = "inst/ogs/ogs_fem")
# === perform Bayesian Optimization ===========
bo <- cal_bayesOpt(par_init = init,
                    kappa = "log_t",
                    max_it = 10,
                    exp_data = groundwater_exp,
                    ogs5_obj = ogs5_obj,
                    outbloc_names = out_names,
                    ogs_exe = NULL,
                    objective_function = f,
                    ensemble_path = "examples/groundwater/ensembles",
                    ensemble_cores = 2,
                    ensemble_name = "test",
                    scale_fun = log10,
                    unscale_fun = function(x) 10**x)


# === reintroduce output object to continue Optimization ===========

bo2 <- cal_bayesOpt(BO_init = bo,
                    kappa = "log_t",
                    max_it = 20,
                    ogs_exe = "inst/ogs/ogs_fem",
                    scale_fun = log10,
                    unscale_fun = function(x) 10**x)

bo$kappa
plot(bo)
plot(bo2)
which.min(bo2$sim_errors)
