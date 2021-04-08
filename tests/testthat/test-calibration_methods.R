context("Calibration Methods")

df <- data.frame(file_ext = "mmp",
                 mkey = "MEDIUM_PROPERTIES1",
                 skey = "PERMEABILITY_TENSOR",
                 spec = "ISOTROPIC",
                 min = 1e-10,
                 max = 1e-1,
                 value = 5e-2)

wrong_df <- data.frame(file_ext = "mmp",
                 mkey = "MEDIUM_PROPERTIES1",
                 skey = "HAPPYNESS",
                 spec = "4real",
                 min = 1e-10,
                 max = 1e-1,
                 value = 5e-2)
test_that("transformation function has its inverse function", {

          expect_equal(
              from01(
                  to01(df,
                       unscale_fun = function(x) {10**x},
                       scale_fun = log10),

                  unscale_fun = function(x) {10**x},
                  scale_fun = log10)$value,
              df$value)
          })

test_that("transformation behaves as expected", {
    expect_equal(to01(df,
                      unscale_fun = function(x) {10**x},
                      scale_fun = log10,
                      scale_which = "ISOTROPIC")$value, 0.5)
})

test_that("cal_change_parameters cannot create input, only change", {
          expect_error(cal_change_parameters(ex1,
                                             par_df = wrong_df),
                       regexp = paste0("Specified keys in row ", 1,
                                       " of par_df do not exist in ogs5_obj"))
})
test_that("check length of cal_create_calibration_set", {
    expect_error(
    cal_create_calibration_set((c("mmp$MEDIUM_PROPERTIES1$PERMEABILITY_TENSOR",
                                  "ISOTROPIC", 1.0e-4))),
    regexp = "Argument 1 should be a a vector of length 4")
})


context("cal_bayesOpt")

test_that("kappa input is checked", {
    expect_error(cal_bayesOpt(kappa = -1),
                 regexp = "kappa must be either one of \"log_t\" or \"cooling\",
                a positive number or a function that accepts arguments d and i")
    expect_error(cal_bayesOpt(kappa = "Sam>Frodo"),
                 regexp = "kappa must be either one of \"log_t\" or \"cooling\",
                a positive number or a function that accepts arguments d and i")
    expect_error(cal_bayesOpt(kappa = function(s, i){}),
                 regexp = "kappa must be either one of \"log_t\" or \"cooling\",
                a positive number or a function that accepts arguments d and i")
})

test_that("check BO_init class", {
          expect_error(cal_bayesOpt(BO_init = list("a", 4),
                            target_function = function(ogs5_obj, exp_data){}),
                       regexp = "BO_init must be of class BO")
    })

test_that("user function sanity", {
    expect_error(cal_bayesOpt(target_function = function(ogs5_obj, ex){NA}),
                regexp = "target_function must have two arguments named
                \"ogs5_obj\" and \"exp_data\".")
    expect_error(cal_bayesOpt(par_init = df,
                              target_function = function(ogs5_obj, exp_data){},
                              scale_fun = log10,
                              unscale_fun = function(x){x**10}),
                 regexp = paste("scale_fun should be the inverse function",
                 "of unscale_fun"))
    })

