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

test_that("cal_change_parameters cannot create input, only change",
          expect_error(cal_change_parameters(ex1, par_df = wrong_df))
          )
