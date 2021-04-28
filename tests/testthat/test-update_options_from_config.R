# update_options_from_config ----------------------------------------------

test_that("update_options_from_config returns values",{
    expect_false(update_options_from_config(file = "XXX"))
    expect_false(update_options_from_config(file = "config_crash.yml"))
    expect_true(update_options_from_config(file = "config_works.yml"))
})
