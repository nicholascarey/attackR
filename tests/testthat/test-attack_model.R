# library(testthat)


# Profiles ----------------------------------------------------------------

## stops if profiles contain values outside correct range
expect_error(attack_model(500, 180, 60, 1000, 250, 250,
                          profile_v = c(0, 0.4, 0.8, 1.2),
                          profile_h = c(0, 0.4, 0.8)),
             "Body profiles must only contain values between 0 and 1.")

expect_error(attack_model(500, 180, 60, 1000, 250, 250,
                          profile_v = c(0, 0.4, 0.8),
                          profile_h = c(0, 0.4, 0.8, 1.2)),
             "Body profiles must only contain values between 0 and 1.")

expect_error(attack_model(500, 180, 60, 1000, 250, 250,
                          profile_v = c(0, 0.4, 0.8, -0.2),
                          profile_h = c(0, 0.4, 0.8)),
             "Body profiles must only contain values between 0 and 1.")

expect_error(attack_model(500, 180, 60, 1000, 250, 250,
                          profile_v = c(0, 0.4, 0.8),
                          profile_h = c(0, 0.4, 0.8, -0.2)),
             "Body profiles must only contain values between 0 and 1.")

## stops if no profile entered
expect_error(attack_model(500, 180, 60, 1000, 250, 250,
                          profile_v = NULL,
                          profile_h = NULL,
             "Provide at least one body profile."))

## stops if profiles less than 3
expect_error(attack_model(500, 180, 60, 1000, 250, 250,
                          profile_v = c(0.2,0.3),
                          profile_h = NULL,
             "Provide at least one body profile."))
expect_error(attack_model(500, 180, 60, 1000, 250, 250,
                          profile_v = NULL,
                          profile_h = c(0.2,0.3),
             "Provide at least one body profile."))
expect_error(attack_model(500, 180, 60, 1000, 250, 250,
                          profile_v = c(0.2),
                          profile_h = c(0.2,0.3),
             "Provide at least one body profile."))


# Max Width Locations -----------------------------------------------------

expect_error(attack_model(500, 180, 60, 1000, 250, 250,
                          profile_v = c(0.2, 0.3, 0.4, 0.5),
                          profile_h = c(0.2, 0.3, 0.4, 0.5),
                          max_width_loc_v = -0.5,
                          "Max width locations must be between 0 and 1"))

expect_error(attack_model(500, 180, 60, 1000, 250, 250,
                          profile_v = c(0.2, 0.3, 0.4, 0.5),
                          profile_h = c(0.2, 0.3, 0.4, 0.5),
                          max_width_loc_v = 1.5,
                          "Max width locations must be between 0 and 1"))

expect_error(attack_model(500, 180, 60, 1000, 250, 250,
                          profile_v = c(0.2, 0.3, 0.4, 0.5),
                          profile_h = c(0.2, 0.3, 0.4, 0.5),
                          max_width_loc_h = -0.5,
                          "Max width locations must be between 0 and 1"))

expect_error(attack_model(500, 180, 60, 1000, 250, 250,
                          profile_v = c(0.2, 0.3, 0.4, 0.5),
                          profile_h = c(0.2, 0.3, 0.4, 0.5),
                          max_width_loc_h = 1.5,
                          "Max width locations must be between 0 and 1"))

#
#
#
#
# # test --------------------------------------------------------------------
#
# ## test list is created
# expect_output(str(constant_speed_model()), "List of 6")
#
# # test --------------------------------------------------------------------
#
# ## test class
# model <- constant_speed_model()
# expect_is(model, "constant_speed_model")
#
# # test --------------------------------------------------------------------
#
# ## test works with default values
# # create model
# mod <- constant_speed_model()
#
# # test specific values for default inputs
# expect_equal(nrow(mod$model), 120)
# expect_equal(round(mod$model$distance[120], 2), 0)
# expect_equal(mod$model$time[120], 2)
#
#
#
