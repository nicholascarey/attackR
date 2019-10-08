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
                          profile_h = NULL),
             "Provide at least one body profile.")

## stops if profiles less than 3
expect_error(attack_model(500, 180, 60, 1000, 250, 250,
                          profile_v = c(0.2,0.3),
                          profile_h = NULL),
             "Profiles must be at least 3 values long: e.g. nose, midpoint, tail.")
expect_error(attack_model(500, 180, 60, 1000, 250, 250,
                          profile_v = NULL,
                          profile_h = c(0.2,0.3)),
             "Profiles must be at least 3 values long: e.g. nose, midpoint, tail.")
expect_error(attack_model(500, 180, 60, 1000, 250, 250,
                          profile_v = c(0.2),
                          profile_h = c(0.2,0.3)),
             "Profiles must be at least 3 values long: e.g. nose, midpoint, tail.")


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


# Output ------------------------------------------------------------------

## list is created when simple_output = FALSE
expect_output(str(attack_model(500, 180, 60, 1000, 250, 250,
                               profile_v = c(0.2, 0.3, 1, 0.5),
                               profile_h = c(0.2, 0.3, 1, 0.5),
                               simple_output = FALSE)),
                  "List of 5")
## data frame is created when simple_output = TRUE
expect_output(str(attack_model(500, 180, 60, 1000, 250, 250,
                               profile_v = c(0.2, 0.3, 1, 0.5),
                               profile_h = c(0.2, 0.3, 1, 0.5),
                               simple_output = TRUE)),
                  "data.frame")

## test class when simple_output = FALSE
model <- attack_model(500, 180, 60, 1000, 250, 250,
                      profile_v = c(0.2, 0.3, 1, 0.5),
                      profile_h = c(0.2, 0.3, 1, 0.5),
                      simple_output = FALSE)
expect_is(model, "attack_model")

