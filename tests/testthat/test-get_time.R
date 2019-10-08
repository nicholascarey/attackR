# library(testthat)

## Create test models
## attack_model simple and full
sl_mod_simp <- attack_model(speed = 550,
                       body_length = 167,
                       profile_v = c(0.00, 0.29, 0.47, 0.61, 0.66, 0.77, 0.88, 0.94, 1.00,
                                     0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.00),
                       body_width_v = 66.8,
                       profile_h = c(0.00, 0.34, 0.44, 0.49, 0.56, 0.69, 0.83, 0.93,1.00,
                                     0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.00),
                       body_width_h = 66.8,
                       model_length = 601,
                       plot_from = 9,
                       plot_to = 10.5,
                       width_filter = "mid",
                       simple_output = TRUE)

sl_mod_full <- attack_model(speed = 550,
                       body_length = 167,
                       profile_v = c(0.00, 0.29, 0.47, 0.61, 0.66, 0.77, 0.88, 0.94, 1.00,
                                     0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.00),
                       body_width_v = 66.8,
                       profile_h = c(0.00, 0.34, 0.44, 0.49, 0.56, 0.69, 0.83, 0.93,1.00,
                                     0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.00),
                       body_width_h = 66.8,
                       model_length = 601,
                       plot_from = 9,
                       plot_to = 10.5,
                       width_filter = "mid",
                       simple_output = FALSE)

## WHALE attack_model_whale simple and full

# Blue Whale Model --------------------------------------------------------

bl_whale <- 2280
prof_v <- c(0.00,1,1,1,1,1,1,1,1,1,0) # 82 - 100

whale_mod_simp <- attack_model_whale(
  speed = bw_speeds.rd[1:100],
  model_length = 100,
  body_length = bl_whale,
  body_width_v = ((bl_whale*0.434)/pi),
  profile_v = prof_v,
  jaw_length = 10^(1.36624*log10(bl_whale/100) - 1.21286)*100,
  jaw_angle_upper = 30,
  jaw_angle_lower = 50,
  simple_output = TRUE,
  plot = FALSE)

whale_mod_full <- attack_model_whale(
  speed = bw_speeds.rd[1:100],
  model_length = 100,
  body_length = bl_whale,
  body_width_v = ((bl_whale*0.434)/pi),
  profile_v = prof_v,
  jaw_length = 10^(1.36624*log10(bl_whale/100) - 1.21286)*100,
  jaw_angle_upper = 30,
  jaw_angle_lower = 50,
  simple_output = FALSE,
  plot = FALSE)



## stops with both alpha an dadt inputs
expect_error(get_time(sl_mod_simp, dadt = 1, alpha = 2),
             "Enter only one of dadt or alpha, not both.")
## stops with no alpha or dadt inputs
expect_error(get_time(sl_mod_simp, dadt = NULL, alpha = NULL),
             "Enter a dadt or alpha value.")
## stops with too high alpha or dadt inputs
expect_error(get_time(sl_mod_simp, dadt = 20, alpha = NULL),
             "target value never reached in this vector")
## stops with too high alpha or dadt inputs
expect_error(get_time(sl_mod_simp, dadt = NULL, alpha = 20),
             "target value never reached in this vector")

## Accepts both simple and full attack_model and attack_model_whale
expect_error(suppressMessages(get_time(sl_mod_simp, dadt = 0.5, alpha = NULL),
             NA))
expect_error(suppressMessages(get_time(sl_mod_full, dadt = 0.5, alpha = NULL),
             NA))
expect_error(suppressMessages(get_time(whale_mod_simp, dadt = 0.5, alpha = NULL),
             NA))
expect_error(suppressMessages(get_time(whale_mod_full, dadt = 0.5, alpha = NULL),
             NA))


## Correct values returned
expect_equal(suppressMessages(get_time(sl_mod_simp, dadt = 0.5, alpha = NULL)),
             tolerance = 0.0001, 0.3666667)
expect_equal(suppressMessages(get_time(sl_mod_full, dadt = 0.5, alpha = NULL)),
             tolerance = 0.0001, 0.3666667)
expect_equal(suppressMessages(get_time(whale_mod_simp, dadt = 0.5, alpha = NULL)),
             0.51666667)
expect_equal(suppressMessages(get_time(whale_mod_full, dadt = 0.5, alpha = NULL)),
             0.51666667)
expect_equal(suppressMessages(get_time(sl_mod_simp, dadt = NULL, alpha = 0.5)),
             0.11666667)
expect_equal(suppressMessages(get_time(sl_mod_full, dadt = NULL, alpha = 0.5)),
             0.11666667)
expect_equal(suppressMessages(get_time(whale_mod_simp, dadt = NULL, alpha = 0.5)),
             1.65)
expect_equal(suppressMessages(get_time(whale_mod_full, dadt = NULL, alpha = 0.5)),
             1.65)

## Correct messages
expect_message(get_time(sl_mod_simp, dadt = NULL, alpha = 0.5),
               "attack_model input - time is from nose")
expect_message(get_time(whale_mod_simp, dadt = NULL, alpha = 0.5),
               "attack_model_whale input - time is from low jaw tip")

