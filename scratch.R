## checks

## profiles
## no value can be greater than 1
## profile_v and profile_h same length
## or else one is NULL

## max_width_loc
## both should be between 0 and 1
## Both or either can be NULL - IF 1.0 occurs within profile_v/profile_h

## model_length
## Required if x is a single value
## If x is vecotr and model_length is NULL - assumed ends on last value of vector
## CHECK - model_length cannot be longer than x

## smooth_profile
## CHECK = mean/mid/min/max

## dAdt_range = only 2 long
## alpha_range = only 2 long

## body_length should be greater than total length of vectors.


## plot
## if plot_from = NULL make it zero


## Set speed
## either constant value or vector
## if single constant value, a start distance (of nose) is required, and model will end when nose reaches prey
## i.e. start_distance becomes zero
## If vector - assumes ends on last value
## If you want to have the prey captured at different poitn along a speed vector, truncate it before entry
speed <- 500
model_length <- NULL

## Set total body length
body_length <- 1050

## Set where the maximum width occurs along the body from the nose as proportion of body length
body_width_v <- 250.669
body_width_h <- 250.669



profile_v <- c(0.0, 0.2, 0.4, 0.6, 0.8, 0.8, 0.6, 0.4, 0.2, 0.0)
max_width_loc_v <- 0.5

profile_v <- c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0.0)
max_width_loc_v <- 0.5

profile_v <- c(0.000, 0.344, 0.558, 0.676, 0.721, 0.777, 0.880, 0.931, 0.950, 0.900, 0.800,
               0.700, 0.600, 0.500, 0.400, 0.300, 0.250, 0.200, 0.150, 0.100, 0.000)
profile_h <- c(0.000, 0.329, 0.443, 0.595, 0.708, 0.803, 0.847, 0.904, 0.950, 0.900, 0.800,
               0.700, 0.600, 0.500, 0.400, 0.300, 0.250, 0.200, 0.150, 0.100, 0.000)

## Set where the maximum width occurs along the body from the nose as proportion of body length
## Can be left NULL if this occurs as part of profiles
max_width_loc_v <- 0.42 ## i.e. 42% along the body from the nose
max_width_loc_h <- 0.42 ## one can be left NULL, in which case they are assumed equal


## v or h width selection method (min, mean/midpoint, max)
## When vertical and horiz alphas differ how to choose which one to use in the model
filter_method <- "mean"





