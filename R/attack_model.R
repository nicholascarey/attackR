#'@title Attack Model
#'
#'@description *\code{attack_model}* models the visual aspects of an attack by a
#'  predator on a prey
#'
#'  Using parameters such as predator size, speed, shape etc. this function
#'  models the visual aspects of an attack by the predator on a prey. From the
#'  prey's perspective it calculates the visual angle of the attacker
#'  (**alpha**, or **{α}**) in radians, and the rate of change of this angle
#'  (**{dα/dt}** in radians/s), as well as distance and time to capture.
#'
#'@details Information on inputs:
#'
#'@section Input units: *\code{speed}* should be in the same units as body
#'  measurements (length, width etc.) *per second*, e.g. cm/s. It can be either
#'  a single, constant speed value or a vector of speeds (at the correct
#'  *\code{frequency}*). If a single value, *\code{model_length}* is required in
#'  order to calculate the distance the predator starts the attack from. For
#'  example, at a frequency of 60 Hz and *\code{model_length}* of 180, the model
#'  will be three seconds in duration. Therefore, at a constant speed of 600
#'  cm/s, the predator will start from approximately 1800 cm away.
#'
#'  *\code{frequency}* is in *\code{Hz}*. If *\code{speed}* is a vector it
#'  should be at the same frequency.
#'
#'  *\code{model_length}* is required when *\code{speed}* is a single value,
#'  where it determines the total length of the model. It is also used to vary
#'  where on variable speed vectors the predator times reaching the prey, for
#'  example cooincident with its maximum speed, to examine how this affects
#'  factors such as **{dα/dt}**. If left as the default *\code{model_length =
#'  NULL}*, the model ends with the predator reaching the prey on the last speed
#'  value.
#'
#'@section Morphology: In order to correctly model the widest apparent part of
#'  the predator, and thus **{α}**, the function requires several morphometric
#'  inputs:
#'
#'@section *Length and Width*: *\code{body_length}* is simply the total length
#'  from nose to tail (or equivalent). *\code{body_width_v}* and
#'  *\code{body_width_h}* are the **maximum** body widths in the vertical (i.e.
#'  dorsal:ventral) and horizontal (left:right) planes of the body. These must
#'  all be in the same units.
#'
#'@section Body Profiles: *\code{profile_v}* and *\code{profile_h}* are vectors
#'  of widths of the predator's body as a proportion of the maximum width going
#'  from the anterior (nose) to the posterior (tail). Therefore, they should
#'  generally start and end on zero (though they don't have to), and all values
#'  must be between zero and 1. They must be regularly spaced, e.g. every 10\%
#'  along the body. They do not have to be the same length (i.e. resolution),
#'  though it is recommended they are. The longer (i.e. higher resolution) these
#'  measurements are, the better their representation of the morphology of the
#'  animal. Actual values of width along the body are calculated to the
#'  resolution of the entered unit of *\code{body_length}* by linearly
#'  interpolating between each proportional width. For example, if the predator
#'  *\code{body_length}* is 1000cm, a width value in cm is calculated at every
#'  cm along the body by interpolating between the proportional widths.
#'  Therefore, use units which will give an appropriate resolution, at least
#'  greater than the profile vector lengths. The higher the numeric value of the
#'  *\code{body_length}* the better, at least three digits is recommended.
#'
#'  You do not need to enter both *\code{profile_v}* and *\code{profile_h}*,
#'  only one is required; if one is left NULL the model will use the other to
#'  calculate **{α}**. This can be useful if your predator is always wider in
#'  one plane than the other.
#'
#'  The function calculates the widest apparent width of any part of the
#'  predator's body from the prey's perspective at each iteration of the model,
#'  in both planes. This is *usually* the maximum width of the predator, but it
#'  also depends on the predator's shape. At close distances, more anterior
#'  parts of the predator will appear to be wider, and so result in a higher
#'  **{α}** value.
#'
#'  Note: no section of the body posterior of the maximum width can ever show a
#'  larger apparent visual **{α}** than the maximum width. Therefore, the
#'  function only really needs profile data from the maximum width forward.
#'  However, it is important for the function to work correctly that a full,
#'  evenly spaced profile is provided. Having said that, the proportional width
#'  values posterior of the max width can be placeholder values and it will not
#'  affect calculated **{α}**, as long as they are less than 1 and spaced
#'  correctly.
#'
#'@section Filtering of maximum apparent width between body planes: Apparent
#'  widths are calculated for both body planes, if two are entered. Which width
#'  at each body segment (e.g. horizontal or vertical) used to calculate **{α}**
#'  is determined via the *\code{width_filter}* operator. This can be the
#'  midpoint value between them (*\code{"mid"}*, the default), maximum value
#'  (*\code{"max"}*), or minimum value (*\code{"min"}*). You can also choose to
#'  use only the vertical or horizontal profile widths exclusively
#'  (*\code{"v"}*, *\code{"h"}*). You can also choose to use the predator's
#'  maximum width in either plane (*\code{max_width_v}*, *\code{max_width_h}*)
#'  to calculate **{α}**, in which case all other segments of the body are
#'  ignored, and only **{α}** of the maximum width is determined.
#'
#'  *\code{max_width_loc_v}* and *\code{max_width_loc_h}* are the locations of
#'  the maximum widths of the predator occur along the body as a proportion of
#'  the total body length going from the anterior. They are only necessary if
#'  they are not specified as one of the proportional widths in
#'  *\code{profile_v}* and *\code{profile_h}*, that is none of these have the
#'  value of exactly 1. They can also occur at an intermediate section of the
#'  body, not included as part of the body profiles.
#'
#'@section Output: By default *\code{simple_output = TRUE}* the output is a data
#'  frame with every row representing a single instance (*\code{frame}*) of the
#'  model at the set *\code{frequency}*. The data frame contains columns for
#'  frame, speed, time, time reversed, distance of the nose of the predator,
#'  **{α}**, and **{dα/dt}**. If *\code{simple_output = FALSE}* the output is a
#'  *\code{list()}* object given class *\code{attack_model}* containing the
#'  above data frame, plus inputs, subset regions (see next section), and data
#'  detailing how each alpha was calculated (interpolated profiles, locations of
#'  maximum apparent width at each frame, etc.). This option greatly increases
#'  the time the function takes to run.
#'
#'@section Subset regions: *\code{alpha_range}* and *\code{dadt_range}* allow
#'  regions bounded by values of **{α}** and **{dα/dt}** to be subset. Both
#'  inputs are a vector of two numeric values indicating the lower and upper
#'  range of the desired **{α}** and **{dα/dt}** region. The function identifies
#'  the closest matching, first instance of these values in the *\code{$alpha}*
#'  and *\code{dadt}* columns of the model data frame. If *\code{simple_output =
#'  FALSE}*, these can be found as their own elements in the output
#'  *\code{list()}* object. If *\code{plot = TRUE}* these are also plotted as
#'  blue and green shaded regions, respectively.
#'
#'@section Plotting: If *\code{plot = TRUE}* a plot is produced showing *speed*,
#'  **{α}** and **{dα/dt}**. The X range of the plot can set with
#'  *\code{plot_from}* and *\code{plot_to}*. Optional *\code{alpha_range}* and
#'  *\code{dadt_range}* can also be plotted (see above).
#'
#'@section General: Models end when the nose (most anterior part in the
#'  profiles) of the predator reaches the prey (last row of the data.frame,
#'  where time and distance equal zero).
#'
#'  At some point, the maximum girth of the predator will not make up the widest
#'  apparent visual angle of the predator, but more anterior segments will
#'  appear to the prey to be wider, and have a higher **{α}**. After
#'  interpolation of body profiles, filtering of apparent widths (see above),
#'  and identification of maximum apparent width at each iteration of the model,
#'  these final widths and their relative distances from the observing prey are
#'  used to calculate the viewing angle, **{α}**, and used to calculate the rate
#'  of change in **{α}**, **{dα/dt}** in radians/s.
#'
#'
#'@usage attack_model(speed, model_length = NULL, frequency = 60, body_length =
#'  NULL, body_width_v = NULL, body_width_h = NULL, profile_v = NULL, profile_h
#'  = NULL, max_width_loc_v = NULL, max_width_loc_h = NULL, width_filter =
#'  "mid", simple_output = TRUE, plot = TRUE, plot_from = 0, plot_to = NULL,
#'  alpha_range = NULL, dadt_range = NULL)
#'
#'@param speed numeric. Either a single constant speed value or vector of speeds
#'  at the same frequency in Hz as *\code{frequency}*. Must be same unit as
#'  *\code{body_length}* per second. If a data.frame is entered the first
#'  colummn is used. For a constant speed value the function will repeat this
#'  the required number of times at the correct frequency based on
#'  *\code{model_length}*.
#'@param model_length integer. Total length of the model in rows. Required if
#'  *\code{speed}* is a single value, in which case along with frequency it
#'  determines the distance the predator starts at. If *\code{speed}* is a
#'  vector *\code{model_length}* can be left NULL, in which case it is assumed
#'  the predator reaches the prey on the last value, and the length of the speed
#'  vector determines total length of model. Alternatively,
#'  *\code{model_length}* can be used to set a different capture point along the
#'  speed vector, in which case its value must be less than the total length of
#'  *\code{speed}*.
#'@param frequency numeric. Frequency (Hz) of the model, i.e. how many speed and
#'  other measurements per second. Must be same frequency in Hz as
#'  *\code{speed}*.
#'@param body_length numeric. Length of the attacker. Must be same units as
#'  *\code{body_width_v}* and *\code{body_width_h}*, and that used in
#'  *\code{speed}*.
#'@param body_width_v numeric. Maximum width of the attacker in the vertical
#'  plane.
#'@param body_width_h numeric. Maximum width of the attacker in the horizontal
#'  plane.
#'@param profile_v numeric. A vector describing the shape of the attacker in the
#'  vertical plane. See details.
#'@param profile_h numeric. A vector describing the shape of the attacker in the
#'  horizontal plane. See details.
#'@param max_width_loc_v numeric. Location of the maximum girth in the vertical
#'  plane of the predator along the body, if not provided as part of the body
#'  profile inputs. See details.
#'@param max_width_loc_h numeric. Location of the maximum girth in the
#'  horizontal plane of the predator along the body, if not provided as part of
#'  the body profile inputs. See details.
#'@param width_filter string. Filters apparent widths between vertical and
#'  horizontal planes for each row of the model in various ways. See details.
#'@param simple_output logical. Choose structure of output. If TRUE, a simple
#'  data frame of the model is returned, otherwise output is a *\code{list}*
#'  object given an *\code{attack_model}* class, and containing the final model,
#'  input parameters, subset regions, and more.
#'@param plot logical. Choose to plot result.
#'@param plot_from numeric. Time on x-axis to plot from.
#'@param plot_to numeric.  Time on x-axis to plot to.
#'@param alpha_range numeric. Vector of two values of alpha. Optional. These
#'  will appear on any plot as a blue region, and if *\code{simple_output =
#'  FALSE}*, this region of the model is subset out to a separate entry in the
#'  saved *\code{list}* object. If any are not reached in the scenario there
#'  should be a message. If upper range is not reached, it is plotted from lower
#'  value to end of model, i.e. *\code{model_length}* location.
#'@param dadt_range numeric. Vector of two values of alpha. Optional. These will
#'  appear on any plot as a green region, and if *\code{simple_output = FALSE}*,
#'  this region of the model is subset out to a separate entry in the saved
#'  *\code{list}* object. If any are not reached in the scenario there should be
#'  a message. If upper range is not reached, it is plotted from lower value to
#'  end of model, i.e. *\code{model_length}* location.
#'
#'@author Nicholas Carey - \email{nicholascarey@gmail.com}, Dave Cade
#'  \email{davecade@stanford.edu},
#'
#'@importFrom grDevices rgb
#'@importFrom graphics abline axis legend mtext par rect
#'@importFrom stats approx na.omit
#'
#'@export

attack_model <- function(
  speed,
  model_length = NULL,
  frequency = 60,
  body_length = NULL,
  body_width_v = NULL,
  body_width_h = NULL,
  profile_v = NULL,
  profile_h = NULL,
  max_width_loc_v = NULL,
  max_width_loc_h = NULL,
  width_filter = "mid",
  simple_output = TRUE,
  plot = TRUE,
  plot_from = 0,
  plot_to = NULL,
  alpha_range = NULL,
  dadt_range = NULL){


  # Error Checks and Messages -----------------------------------------------

  ## Checks here

  ## speed
  ## If speed single value, require model_length
  if(length(speed) == 1 && is.null(model_length)) stop("For constant speed values a model_length is required")

  ## model_length
  ## Cannot be longer than speed
  if(length(speed) > 1 && !is.null(model_length) && model_length > length(speed)) stop("model_length cannot be longer than the speed vector")
  if(length(speed) > 1 && is.null(model_length)) message("model_length set to final value in speed vector")

  ## body_length
  if(body_length < 100) message("body_length is numerically quite low. For best results in interpolation of widths etc., use a unit that has higher numeric value, \nideally 100 or greater (ish). E.g. if using metres, use centimentres instead. ")

  # Profiles
  # Values must be between 0 and 1
  if(any(profile_v > 1) || any(profile_h > 1)) stop("Body profiles must only contain values between 0 and 1.")
  if(any(profile_v < 0) || any(profile_h < 0)) stop("Body profiles must only contain values between 0 and 1.")
  ## Can't both be NULL
  if(is.null(profile_v) && is.null(profile_h)) stop("Provide at least one body profile.")
  ## Must be over 2 long (nose, mid, tail)
  if((!is.null(profile_v) && length(profile_v) < 3) || (!is.null(profile_h) && length(profile_h) < 3)) stop("Profiles must be at least 3 values long: e.g. nose, midpoint, tail.")
  ## If a profile is empty, message that associated inputs ignored
  if(is.null(profile_v)) message("No vertical body profile (profile_v) found. Any inputs for max_width_loc_v and body_width_v ignored.")
  if(is.null(profile_h)) message("No horizontal body profile (profile_h) found. Any inputs for max_width_loc_h and body_width_h ignored.")
  ## If a profile doesn't contain 1.0, then max_width_loc should be NULL
  #if(any(profile_v == 1) && !is.null(max_width_loc_v)) stop("profile_v already contains a max girth location (value of 1.0). max_width_loc_v cannot also be specified.")
  #if(any(profile_h == 1) && !is.null(max_width_loc_h)) stop("profile_h already contains a max girth location (value of 1.0). max_width_loc_h cannot also be specified.")
  ## And vice versa - if no 1.0 in profile, then mac_girth_loc required
  if(!is.null(profile_v) && !any(profile_v == 1) && is.null(max_width_loc_v)) stop("No max girth location (value of 1.0) found in profile_v. Please specify one with max_width_loc_v.")
  if(!is.null(profile_h) && !any(profile_h == 1) && is.null(max_width_loc_h)) stop("No max girth location (value of 1.0) found in profile_h. Please specify one with max_width_loc_h.")

  ## max_width_loc
  ## Must be between 0 and 1 (if entered)
  if(!is.null(max_width_loc_v) && (max_width_loc_v >= 1 || max_width_loc_v <= 0)) stop("Max width locations must be between 0 and 1. They represent a proportional distance along the length from the nose.")
  if(!is.null(max_width_loc_h) && (max_width_loc_h >= 1 || max_width_loc_h <= 0)) stop("Max width locations must be between 0 and 1. They represent a proportional distance along the length from the nose.")

  ## width_filter
  if(!(width_filter %in% (c("mid", "max", "min", "v", "h", "max_width_v", "max_width_h")))) stop("width_filter input not recognised.")


  ## body_length
  if(is.null(body_length)) stop("Please enter a body_length.")


  ## body_width
  if(!is.null(profile_v) && is.null(body_width_v)) stop("Please enter a body_width_v.")
  if(!is.null(profile_h) && is.null(body_width_h)) stop("Please enter a body_width_h.")


  # Save inputs -------------------------------------------------------------
  # ## put all inputs into a list for inclusion in final output
  inputs <- list(
    speed = speed,
    model_length = model_length,
    frequency = frequency,
    body_length = body_length,
    body_width_v = body_width_v,
    body_width_h = body_width_h,
    profile_v = profile_v,
    max_width_loc_v = max_width_loc_v,
    profile_h = profile_h,
    max_width_loc_h = max_width_loc_h,
    width_filter = width_filter,
    simple_output = simple_output,
    plot = plot,
    plot_from = plot_from,
    plot_to = plot_to,
    alpha_range = alpha_range,
    dadt_range = dadt_range)


  # Fix speed if dataframe ------------------------------------------------------

  ## If speed is a dataframe, make it a vector of FIRST column
  if(is.data.frame(speed)){
    speed <- speed[,1]}


  # v and h profile copying -------------------------------------------------

  # If one of the profiles is empty, just copy to the other. ----------------
  ## same with two other _v and _h settings
  ## Duplicates a lot of calcs, but avoids code breaking

  if(is.null(profile_h)){
    profile_h <- profile_v
    body_width_h <- body_width_v
    max_width_loc_h <- max_width_loc_v}

  if(is.null(profile_v)){
    profile_v <- profile_h
    body_width_v <- body_width_h
    max_width_loc_v <- max_width_loc_h}


  # Set prey location along speed profile -----------------------------------
  ## Modify speed to end at model_length

  ## Save original and add time
  ## This is only for plotting later
  if(length(speed) == 1){
    speed_orig <- data.frame(time = seq(0, model_length/60-1/frequency, 1/frequency),
                             speed = rep(speed, model_length))
  } else {
    speed_orig <- data.frame(time = seq(0, length(speed)/60-1/frequency, 1/frequency),
                             speed = speed)
  }

  ## Truncate (or replicate) speed to model_length if model_length not NULL
  if(length(speed) == 1) speed <- rep(speed, model_length)
  if(length(speed) > 1 && !is.null(model_length)) speed <- speed[1:model_length]
  if(length(speed) > 1 && is.null(model_length)) model_length <- length(speed)

  # Calculate start distances -----------------------------------------------

  ## Calculate the start_distance using the speed vector
  ## Remove last value because it is a derivative.
  start_distance <- sum((speed[-length(speed)]/frequency))

  # Create model ------------------------------------------------------------

  ## Build up model as dataframe by column

  ## frame
  model_data <- data.frame(frame = seq(1, length(speed), 1))

  ## speed profile
  model_data$speed <- speed

  ## Time and time reversed (in seconds)
  model_data$time <- seq(0, nrow(model_data)/60-1/frequency, 1/frequency)
  model_data$time_rev <- rev(model_data$time)


  # Distances ---------------------------------------------------------------

  ## Prey distance from nose tip (assumed capture point)
  model_data$distance_nose <-
    c(start_distance,
      start_distance-(cumsum(model_data$speed[-length(model_data$speed)]/frequency)))

  # Widths ------------------------------------------------------------------

  ## This section takes the two body profiles, incorporates max_width_loc if
  ## it isn't in either, and interpolates linearly between each segment.
  ## Then it works out a final width - either mean/max/min

  ## Widths at resolution of body_length unit
  widths_v <- interpolate_widths(profile_v, max_width_loc_v, body_length, body_width_v)
  widths_h <- interpolate_widths(profile_h, max_width_loc_h, body_length, body_width_h)

  widths_df <- data.frame(widths_v = widths_v,
                          widths_h = widths_h)

  ## Filter widths based on width_filter input
  ## And add distance from nose
  if(width_filter == "mid") {
    widths <- apply(widths_df, 1, function(x) mean(x))
    widths <- data.frame(dist_from_nose = (1:length(widths))-1,
                         width = widths)}

  if(width_filter == "max") {
    widths <- apply(widths_df, 1, function(x) max(x))
    widths <- data.frame(dist_from_nose = (1:length(widths))-1,
                         width = widths)}

  if(width_filter == "min") {
    widths <- apply(widths_df, 1, function(x) min(x))
    widths <- data.frame(dist_from_nose = (1:length(widths))-1,
                         width = widths)}

  if(width_filter == "v") {
    widths <- widths_df[[1]]
    widths <- data.frame(dist_from_nose = (1:length(widths))-1,
                         width = widths)}

  if(width_filter == "h") {
    widths <- widths_df[[2]]
    widths <- data.frame(dist_from_nose = (1:length(widths))-1,
                         width = widths)}

  if(width_filter == "max_width_v") {
    ## location of max_width
    max_width_index_v <- which.max(widths_df[[1]])
    widths <- max(widths_df[[1]])
    widths <- data.frame(dist_from_nose = max_width_index_v-1,
                         width = widths)}

  if(width_filter == "max_width_h") {
    ## location of max_width
    max_width_index_h <- which.max(widths_df[[2]])
    widths <- max(widths_df$widths_h)
    widths <- data.frame(dist_from_nose = max_width_index_h-1,
                         width = widths)}

  ## For every row of model add distance_nose to segment distances
  ## This gives distance of every segment from prey at every iteration of model
  distances_all <- lapply(model_data$distance_nose, function(x) widths$dist_from_nose + x)

  ## Convert these to alpha
  alpha_all <- lapply(distances_all, function(x) calc_alpha(widths$width, x))

  ## Index of max alpha
  ## i.e. what part of body is max alpha at any particular stage
  alpha_max_index <- sapply(alpha_all, function(x) which.max(x))

  ## Max alpha
  ## Max alpha of any body segment at each iteration
  alpha_max <- sapply(alpha_all, function(x) max(x))

  ## Add to model
  model_data$alpha <- alpha_max

  ## Calc dadt
  model_data$dadt <- c(
    NA,
    diff(model_data$alpha) * frequency)

  # Find alpha and dadt ranges ----------------------------------------------

  ## dadt region
  if(is.null(dadt_range)){
    dadt_range_region <- NULL
  } else {
    ## find location of closest match to LOWER dadt_range
    ## which dadt are higher than lower value?
    dadt_range_low_index <- first_closest(dadt_range[1], model_data$dadt)
    ## if it's never reached, set it to NA
    if(length(dadt_range_low_index)==0){
      dadt_range_low_index <- NA
      message("Lower range of dadt_range never reached in this scenario. No dadt_range range plotted.")}

    ## same for UPPER dadt_range range
    ## NOTE - it's third in the vector (mean is second)
    dadt_range_high_index <- first_closest(dadt_range[2], model_data$dadt)
    if(length(dadt_range_high_index)==0){
      dadt_range_high_index <- NA
      message("Upper range of dadt_range never reached in this scenario.")}


    ## Use these to subset model to dadt_range range
    if(is.na(dadt_range_low_index)){
      dadt_range_region <- "No matching dadt_range region in this model"
    } else if (is.na(dadt_range_high_index)) {
      dadt_range_region <- model_data[(dadt_range_low_index-1):model_length,]
    } else {
      dadt_range_region <- model_data[(dadt_range_low_index-1):(dadt_range_high_index+1),]
    }

  }

  ## alpha region
  if(is.null(alpha_range)){
    alpha_range_region <- NULL
  } else {
    ## find location of closest match to LOWER alpha_range
    ## which dadt are higher than lower value?
    alpha_range_low_index <- first_closest(alpha_range[1], model_data$alpha)
    ## if it's never reached, set it to NA
    if(length(alpha_range_low_index)==0){
      alpha_range_low_index <- NA
      message("Lower range of alpha_range never reached in this scenario. No alpha_range range plotted.")}

    ## same for UPPER alpha_range range
    ## NOTE - it's third in the vector (mean is second)
    alpha_range_high_index <- first_closest(alpha_range[2], model_data$alpha)
    if(length(alpha_range_high_index)==0){
      alpha_range_high_index <- NA
      message("Upper range of alpha_range never reached in this scenario.")}

    ## Use these to subset model to alpha_range range
    if(is.na(alpha_range_low_index)){
      alpha_range_region <- "No matching alpha_range region in this model"
    } else if (is.na(alpha_range_high_index)) {
      alpha_range_region <- model_data[(alpha_range_low_index-1):model_length,]
    } else {
      alpha_range_region <- model_data[(alpha_range_low_index-1):(alpha_range_high_index+1),]
    }

  }



  # Assemble final output ---------------------------------------------------
  ## if simple_output = TRUE, output model data frame only
  if(simple_output == TRUE){
    output <- model_data
    ## otherwise assemble output list() object
  } else if(simple_output == FALSE){
    output <- list(
      final_model = model_data,
      inputs = inputs,
      dadt_range_region = dadt_range_region,
      alpha_range_region = alpha_range_region,
      all_data = list(
        widths_interpolated = widths_df,
        widths_filtered = widths,
        distances_per_i = distances_all,
        alpha_per_i = alpha_all,
        body_max_alpha_per_i = alpha_max_index))

    ## Give it a class
    ## Only works for lists, not dataframes
    class(output) <- "attack_model"
  }


  # Plot --------------------------------------------------------------------

  if(plot == TRUE){

    ## make x limits
    if(is.null(plot_to)){
      plot_to <- max(speed_orig$time)
    }
    ## make x limits
    if(is.null(plot_from)){
      plot_from <- 0
    }

    ## set plot parameters - will apply to all unless changed
    ## mgp controls - (axis.title.position, axis.label.position, axis.line.position)
    par(mgp = c(3, 0.5, 0), mar = c(3,1.5,1.5,1.5))

    ## plot complete speed profile

    # colour for all speed plotting
    speed_col <- "grey"

    ## as blank points
    plot(speed~time, data = speed_orig,
         ylim = c(0, max(speed_orig$speed)),
         xlim = c(plot_from, plot_to),
         axes = FALSE,
         pch = ".",
         col = "white",
         ylab = "",
         xlab = "")
    ## add line of speed
    with(speed_orig, lines(x = time, y = speed, lwd = 3, col = speed_col))
    ## add x axis and title
    axis(side = 1, col = "black", lwd = 3, col.axis = "black",
         at = seq(0, max(speed_orig$time)+1, 1))
    mtext("time", side = 1, line = 1.5)
    ## add y axis and title
    axis(side = 2, col = speed_col, lwd = 3, col.axis = speed_col, pos = plot_from, cex.axis = 0.8)


    ## plot alpha
    par(new=T)
    # colour for all alpha plotting
    alpha_col <- "slateblue1"

    plot(alpha~time, data = model_data,
         ylim = c(0, max(model_data$alpha, na.rm = T)),
         xlim = c(plot_from, plot_to),
         pch = ".",
         col = "white",
         axes = FALSE,
         ylab = "",
         xlab = "")
    with(model_data, lines(x = time, y = alpha, col = alpha_col, lwd = 3))
    ## add y axis and title
    axis(side = 2, col = alpha_col, lwd = 3, col.axis = alpha_col,
         pos = plot_from+(0.05*(plot_to-plot_from)), cex.axis = 0.8)

    ## plot dadt
    par(new=T)
    # colour for all alpha plotting
    dadt_col <- "green"
    plot(dadt~time, data = model_data,
         ylim = c(0, max(model_data$dadt, na.rm = T)),
         xlim = c(plot_from, plot_to),
         pch = ".",
         col = "white",
         axes=FALSE,
         ylab = "",
         xlab = "")
    with(model_data, lines(x = time, y = dadt, col = dadt_col, lwd = 3))
    axis(side = 2, col = dadt_col, lwd = 3, col.axis = dadt_col,
         pos = plot_from+(0.1*(plot_to-plot_from)), cex.axis = 0.8)

    ## add dadt_range range

    ## if upper and lower of dadt_range range locations are equal, just draw dashed line
    ## but don't draw it if both equal length of x
    ## this means neither actually occurs
    ## see above (this is hacky - must be a better way)

    if(!is.null(dadt_range)){
      abline(v = model_data$time[dadt_range_low_index],
             col = rgb(15/255,245/255,53/255,  alpha = 0.4),
             lty = 1,
             lwd = 3)

      abline(v = model_data$time[dadt_range_high_index],
             col = rgb(15/255,245/255,53/255,  alpha = 0.4),
             lty = 1,
             lwd = 3)

      rect(xleft = model_data$time[dadt_range_low_index],
           ybottom = -5,
           xright = model_data$time[dadt_range_high_index],
           ytop = max(model_data$dadt, na.rm = T)+5,
           col = rgb(15/255,245/255,53/255,  alpha = 0.2),
           lty = 0)

      ## if dadt_range_high_index is NA, then fill to end
      if(is.na(dadt_range_high_index)){
        rect(xleft = model_data$time[dadt_range_low_index],
             ybottom = -5,
             xright = model_data$time[nrow(model_data)],
             ytop = 100,
             col = rgb(15/255,245/255,53/255,  alpha = 0.2),
             lty = 0)}
    }

    ## add alpha_range range

    ## if upper and lower of alpha_range range locations are equal, just draw dashed line
    ## but don't draw it if both equal length of x
    ## this means neither actually occurs
    ## see above (this is hacky - must be a better way)

    if(!is.null(alpha_range)){
      abline(v = model_data$time[alpha_range_low_index],
             col = rgb(77/255,195/255,255/255,  alpha = 0.4),
             lty = 1,
             lwd = 3)

      abline(v = model_data$time[alpha_range_high_index],
             col = rgb(77/255,195/255,255/255,  alpha = 0.4),
             lty = 1,
             lwd = 3)

      rect(xleft = model_data$time[alpha_range_low_index],
           ybottom = -5,
           xright = model_data$time[alpha_range_high_index],
           ytop = 20,
           col = rgb(77/255,195/255,255/255,  alpha = 0.2),
           lty = 0)

      ## if alpha_range_high_index is NA, then fill to end
      if(is.na(alpha_range_high_index)){
        rect(xleft = model_data$time[alpha_range_low_index],
             ybottom = -5,
             xright = model_data$time[nrow(model_data)],
             ytop = max(model_data$alpha, na.rm = T)+5,
             col = rgb(77/255,195/255,255/255,  alpha = 0.2),
             lty = 0)}
    }

    ## add prey location line
    abline(v=model_data$time[model_length], lty = 3, lwd = 2)

    ## add legend
    legend("topleft", inset=.15,
           c("Speed", "Alpha", "dadt", "prey"),
           text.col = c(speed_col, alpha_col, dadt_col, "black"),
           col = c(speed_col, alpha_col, dadt_col, "black"),
           lty=c(1,1,1,3), lwd = 3, cex=0.8)
  }

  # Return results ----------------------------------------------------------
  return(output)}






