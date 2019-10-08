#'@title Attack Model
#'
#'@description *\code{attack_model_whale}* models the visual aspects of an
#'  attack by a whale on a prey
#'
#'  This function is a customised version of *\code{\link{attack_model}}* which
#'  incorporates the unique changes to a rorqual whale's visual profile caused
#'  by it opening its huge mouth when attacking a school of prey.
#'
#'  It contains several additional inputs relating to the morphology of the
#'  mouth and the timings of its opening, which greatly change the whale's
#'  visual profile.
#'
#'  This help document only contains help on use of the inputs specific to this
#'  function. See *\code{\link{attack_model}}* for description of the others.
#'
#'@details These inputs are used to calculate the apparent width to the prey of
#'  a whale's opening jaws, and this is subsequently used to calculate the
#'  maximum **{α}**.
#'
#'@section *\code{jaw_length}*: The distance of the whale's jaw 'hinge' (i.e.
#'  where upper and lower jaws meet) from the rostrum, in the same units as the
#'  *\code{body_length}*. Can be an exact value or an allometric formula based
#'  on length. For example:\cr
#'
#'  \code{## Humpback Whale jaw location in cm (source:)}\cr \code{jaw_length =
#'  (10^(1.205*log10(hw_bl/100) - 0.880))*100}
#'
#'  \code{## Blue Whale jaw location in cm (source:)}\cr \code{jaw_length =
#'  10^(1.36624*log10(bw_bl/100) - 1.21286)*100}
#'
#'  Note the body length values (*\code{hw_bl}*, *\code{bw_bl}*) must exist
#'  externally; they cannot reference the entered *\code{body_length}* value
#'  internal to the function, unless this also references the same existing
#'  value.
#'
#'@section *\code{jaw_angle_upper}*: This is the angle in radians off the
#'  longitudnal axis of the whale of the upper jaw at maximum gape. In both
#'  humpbacks and blue whales this is 0.5235988 (30°).
#'
#'@section *\code{jaw_angle_lower}*: This is the angle in radians off the
#'  longitudnal axis of the whale of the lower jaw at maximum gape. In both
#'  humpbacks and blue whales this is 0.8726646 (50°).
#'
#'@section *\code{a_, b_ c_, d_} inputs*: *\code{a_mouth_open}* - when the mouth
#'  starts to open \cr *\code{b_max_gape_start}* - when maximum gape is reached
#'  \cr *\code{c_max_gape_end}* - when mouth starts to close, or how long it is
#'  held at max gape \cr *\code{d_mouth_closed}* - when mouth is completely
#'  closed \cr \cr
#'
#'  These inputs set the timings (i.e. iteration, row or frame) of these events
#'  within the model. If *\code{speed}* is a vector, they set the locations
#'  along the speed vector these events occur. Similarly if *\code{speed}* is a
#'  single value, they set similarly the timings within the model, but obviously
#'  this is related to *\code{model_length}*.
#'
#'  The complete mouth opening action does not have to occur during the model.
#'  The inputs can be used to set, for example timing of max gape to be at the
#'  last value in the speed vector. Also, if these are left *\code{NULL}*, the
#'  mouth will not open, and the model is equivalent to one created using
#'  *\code{\link{attack_model}}*.
#'
#'@section Application of the mouth opening and morphology inputs: The function
#'  programatically determines the location of the jaw tips at each iteration of
#'  the model during the mouth opening event, and their distance from the prey,
#'  calculates their visual angle **{α}**, and combines these to give a total
#'  jaw **{α}**. This is then compared to the **{α}** of the rest of the body to
#'  determine the maximum **{α}**. These calculations are done in the vertical
#'  plane only, and occur separately from any **{α}** calculations done using
#'  the body profiles; if the total jaw **{α}** is greater than the **{α}**
#'  determined from the body widths, it will always be selected as the maximum
#'  **{α}** regardless of any filtering between vertical and horizontal planes
#'  using *\code{width_filter}*.
#'
#'@usage attack_model_whale(speed, model_length = NULL, frequency = 60,
#'  body_length = NULL, body_width_v = NULL, body_width_h = NULL, profile_v =
#'  NULL, profile_h = NULL, max_width_loc_v = NULL, max_width_loc_h = NULL,
#'  width_filter = "mid", jaw_length = NULL, jaw_angle_upper = 0.5235988,
#'  jaw_angle_lower = 0.8726646, a_mouth_open = NULL, b_max_gape_start = NULL,
#'  c_max_gape_end = NULL, d_mouth_closed = NULL, simple_output = FALSE, plot =
#'  TRUE, plot_from = 0, plot_to = NULL, alpha_range = NULL, dadt_range = NULL)
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
#'@param   jaw_length numeric. distance of the whale's jaw 'hinge' (i.e. where
#'  upper and lower jaws meet) from the rostrum, in the same units as the
#'  body_length. See details.
#'@param   jaw_angle_upper numeric. Angle in radians off the whale's longitudnal
#'  axis of the upper jaw at maximum gape. See details.
#'@param   jaw_angle_lower numeric. Angle in radians off the whale's longitudnal
#'  axis of the lower jaw at maximum gape. See details.
#'@param   a_mouth_open integer. Iteration of the model (i.e. row, or placement
#'  along the speed profile) where the mouth starts to open. See details.
#'@param   b_max_gape_start integer. Iteration of the model (i.e. row, or placement
#'  along the speed profile) where the mouth has reached max gape. See details.
#'@param   c_max_gape_end integer. Iteration of the model (i.e. row, or placement
#'  along the speed profile) where the mouth starts to close See details.
#'@param   d_mouth_closed integer. Iteration of the model (i.e. row, or placement
#'  along the speed profile) where the mouth has fully closed. See details.
#'@param simple_output logical. Choose structure of output. If TRUE, a simple
#'  data frame of the model is returned, otherwise output is a *\code{list}*
#'  object given an *\code{attack_model_whale}* class, and containing the final
#'  model, input parameters, subset regions, and more.
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
#'@export

attack_model_whale <- function(
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
  jaw_length = NULL,
  jaw_angle_upper = 0.5235988,
  jaw_angle_lower = 0.8726646,
  a_mouth_open = NULL,
  b_max_gape_start = NULL,
  c_max_gape_end = NULL,
  d_mouth_closed = NULL,
  simple_output = FALSE,
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

  ## Profiles
  ## Values must be between 0 and 1
  # if(any(profile_v > 1) || any(profile_h > 1)) stop("Body profiles must only contain values between 0 and 1.")
  # if(any(profile_v < 0) || any(profile_h < 0)) stop("Body profiles must only contain values between 0 and 1.")
  # ## Can't both be NULL
  # if(is.null(profile_v) && is.null(profile_h)) stop("Provide at least one body profile.")
  # ## Must be over 2 long (nose, mid, tail)
  # if((!is.null(profile_v) && length(profile_v) < 3) || (!is.null(profile_h) && length(profile_h) < 3)) stop("Profiles must be at least 3 values long: e.g. nose, midpoint, tail.")
  # ## If a profile is empty, message that associated inputs ignored
  # if(is.null(profile_v)) message("No vertical body profile (profile_v) found. Any inputs for max_width_loc_v and body_width_v ignored.")
  # if(is.null(profile_h)) message("No horizontal body profile (profile_h) found. Any inputs for max_width_loc_h and body_width_h ignored.")
  # ## If a profile doesn't contain 1.0, then max_width_loc should be NULL
  # if(any(profile_v == 1) && !is.null(max_width_loc_v)) stop("profile_v already contains a max girth location (value of 1.0). max_width_loc_v cannot also be specified.")
  # if(any(profile_h == 1) && !is.null(max_width_loc_h)) stop("profile_h already contains a max girth location (value of 1.0). max_width_loc_h cannot also be specified.")
  # ## And vice versa - if no 1.0 in profile, then mac_girth_loc required
  # if(!any(profile_v == 1) && is.null(max_width_loc_v)) stop("No max girth location (value of 1.0) found in profile_v. Please specify one with max_width_loc_v.")
  # if(!any(profile_h == 1) && is.null(max_width_loc_h)) stop("No max girth location (value of 1.0) found in profile_h. Please specify one with max_width_loc_h.")

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
    profile_h = profile_h,
    max_width_loc_v = max_width_loc_v,
    max_width_loc_h = max_width_loc_h,
    width_filter = width_filter,
    jaw_length = jaw_length,
    jaw_angle_upper = jaw_angle_upper,
    jaw_angle_lower = jaw_angle_lower,
    a_mouth_open = a_mouth_open,
    b_max_gape_start = b_max_gape_start,
    c_max_gape_end = c_max_gape_end,
    d_mouth_closed = d_mouth_closed,
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


  # Mouth opening parameters ------------------------------------------------

  ## Is mouth opening?
  if(!is.null(a_mouth_open)) {mouth_opening <- TRUE
  } else {mouth_opening <- FALSE}

  ## If a-d are NULL, just make jaw XZ all zeros
  ## This will keep mouth closed
  if(!mouth_opening){
    message("Whale model with mouth NOT opening... ")

    ## set jaw coords all to zero to length of model
    up_X <- rep(0, length(speed))
    up_Z <- up_X
    low_X <- up_X
    low_Z <- up_X

    ## Also make inputs zero - for plotting later
    a_mouth_open <- 0
    b_max_gape_start <- 0
    c_max_gape_end <- 0
    d_mouth_closed <- 0

  } else {
    message("Whale model with mouth opening... ")

    ## rename input variables
    a <- a_mouth_open
    b <- b_max_gape_start
    c <- c_max_gape_end
    d <- d_mouth_closed
    ## extra term - for if mouth closes BEFORE end of speed vector
    e <- length(speed)

    ## Create mouth open XZ
    # empty vector
    up_X <- c()
    # mouth closed - fill zeros to a
    up_X[1:a] <- 0
    # mouth opens - fill a to b
    up_X[a:b] <- (jaw_length - cos(((a:b)-a)/(b-a)*jaw_angle_upper)*jaw_length) # % in cm
    # mouth held at max gape - repeat last value to c
    up_X[b:c] <- up_X[b]
    # mouth closes - fill c to d
    up_X[c:d] <- (jaw_length - cos(((d:c)-c)/(d-c)*jaw_angle_upper)*jaw_length) # % in cm
    # if mouth closes before end of vector, fill in zeros
    if(e > d){up_X[d:e] <- 0}
    # truncate to same length as speed/model_length
    up_X <- up_X[1:model_length]

    ## same for upper jaw Z
    up_Z <- c()
    up_Z[1:a] <- 0
    up_Z[a:b] <- sin(((a:b)-a)/(b-a)*jaw_angle_upper)*jaw_length
    up_Z[b:c] <- up_Z[b]
    up_Z[c:d] <- sin(((d:c)-c)/(d-c)*jaw_angle_upper)*jaw_length
    if(e > d){up_Z[d:e] <- 0}
    up_Z <- up_Z[1:model_length]

    ## same for lower jaw X
    low_X <- c()
    low_X[1:a] <- 0
    low_X[a:b] <- (jaw_length - cos(((a:b)-a)/(b-a)*jaw_angle_lower)*jaw_length)
    low_X[b:c] <- low_X[b]
    low_X[c:d] <- (jaw_length - cos(((d:c)-c)/(d-c)*jaw_angle_lower)*jaw_length)
    if(e > d){low_X[d:e] <- 0}
    low_X <- low_X[1:model_length]

    ## same for lower jaw Z
    low_Z <- c()
    low_Z[1:a] <- 0
    low_Z[a:b] <- sin(((a:b)-a)/(b-a)*jaw_angle_lower)*jaw_length
    low_Z[b:c] <- low_Z[b]
    low_Z[c:d] <- sin(((d:c)-c)/(d-c)*jaw_angle_lower)*jaw_length
    if(e > d){low_Z[d:e] <- 0}
    low_Z <- low_Z[1:model_length]
  }

  # Calculate start distances -----------------------------------------------

  ## Calculate the start_distance using the speed vector
  ## Remove last value because it is a derivative.
  ## This is of the jaw
  ## Takes into account jaw moving backwards due to it opening at the end of the
  ## model. That is start_distance would otherwise be larger - further back.
  start_distance <- sum((speed[-length(speed)]/frequency)) - low_X[model_length]


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

  ## Prey distance from nose tip

  ## This is assuming mouth stays closed. Therefore towards end of the model
  ## some of these will go past the prey and will need filtered out
  model_data$distance_nose <-
    c(start_distance,
      start_distance-(cumsum(model_data$speed[-length(model_data$speed)]/frequency)))

  ## Prey distance from low jaw tip (assumed capture point)
  ## Adds low_X - distance the jaw has moved backwards due to opening
  model_data$distance_low_jaw <-
    c(start_distance,
      start_distance-(cumsum(model_data$speed[-length(model_data$speed)]/frequency))) + low_X

  model_data$distance_up_jaw <-
    c(start_distance,
      start_distance-(cumsum(model_data$speed[-length(model_data$speed)]/frequency))) + up_X

  ## Calc alpha of both jaws plus total jaw alpha
  ## Replace any values above pi/2 for half jaw
  alpha_up_jaw <- atan2(up_Z, model_data$distance_up_jaw)
  alpha_up_jaw <- replace(alpha_up_jaw, alpha_up_jaw > pi/2, pi/2)

  alpha_low_jaw <- atan2(low_Z, model_data$distance_low_jaw)
  alpha_low_jaw <- replace(alpha_low_jaw, alpha_low_jaw > pi/2, pi/2)

  alpha_total_jaw <- alpha_up_jaw + alpha_low_jaw

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
                         width = widths)
    ## if mouth is opening, only use widths up to jaw
    if(mouth_opening) widths[1:round(jaw_length),2] <- NA}


  if(width_filter == "max") {
    widths <- apply(widths_df, 1, function(x) max(x))
    widths <- data.frame(dist_from_nose = (1:length(widths))-1,
                         width = widths)
    if(mouth_opening) widths[1:round(jaw_length),2] <- NA}

  if(width_filter == "min") {
    widths <- apply(widths_df, 1, function(x) min(x))
    widths <- data.frame(dist_from_nose = (1:length(widths))-1,
                         width = widths)
    if(mouth_opening) widths[1:round(jaw_length),2] <- NA}

  if(width_filter == "v") {
    widths <- widths_df[[1]]
    widths <- data.frame(dist_from_nose = (1:length(widths))-1,
                         width = widths)
    if(mouth_opening) widths[1:round(jaw_length),2] <- NA}

  if(width_filter == "h") {
    widths <- widths_df[[2]]
    widths <- data.frame(dist_from_nose = (1:length(widths))-1,
                         width = widths)
    if(mouth_opening) widths[1:round(jaw_length),2] <- NA}

  if(width_filter == "max_width_v") {
    ## location of max_width - first one in case of multiple matches
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

  ## Max alpha of any body segment at each iteration
  alpha_max <- sapply(alpha_all, function(x) max(x, na.rm = TRUE))

  ## max of all body segemnts or total jaw
  alpha_max <- pmax(alpha_max, alpha_total_jaw, na.rm = TRUE)

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
    dadt_range_low_index <- first_over(dadt_range[1], model_data$dadt)
    ## if it's never reached, set it to NA
    if(length(dadt_range_low_index)==0){
      dadt_range_low_index <- NA
      message("Lower range of dadt_range never reached in this scenario. No dadt_range range plotted.")}

    ## same for UPPER dadt_range range
    ## NOTE - it's third in the vector (mean is second)
    dadt_range_high_index <- first_over(dadt_range[2], model_data$dadt)
    if(length(dadt_range_high_index)==0){
      dadt_range_high_index <- NA
      message("Upper range of dadt_range never reached in this scenario.")}


    ## Use these to subset model to dadt_range range
    if(is.na(dadt_range_low_index)){
      dadt_range_region <- "No matching dadt_range region in this model"
    } else if (is.na(dadt_range_high_index)) {
      dadt_range_region <- model_data[dadt_range_low_index:model_length,]
    } else {
      dadt_range_region <- model_data[dadt_range_low_index:dadt_range_high_index,]
    }

  }

  ## alpha region
  if(is.null(alpha_range)){
    alpha_range_region <- NULL
  } else {
    ## find location of closest match to LOWER alpha_range
    ## which dadt are higher than lower value?
    alpha_range_low_index <- first_over(alpha_range[1], model_data$alpha)
    ## if it's never reached, set it to NA
    if(length(alpha_range_low_index)==0){
      alpha_range_low_index <- NA
      message("Lower range of alpha_range never reached in this scenario. No alpha_range range plotted.")}

    ## same for UPPER alpha_range range
    ## NOTE - it's third in the vector (mean is second)
    alpha_range_high_index <- first_over(alpha_range[2], model_data$alpha)
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
        body_max_alpha_per_i = alpha_max_index,
        alpha_up_jaw = alpha_up_jaw,
        alpha_low_jaw = alpha_low_jaw,
        alpha_total_jaw = alpha_total_jaw))

    ## Give it a class
    ## Only works for lists, not dataframes
    class(output) <- "attack_model_whale"
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
    ## add points of mouth open
    with(speed_orig, points(x = time[a_mouth_open:b_max_gape_start],
                            y = speed[a_mouth_open:b_max_gape_start],
                            pch = "*",
                            col = "red"))
    ## add points of max gape
    with(speed_orig, points(x = time[b_max_gape_start:c_max_gape_end],
                            y = speed[b_max_gape_start:c_max_gape_end],
                            pch = "*",
                            col = "gold2"))
    ## add points of mouth closing
    with(speed_orig, points(x = time[c_max_gape_end:d_mouth_closed],
                            y = speed[c_max_gape_end:d_mouth_closed],
                            pch = "*",
                            col = "darkgreen"))
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
           c("Speed", "Alpha", "dadt", "prey", "mouth opening", "max gape", "mouth closing"),
           text.col = c(speed_col, alpha_col, dadt_col, "black", "red", "gold2", "darkgreen"),
           col = c(speed_col, alpha_col, dadt_col, "black", "red", "gold2", "darkgreen"),
           lty=c(1,1,1,3, NA, NA, NA),
           pch = c("*", "*", "*"),
           lwd = 3, cex=0.8)
  }

  # Return results ----------------------------------------------------------
  return(output)}







