#' @title Attack Model
#'
#' @description \code{attack_model} models the visual aspects of an attack by a
#'   predator on a prey
#'
#'   Models the visual aspects of an attack by a predator on a prey using
#'   parameters such as size, speed, shape etc. From the prey perspective, it
#'   calculates the visual angle of the diameter of the attacker (alpha, or
#'   \strong{a}) in radians, and the rate of change of this angle
#'   (\strong{da/dt} in radians/s).
#'
#'   Input units: \code{frequency} is in \code{Hz}. If \code{speed} is a vector
#'   it should be at the same frequency. Body measurements and speed units can
#'   be any unit, but should be consistent. i.e. all in cm, and speeds in cm/s,
#'   etc.
#'
#'   \code{speed} can be either a constant value or vector of speeds (at the
#'   correct frequency). If a single constant value, a \code{model_length} is
#'   required in order to calculate the distance the predator starts the attack
#'   from. For example, at a frequency of 60 Hz and \code{model_length} of 180,
#'   the model will be three seconds in duration. Therefore, at a constant speed
#'   of 600 cm/s, the predator will start from approximately 1800 cm away.
#'
#'   The \code{model_length} operator can also be used to vary where on variable
#'   speed vectors the predator times reaching the prey, to examine how this
#'   affects perceived alphas. If left as the default \code{model_length =
#'   NULL}, the model ends with the predator reaching the prey on the last speed
#'   value, however \code{model_length} can be used to set this to be earlier in
#'   the model.
#'
#'   Models end when the nose (most anterior part in the profiles) of the
#'   predator reaches the prey (last row of the data.frame, where time and
#'   distance equal zero).
#'
#' @details In order to correctly model the widest apparent part of the predator
#'   it requires several morphometric inputs:
#'
#' @section Length and Width: \code{body_length} is simply the total length from
#'   nose to tail (or equivalent). \code{body_width_v} and  \code{body_width_h}
#'   are the *maximum* body widths in the vertical (i.e. dorsal:ventral) and
#'   horizontal (left:right) vectors of the body. These must be in the same
#'   units.
#'
#' @section Body Profiles: \code{profile_v} and \code{profile_h} are vectors of
#'   widths of the predator's body as a proportion of the maximum width going
#'   from the anterior (nose) to the posterior (tail). Therefore, they should
#'   generally start and end on zero (though they don't have to), and all values
#'   must be between zero and 1. They must be evenly spaced, e.g. every 10%
#'   along the body. The longer (i.e. higher resolution) these measurements are,
#'   the better their representation of the morphology of the animal. Actual
#'   values of width along the body are calculated to the resolution of the
#'   entered unit of length by linearly interpolating between each proportional
#'   width. For example, if the predator length (body_length) is 1000cm, a width
#'   value in cm is calculated at every cm along the body using either the
#'   proportional width given in the vector or a linear interpolation between
#'   adjoining proportional widths. Therefore, use units which will give an
#'   appropriate resolution, at least greater than the profile vector lengths.
#'   The higher the numeric value of the length the better, at least three
#'   digits is recommended.
#'
#'   Which width at each body segment (e.g. horizontal or vertical) used to
#'   calculate alpha (\strong{a}) is determined via the \code{select_width}
#'   operator. This can be the midpoint value between them ("mid", the default),
#'   maximum value ("max"), or minimum value ("min"). You can also choose to use
#'   only the vertical or horizontal profile widths ("v", "h"). You can also
#'   choose to use the maximum width to calculate alpha, in which case the alpha
#'   of no other segments of the body is calculated, only that of the maximum
#'   girth.
#'
#'   These final widths and their relative distances from the observing prey are
#'   used to calculate the viewing angle alpha (\strong{a}). At some point, the
#'   maximum girth of the predator will not make up the widest apparent visual
#'   angle of the predator, but more anterior segments will appear to the prey
#'   to be wider, and have a higher alpha. The maximum alpha at each iteration
#'   of the model (i.e. each step along the speed vector at the chosen
#'   frequency) is extracted from these, and used to calculate the rate of
#'   change in alpha, dA/dt (\strong{da/dt} in radians/s.
#'
#'   Because this interpolation is done on both vectors, they do not need to be
#'   the same length. If the predator is approximately equal in both dimensions
#'   or always wider in one dimension, only one profile and other
#'   profile-specific inputs is required, and the others can be left = NULL.
#'   Maximum girth locations can occur at an intermediate section of the body,
#'   but the proportional widths on either side must be less than 1.000.
#'
#'   \code{max_girth_loc_v} and \code{max_girth_loc_h} are where the maximum
#'   widths occur along the body as a proportion of the total body length going
#'   from the nose. They are only necessary if they are not specified as one of
#'   the proportional widths in \code{profile_v} and \code{profile_h}, that is
#'   none of these have the value of exactly 1.
#'
#' @section Plotting: Several options control plotting....
#'
#' @usage attack_model(speed, frequency = 60, body_length = 10.5, model_length =
#'   NULL, simple_output = TRUE, plot = TRUE, plot_from = 0, plot_to = NULL)
#'
#' @param speed numeric. Either a single constant speed value or vector of
#'   speeds of the approaching attacker. Must be at same frequency in Hz as
#'   \code{frequency}. If a data.frame is entered the first colummn is used. For
#'   a constant speed value the function will repeat this the required number of
#'   times at the correct frequency based on \code{model_length}.
#' @param model_length integer. Total length of the model in rows. Required if
#'   \code{speed} is a single value, where along with frequency it determines
#'   the distance the predator starts at. If \code{speed} is a vector
#'   \code{model_length} can be left NULL, in which case it is assumed the
#'   predator reaches the prey on the last value, and the length of the speed
#'   vector determines total length of model. Alternatively, \code{model_length}
#'   can be used to set a different capture point along the speed vector, in
#'   which case its value must be less than the total length of \code{speed}.
#' @param frequency numeric. Frequency (Hz) of the model, i.e. how many speed
#'   and other measurements per second. Must be same frequency in Hz as
#'   \code{speed}.
#' @param attacker_length numeric. Length of the attacker. Must be same units as
#'   maximum width, and same as distance unit used in speed. E.g. if speed is in
#'   cm/s, must be cm.
#' @param alpha_range numeric. Vector of two values of alpha. These will appear
#'   on any plot as a blue region, and if \code{simple_output = FALSE}, this
#'   region of the model is subset out to a separate entry in the saved
#'   \code{list} object. If any are not reached in the scenario there should be
#'   a message. If upper range is not reached, it is plotted from lower value to
#'   end of model, i.e. model_length location.
#' @param dAdt_range numeric. Vector of two values of dA/dt. These will appear
#'   on any plot as a green region, and if \code{simple_output = FALSE}, this
#'   region of the model is subset out to a separate entry in the saved
#'   \code{list} object. If any are not reached in the scenario there should be
#'   a message. If upper range is not reached, it is plotted from lower value to
#'   end of model, i.e. model_length location.
#' @param simple_output logical. Choose structure of output. If TRUE, a simple
#'   data frame of the model is returned, otherwise output is a \code{list}
#'   object containing the final model, input parameters, subset regions, and
#'   more.
#' @param plot logical. Choose to plot result.
#' @param plot_from numeric. Time on x-axis to plot from.
#' @param plot_to numeric.  Time on x-axis to plot to.
#'
#'  @examples
#' attack_model(...)
#'
#' @author Nicholas Carey - \email{nicholascarey@gmail.com}, Dave Cade
#'   \email{davecade@stanford.edu},
#'
#' @export

attack_model <- function(
  speed,
  model_length = NULL,
  frequency = 60,
  body_length = NULL,
  body_width_v = NULL,
  body_width_h = NULL,
  profile_v = NULL,
  profile_h = NULL,
  max_girth_loc_v = NULL,
  max_girth_loc_h = NULL,
  select_width = "mid",
  simple_output = TRUE,
  plot = TRUE,
  plot_from = 0,
  plot_to = NULL,
  alpha_range = NULL,
  dAdt_range = NULL){


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
  if(any(profile_v > 1) || any(profile_h > 1)) stop("Body profiles must only contain values between 0 and 1.")
  if(any(profile_v < 0) || any(profile_h < 0)) stop("Body profiles must only contain values between 0 and 1.")
  ## Can't both be NULL
  if(is.null(profile_v) && is.null(profile_h)) stop("Provide at least one body profile.")
  ## Must be over 2 long (nose, mid, tail)
  if((!is.null(profile_v) && length(profile_v) < 3) || (!is.null(profile_h) && length(profile_h) < 3)) stop("Profiles must be at least 3 values long: e.g. nose, midpoint, tail.")
  ## If a profile is empty, message that associated inputs ignored
  if(is.null(profile_v)) message("No vertical body profile (profile_v) found. Any inputs for max_girth_loc_v and body_width_v ignored.")
  if(is.null(profile_h)) message("No horizontal body profile (profile_h) found. Any inputs for max_girth_loc_h and body_width_h ignored.")
  ## If a profile doesn't contain 1.0, then max_girth_loc should be NULL
  if(any(profile_v == 1) && !is.null(max_girth_loc_v)) stop("profile_v already contains a max girth location (value of 1.0). max_girth_loc_v cannot also be specified.")
  if(any(profile_h == 1) && !is.null(max_girth_loc_h)) stop("profile_h already contains a max girth location (value of 1.0). max_girth_loc_h cannot also be specified.")
  ## And vice versa - if no 1.0 in profile, then mac_girth_loc required
  if(!any(profile_v == 1) && is.null(max_girth_loc_v)) stop("No max girth location (value of 1.0) found in profile_v. Please specify one with max_girth_loc_v.")
  if(!any(profile_h == 1) && is.null(max_girth_loc_h)) stop("No max girth location (value of 1.0) found in profile_h. Please specify one with max_girth_loc_h.")

  ## max_girth_loc
  ## Must be between 0 and 1 (if entered)
  if(!is.null(max_girth_loc_v) && (max_girth_loc_v >= 1 || max_girth_loc_v <= 0)) stop("Max width locations must be between 0 and 1. They represent a proportional distance along the length from the nose.")
  if(!is.null(max_girth_loc_h) && (max_girth_loc_h >= 1 || max_girth_loc_h <= 0)) stop("Max width locations must be between 0 and 1. They represent a proportional distance along the length from the nose.")

  ## select_width
  if(!(select_width %in% (c("mid", "max", "min", "v", "h", "max_girth_v", "max_girth_h")))) stop("select_width input not recognised.")


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
    max_girth_loc_v = max_girth_loc_v,
    profile_h = profile_h,
    max_girth_loc_h = max_girth_loc_h,
    select_width = select_width,
    simple_output = simple_output,
    plot = plot,
    plot_from = plot_from,
    plot_to = plot_to,
    alpha_range = alpha_range,
    dAdt_range = dAdt_range)


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
    max_girth_loc_h <- max_girth_loc_v}

  if(is.null(profile_v)){
    profile_v <- profile_h
    body_width_v <- body_width_h
    max_girth_loc_v <- max_girth_loc_h}


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
  widths_v <- interpolate_widths(profile_v, max_girth_loc_v, body_length, body_width_v)
  widths_h <- interpolate_widths(profile_h, max_girth_loc_h, body_length, body_width_h)

  widths_df <- data.frame(widths_v = widths_v,
                          widths_h = widths_h)

  ## Filter widths based on select_width input
  ## And add distance from nose
  if(select_width == "mid") {
    widths <- apply(widths_df, 1, function(x) mean(x))
    widths <- data.frame(dist_from_nose = (1:length(widths))-1,
                         width = widths)}

  if(select_width == "max") {
    widths <- apply(widths_df, 1, function(x) max(x))
    widths <- data.frame(dist_from_nose = (1:length(widths))-1,
                         width = widths)}

  if(select_width == "min") {
    widths <- apply(widths_df, 1, function(x) min(x))
    widths <- data.frame(dist_from_nose = (1:length(widths))-1,
                         width = widths)}

  if(select_width == "v") {
    widths <- widths_df[[1]]
    widths <- data.frame(dist_from_nose = (1:length(widths))-1,
                         width = widths)}

  if(select_width == "h") {
    widths <- widths_df[[2]]
    widths <- data.frame(dist_from_nose = (1:length(widths))-1,
                         width = widths)}

  if(select_width == "max_girth_v") {
    ## location of max_girth
    max_girth_index_v <- which.max(widths_df[[1]])
    widths <- max(widths_df[[1]])
    widths <- data.frame(dist_from_nose = max_girth_index_v-1,
                         width = widths)}

  if(select_width == "max_girth_h") {
    ## location of max_girth
    max_girth_index_h <- which.max(widths_df[[2]])
    widths <- max(widths_df$widths_h)
    widths <- data.frame(dist_from_nose = max_girth_index_h-1,
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

  ## Calc dAdt
  model_data$dAdt <- c(
    NA,
    diff(model_data$alpha) * frequency)

  # Find alpha and dAdt ranges ----------------------------------------------

  ## dAdt region
  if(is.null(dAdt_range)){
    dAdt_range_region <- NULL
  } else {
    ## find location of closest match to LOWER dAdt_range
    ## which dadt are higher than lower value?
    dAdt_range_low_index <- which(model_data$dAdt >= dAdt_range[1])[1]
    ## if it's never reached, set it to NA
    if(length(dAdt_range_low_index)==0){
      dAdt_range_low_index <- NA
      message("Lower range of dAdt_range never reached in this scenario. No dAdt_range range plotted.")
      ## otherwise, we only want first occurence
    } else {
      dAdt_range_low_index <- dAdt_range_low_index[1]}

    ## same for UPPER dAdt_range range
    ## NOTE - it's third in the vector (mean is second)
    dAdt_range_high_index <- which(model_data$dAdt <= dAdt_range[2])
    if(length(dAdt_range_high_index)==0){
      dAdt_range_high_index <- NA
      message("Upper range of dAdt_range never reached in this scenario.")
    } else {
      dAdt_range_high_index <- tail(dAdt_range_high_index, 1)}


    ## Use these to subset model to dAdt_range range
    if(is.na(dAdt_range_low_index)){
      dAdt_range_region <- "No matching dAdt_range region in this model"
    } else if (is.na(dAdt_range_high_index)) {
      dAdt_range_region <- model_data[dAdt_range_low_index:model_length,]
    } else {
      dAdt_range_region <- model_data[dAdt_range_low_index:dAdt_range_high_index,]
    }

  }

  ## alpha region
  if(is.null(alpha_range)){
    alpha_range_region <- NULL
  } else {
    ## find location of closest match to LOWER alpha_range
    ## which dadt are higher than lower value?
    alpha_range_low_index <- which(model_data$alpha >= alpha_range[1])[1]
    ## if it's never reached, set it to NA
    if(length(alpha_range_low_index)==0){
      alpha_range_low_index <- NA
      message("Lower range of alpha_range never reached in this scenario. No alpha_range range plotted.")
      ## otherwise, we only want first occurence
    } else {
      alpha_range_low_index <- alpha_range_low_index[1]}

    ## same for UPPER alpha_range range
    ## NOTE - it's third in the vector (mean is second)
    alpha_range_high_index <- which(model_data$alpha <= alpha_range[2])
    if(length(alpha_range_high_index)==0){
      alpha_range_high_index <- NA
      message("Upper range of alpha_range never reached in this scenario.")
    } else {
      alpha_range_high_index <- tail(alpha_range_high_index, 1)}


    ## Use these to subset model to alpha_range range
    if(is.na(alpha_range_low_index)){
      alpha_range_region <- "No matching alpha_range region in this model"
    } else if (is.na(alpha_range_high_index)) {
      alpha_range_region <- model_data[alpha_range_low_index:model_length,]
    } else {
      alpha_range_region <- model_data[alpha_range_low_index:alpha_range_high_index,]
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
      dAdt_range_region = dAdt_range_region,
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

    ## plot dAdt
    par(new=T)
    # colour for all alpha plotting
    dAdt_col <- "green"
    plot(dAdt~time, data = model_data,
         ylim = c(0, max(model_data$dAdt, na.rm = T)),
         xlim = c(plot_from, plot_to),
         pch = ".",
         col = "white",
         axes=FALSE,
         ylab = "",
         xlab = "")
    with(model_data, lines(x = time, y = dAdt, col = dAdt_col, lwd = 3))
    axis(side = 2, col = dAdt_col, lwd = 3, col.axis = dAdt_col,
         pos = plot_from+(0.1*(plot_to-plot_from)), cex.axis = 0.8)

    ## add dAdt_range range

    ## if upper and lower of dAdt_range range locations are equal, just draw dashed line
    ## but don't draw it if both equal length of x
    ## this means neither actually occurs
    ## see above (this is hacky - must be a better way)

    if(!is.null(dAdt_range)){
      abline(v = model_data$time[dAdt_range_low_index],
             col = rgb(15/255,245/255,53/255,  alpha = 0.4),
             lty = 1,
             lwd = 3)

      abline(v = model_data$time[dAdt_range_high_index],
             col = rgb(15/255,245/255,53/255,  alpha = 0.4),
             lty = 1,
             lwd = 3)

      rect(xleft = model_data$time[dAdt_range_low_index],
           ybottom = -5,
           xright = model_data$time[dAdt_range_high_index],
           ytop = max(model_data$dAdt, na.rm = T)+5,
           col = rgb(15/255,245/255,53/255,  alpha = 0.2),
           lty = 0)

      ## if dAdt_range_high_index is NA, then fill to end
      if(is.na(dAdt_range_high_index)){
        rect(xleft = model_data$time[dAdt_range_low_index],
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
           c("Speed", "Alpha", "dAdt", "prey"),
           text.col = c(speed_col, alpha_col, dAdt_col, "black"),
           col = c(speed_col, alpha_col, dAdt_col, "black"),
           lty=c(1,1,1,3), lwd = 3, cex=0.8)
  }

  # Return results ----------------------------------------------------------
  return(output)}






