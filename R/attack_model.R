#'@title Attack Model
#'
#'@description \code{attack_model} models the visual aspects of an attack by a
#'  predator on a prey
#'
#'  Models an attack by a predator on a prey using parameters such as size,
#'  speed, shape etc. From the prey perspective, it calculates the visual angle
#'  of the diameter of the attacker (alpha, or \strong{a}) in radians, and the
#'  rate of change of this angle (\strong{da/dt} in radians/s).
#'
#'  Input units: `frequency` is in \code{Hz}. If `speed` is a vector it should
#'  be at the same frequency. Body measurements and `speed` units can be any
#'  unit, but should be consistent. i.e. all in `cm`, speeds in `cm/s`, etc.
#'
#'  `speed` can be either a constant value or vector of speeds (at the
#'  correct`frequency`). If a single constant value, a `model_length` is
#'  required in order to calculate the distance the predator starts the attack
#'  from. For example, at a `frequency` of 60 Hz and `model_length` of 180, the
#'  model will be three seconds in duration. At a constant `speed` of 600 cm/s,
#'  the predator will start from approximately 1800 cm away.
#'
#'  The `model_length` operator can also be used to vary where on variable speed
#'  vectors the predator times reaching the prey, to examine how this affects
#'  perceived alphas. If left NULL, with speed vectors by default the model ends
#'  with the predator reaching the prey on the last speed value, however
#'  `model_length` can be used to set this to be earlier.
#'
#'  Models end when the nose (most anterior part in the profiles) of the
#'  predator reaches the prey (last row of the data.frame, where time and
#'  distance equal zero).
#'
#'@details In order to correctly model the widest apparent part of the predator
#'  it requires several morphometric inputs.
#'
#'@section Length and Width. `body_length` is simply the total length from nose
#'  to tail (or equivalent). `body_width_v` and  `body_width_h` are the
#'  *maximum* body widths in the vertical (i.e. dorsal:ventral) and horizontal
#'  (left:right) vectors of the body. These must be in the same units.
#'
#'@section Profiles. `profile_v` and `profile_h` are vectors of widths of the
#'  predator's body as a proportion of the maximum width going from the anterior
#'  (nose) to the posterior (tail). Therefore, they should generally start and
#'  end on zero (though they don't have to), and all values must be between zero
#'  and 1. They must be evenly spaced, e.g. every 10% along the body. The longer
#'  (i.e. higher resolution) these vectors are, the better their representation
#'  of the morphology of the animal. Actual values of width along the body are
#'  calculated to the resolution of the entered unit of length by linearly
#'  interpolating between each proportional width. For example, if the predator
#'  length (`body_length`) is 1000cm, a width value in cm is calculated at every
#'  cm along the body using either the proportional width given in the vector or
#'  a linear interpolation between adjoining proportional widths. Therefore, use
#'  units which will give an appropriate resolution, at least greater than the
#'  profile vector lengths.
#'
#'  Which width at each body segment (i.e. horizontal or vertical) used to
#'  calculate alpha (\strong{a}) is determined via the `select_widths` operator.
#'  This can be the midpoint value between them (`"mean"` or `"mid"`, both the
#'  same and the default), maximum value (`"max"`), or minimum value (`"min"`).
#'
#'  These final widths and their relative distances from the observing prey are
#'  used to calculate the viewing angle alpha (\strong{a}). At some point, the
#'  maximum width of the predator will not be the widest apparent part of the
#'  predator, but more anterior segments will appear to the prey to be wider,
#'  that is have a higher alpha. The maximum alpha at each iteration of the
#'  model (i.e. each step along the speed vector at the chosen frequency) is
#'  extracted from these, and used to calculate the rate of change in alpha,
#'  dA/dt (\strong{da/dt} in radians/s.
#'
#'  Because this interpolation is done on both vectors, they do not need to be
#'  the same length. The exception is if the predator is approximately equal in
#'  both dimensions or always wider in one dimension, in which case only one
#'  profile is required, and the other can be left `= NULL`. Maximum girth
#'  locations can occur at an intermediate section of the body, but obviously
#'  the proportional widths on either side must be less than 1.000.
#'
#'  `max_width_loc_v` and `max_width_loc_h` are where the maximum widths occur
#'  along the body. They are only necessary if they are not specified as one of
#'  the proportional widths in `profile_v` and `profile_h`, that is none of
#'  these have the value of exactly 1.
#'
#'@section Plotting. Several options control plotting.
#'
#'
#'@usage attack_model(speed, frequency = 60, body_length = 10.5, model_length =
#'  NULL, simple_output = TRUE, cALT = c(0.89, 1.66, 2.06), plot = TRUE,
#'  plot_from = 0, plot_to = NULL)
#'
#'@param speed numeric. A vector of speeds of the modelled approaching attacker.
#'  Must be at same frequency in Hz as `frequency`. If a data.frame is entered
#'  the first colummn is used. A single constant speed value can also be used;
#'  the function will repeat this the required number of times at the correct
#'  frequency based on `model_length`.
#'@param model_length integer. Total length of the model. Required if `speed` is
#'  a single value, when along with `frequency` it determines the distance the
#'  predator starts at. If `speed` is a vector this can be left NULL, in which
#'  case it is assumed the predator reaches the prey on the last value, and the
#'  length of the `speed` vector determines total length of model.
#'  Alternatively, `model_length` can be used to set a different capture point
#'  along the speed vector, in which case its value must be less than the total
#'  length of `speed`.
#'@param frequency numeric. Frequency (Hz) of the model (frame rate of any
#'  resulting animation). Must be same frequency in Hz as \code{speed}
#'@param attacker_length numeric. Length of the attacker. Must be same units as
#'  maximum width.
#'@param alpha numeric. Vector of three values of critical ALT range: lower,
#'@param dadt numeric. Vector of three values of critical ALT range: lower,
#'  mean, upper. Enter as c(1,2,3). Helps put range on plot. Mean is not plotted
#'  for now. If any are not reached in the scenario there should be a message.
#'  If upper range is not reached, range is plotted from lower value to end of
#'  model, i.e. `model_length` location.
#'@param simple_output logical. Choose structure of output. If TRUE, a simple
#'  dataframe of the model is returned, otherwise output is a List object
#'  containing the final model and input parameters.
#'@param plot logical. Choose to plot result.
#'@param plot_from numeric. Time on x-axis to plot from.
#'@param plot_to numeric.  Time on x-axis to plot to.
#'
#' @examples
#' attack_model(...)
#'
#'@author Nicholas Carey - \link{nicholascarey@gmail.com}, Dave Cade
#'  \link{davecade@stanford.edu},
#'
#'@export


attack_model <-

  function(
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
    select_widths = "mean",
    simple_output = TRUE,
    plot = TRUE,
    plot_from = 0,
    plot_to = NULL,
    alpha_range = NULL,
    dAdt_range = NULL){


    # Error Checks and Messages -----------------------------------------------

    ## Checks here

    ## Profiles
    ## Values must be between 0 and 1
    if(any(profile_v > 1) || any(profile_h > 1)) stop("Body profiles must only contain values between 0 and 1.")
    if(any(profile_v < 0) || any(profile_h < 0)) stop("Body profiles must only contain values between 0 and 1.")
    ## Can't both be NULL
    if(is.null(profile_v) || is.null(profile_h)) stop("Provide at least one body profile.")
    ## Must be over 2 long (nose, mid, tail)
    if(length(profile_v) < 3 || length(profile_h) < 3) stop("Profiles must at least 3 values long: e.g. nose, midpoint, tail.")

    ## max_wdith_loc
    ## Must be between 0 and 1
    if(max_width_loc_v >= 1 || max_width_loc_v <= 0) stop("Max width locations must be between 0 and 1. They represent a proportional distance along the length from the nose.")
    if(max_width_loc_h >= 1 || max_width_loc_h <= 0) stop("Max width locations must be between 0 and 1. They represent a proportional distance along the length from the nose.")
    ## If max_width_loc entered, cannot be a value of 1.0 in corresponding profile
    max_width_loc_v = -0.5


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
      select_widths = select_widths,
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


    # If one of the profiles is empty, just copy to the other. ----------------
    ## same with other _v and _h settings






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


    ## Filter widths based on select_widths input
    if(select_widths == "mean" || select_widths == "mid") widths <- apply(widths_df, 1, function(x) mean(x))
    if(select_widths == "max") widths <- apply(widths_df, 1, function(x) max(x))
    if(select_widths == "min") widths <- apply(widths_df, 1, function(x) min(x))


    ## Add distance from nose - starting at zero
    widths <- data.frame(dist_from_nose = (1:length(widths))-1,
                         width = widths)


    ## For every row of model add distance_nose to segment distances
    ## This gives distance of every segment at every iteration of model
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
    if(!is.null(dAdt_range)){

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
    if(!is.null(alpha_range)){

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
          body_max_alpha_per_i = alpha_max_index
        )
      )

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
               ytop = max(model_data$dAdt, na.rm = T)+5,
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
             ytop = max(model_data$alpha, na.rm = T)+5,
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






