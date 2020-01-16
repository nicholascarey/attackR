# Internal Functions ------------------------------------------------------

#' Interpolates a body profile to the units used in body length
#'
#' @details
#' Interpolates linearly between each proportional width.
#' E.g. if 10 prop widtsh are given in the profile and the body length is 100cm,
#' result gives 101 proportional values from 0 to 100.
#'
#' @importFrom dplyr arrange
#' @keywords internal
#' @export

interpolate_widths <- function(profile, max_girth_loc, body_length, body_width){
  ## proportional locations along body length of proportional widths
  locs_bl <- seq(0, 1, length.out = length(profile))
  ## make df of index and proportional widths
  df <- data.frame(locs_bl = locs_bl,
                   profile = profile)
  # add max width location and reorder
  # Only do this if max width location not already specified in profile
  # by having one equal 1.00
  if(!any(profile == 1)) df <- rbind(df, c(max_girth_loc, 1.000))
  # reorder
  df <- dplyr::arrange(df, locs_bl)
  ## now interpolate at resolution of body length value
  profile_interpolated <- approx(x = df$locs_bl, y = df$profile, method = "linear", n = body_length)$y
  ## convert to actual widths
  widths_interpolated <- profile_interpolated * body_width
  ## return
  return(widths_interpolated)
}




#' Calculate alpha in radians
#'
#' Note - only works for complete objects with same diameter and distance. So
#' can be used for alpha of max_girth, but not of jaws.
#'
#' @keywords internal
#' @export
calc_alpha <- function(diameter, distance){
  output <- 2*(atan((diameter/2)/distance))
  return(output)
}

#' Calculate da/dt in radians
#'
#' Note - only works for complete objects with same diameter and distance. So
#' can be used for dadt of max_girth, but not of jaws.
#'
#' @keywords internal
#' @export
calc_dadt <- function(speed, diameter, distance){
  output <- 4*(speed*diameter)/((4*distance^2)+(diameter^2))
  return(output)
}




#' extract attacker alpha when a specific da/dt value is exceeded
#'
#' @keywords internal
#' @export
get_alpha <- function(mod, dadt){
  index <- which(mod$dadt > dadt)[1]
  alpha <- mod$alpha[index]
  return(alpha)
}


#' @title Get first close matching value in a generally increasing vector
#'
#' @description \code{first_over} - takes a single value and finds the index of
#'   first occurrence or first time that value is exceeded in a vector.
#'
#' @details Finds the index (i.e. position) of the first occurrence or first
#'   time a value is exceeded in a vector.
#' @param this numeric. Single value to find first occurence of.
#' @param there numeric. The vector in which to find value.
#' @return Index vector of where the value occurs or is exceeded in the target
#'   vector.
#' @keywords internal
#' @export
first_over <- function(this, there) { # search for *this* in *there*
  ## if this > any there stop
  if (!any(na.omit(there) >= this)) {
    stop("target value never reached in this vector")
  }
  result <- which(there >= this)[1]
  return(result)
}


#' @title Get closest matching value
#'
#' @description \code{closest} - takes a numeric vector or single value and
#'   finds the index of closest matching value(s) in a different vector.
#'
#' @details Finds the index (i.e. position(s)) of the closest
#' matching value(s) of the input(s) in a vector
#'
#' @seealso \code{\link{closest}}
#'
#' @usage closest(this, there)
#'
#' @param this numeric. Vector or value. The value(s) to match.
#' @param there numeric. The vector in which to find closest matching value(s).
#'
#' @return Index of where closest matching value(s) occur in the target vector.
#'
#' @keywords internal
#' @export
closest <- function(this, there) { # search for *this* in *there*
  if (length(this) == 1) {
    output <- which.min(abs(there - this))
    return(output)
  } else {
    output <- sapply(this, function(this) which.min(abs(there - this)))
    return(output)
  }
}


#' @title Get first AND closest matching value
#'
#' @description \code{first_closest} - takes a single value and finds the index
#'   of first time that value is exceeded in a vector, then if that value or the
#'   one preceding it is actually closer.
#'
#' @seealso \code{\link{first_closest}}
#'
#' @usage first_closest(this, there)
#'
#' @param this numeric. Vector or value. The value(s) to match.
#' @param there numeric. The vector in which to find first closest matching value(s).
#'
#' @return Index of where first_closest matching value(s) occur in the target vector.
#'
#' @keywords internal
#' @export
first_closest <- function(this, there) { # search for *this* in *there*
  ## if this > any there stop
  if (!any(na.omit(there) >= this)) {
    stop("target value never reached in this vector")
  }
  result <- which(there >= this)[1]

  ## is 'this' closer to 'result' or previous entry in vector?
  ## if so, make this the result
  ## This will either result in 0 if original first entry over is closest,
  ## or -1 if the entry before that is closer.
  ## Result is added to index
  result <- result + (which.min(c(abs(there[result-1]-this),
                                  abs(there[result]-this)))-2)

  return(result)
}


#' @title Calculates screen diameter for a model based on alpha and a viewing
#'   distance
#'
#' @description Calculates screen diameter for a model based on alpha and a
#'   viewing distance
#' @param x numeric. Output of `attack_model` or `attack_model_whale` function,
#'   or other object containing `alpha` angles data.
#' @param alpha_col numeric. The column of alpha values. Automatically detected
#'   in most models, but can be specified here.
#' @param screen_distance numeric. distance from screen in same units as those
#'   used in model
#' @return Original object or data frame with 'screen_diam' column added, in
#'   same units as used in model and screen_distance. Rounded to 2 decimals
#'   places. Because of values approaching infinty at very close distances, any
#'   value over 1000 is replaced with 1000, so use appropriate units (cm is
#'   best).
#' @keywords internal
#' @export
add_screen_diam <- function(x, alpha = NULL, screen_distance){

  if(class(x) == 'attack_model' || class(x) == 'attack_model_whale') {
    df <- x$final_model
    alpha <- which(names(df) == "alpha")
  } else if(class(x) == 'constant_speed_model' || class(x) == 'variable_speed_model' || class(x) == 'diameter_model') {
    df <- x$model
    alpha <- which(names(df) == "alpha")
  } else if(class(x) == 'data.frame') {
    df <- x
    if(is.null(alpha)) alpha <- alpha <- which(names(df) == "alpha")
  } else {
    stop("Input not recognised")
  }

  if(is.null(alpha)) stop("alpha column required")

  ## add screen_diam column
  df$screen_diam <- 2 * screen_distance * (tan(df[[alpha]] / 2))
  ## Round to 2 decimal places (1/10th of a mm)
  df$screen_diam <- sapply(df$screen_diam, function(z) round(z, 2))
  ## Convert any diameter over 1000 to 1000, which can't be displayed on screen anyway.
  ## (deals with values on last frames, where diam can be approaching infinity)
  df$screen_diam <- sapply(df$screen_diam, function(z) ifelse(z > 1000, z <- 1000, z))

  if(class(x) == 'attack_model' || class(x) == 'attack_model_whale') {
    x$final_model <- df
  } else if(class(x) == 'constant_speed_model' || class(x) == 'variable_speed_model' || class(x) == 'diameter_model') {
    x$model <- df
  } else if(class(x) == 'data.frame') {
    x <- df
  } else {
    stop("Input not recognised")
  }

  return(x)
}
