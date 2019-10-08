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
