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

interpolate_widths <- function(profile, max_width_loc, body_length, body_width){
  ## proportional locations along body length of proportional widths
  locs_bl <- seq(0, 1, length.out = length(profile))
  ## make df of index and proportional widths
  df <- data.frame(locs_bl = locs_bl,
                   profile = profile)
  # add max width location and reorder
  # Only do this if max width location not already specified in profile
  # by having one equal 1.00
  if(!any(profile == 1)) df <- rbind(df, c(max_width_loc, 1.000))
  # reorder
  df <- dplyr::arrange(df, locs_bl)
  ## now interpolate at resolution of body length value
  profile_interpolated <- approx(df$profile, method = "linear", n = body_length)$y
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

#' Calculates screen diameter for each model.
#'
#' x = model dataframe containing alpha angles data
#' alpha_col = column identifier for alpha angles data
#' screen_distance = distance simulation will be viewed at
#'
#' A new column 'diam_on_screen' will be added to the dataframe
#'
#' @keywords internal
#' @export
add_diam_on_screen <- function(x, alpha_col, screen_distance){
  ## add diam_on_screen column
  x$diam_on_screen <- 2 * screen_distance * (tan(x[[alpha_col]] / 2))
  ## Round to 2 decimal places (1/10th of a mm)
  x$diam_on_screen <- sapply(x$diam_on_screen, function(z) round(z, 2))
  ## Convert any diameter over 500cm to 500cm, which can't be displayed on screen anyway.
  ## (deals with values on last frames, where diam can be approaching infinity)
  x$diam_on_screen <- sapply(x$diam_on_screen, function(z) ifelse(z > 500, z <- 500, z))
  return(x)
}

#' Extract attacker distance when a specific da/dt value is exceeded
#'
#' @keywords internal
#' @export
get_dist <- function(mod, ALT){
  index <- which(mod$dadt > ALT)[1]
  dist <- mod$distance_nose[index]
  return(dist)
}

#' extract attacker time to reach prey when a specific da/dt value is exceeded
#'
#' @keywords internal
#' @export
get_time <- function(mod, ALT){
  index <- which(mod$dadt > ALT)[1]
  time <- mod$time_rev[index]
  return(time)
}

#' extract attacker alpha when a specific da/dt value is exceeded
#'
#' @keywords internal
#' @export
get_alpha <- function(mod, ALT){
  index <- which(mod$dadt > ALT)[1]
  alpha <- mod$alpha[index]
  return(alpha)
}


