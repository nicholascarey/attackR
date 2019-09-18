#' @title Calculates screen diameter given an alpha angle and viewing distance
#'
#' @description \code{diam_on_screen} calculates the correct diameter on screen
#'   for an alpha viewing angle and screen distance.
#'
#' @details If input is an object of class \code{attack_model} or
#'   \code{data.frame} exported from \code{attack_model}, the alpha angle column
#'   is identified automatically, and an identical object is exported with a new
#'   \code{diam_on_screen} column added.
#'
#'   Otherwise, a vector of alpha angles can be entered, in which case the
#'   output is a vector of screen diameters.
#'
#' @usage diam_on_screen(x, screen_distance)
#'
#' @param x numeric. a list object of class \code{attack_model} or
#'   \code{data.frame} exported from \code{attack_model} where there is an
#'   existing \code{$alpha} column. Alternatively, a numeric vector.
#' @param screen_distance numeric. Distance from the display screen of the
#'   observing specimen.
#'
#' @return A new column 'diam_on_screen' will be added to the data frame inputs,
#'   otherwise a numeric vector is returned. Diameters are in same units as
#'   \code{screen_distance}.
#'
#' @author Nicholas Carey - \email{nicholascarey@gmail.com}
#'
#' @export

diam_on_screen <- function(x, screen_distance){

  if(class(x) == "attack_model") {alpha <- x$final_model$alpha}
  else if(is.data.frame(x)) {alpha <- x$alpha}
  else {alpha <- x}

  ## substitute any alpha values above 3.10 to 3.10
  ## (deals with values on last frames, where alpha approaches max, and
  ## diam can be approaching infinity - seems like a good compromise value)
  alpha <- replace(alpha, alpha > 3.10, 3.10)

  ## calc diam_on_screen
  diam_on_screen <- 2 * screen_distance * (tan(alpha / 2))

  if(class(x) == "attack_model") {x$final_model <- cbind(x$final_model, diam_on_screen = diam_on_screen)}
  else if(is.data.frame(x)) {x <- cbind(x, diam_on_screen = diam_on_screen)}
  else {x <- diam_on_screen}

  ## return
  return(x)
}