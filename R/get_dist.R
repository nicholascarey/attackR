#' @title Extract attacker distance when a specific **{α}** or **{dα/dt}** value
#'   is exceeded
#'
#' @description \code{get_dist} returns the distance of the predator when a
#'   specific **{α}** or **{dα/dt}** value is reached.
#'
#' @details Input can be an object of class \code{attack_model},
#'   \code{attack_model_whale}, or \code{data.frame} exported from either of
#'   those functions. **{α}** and **{dα/dt}** columns are identified
#'   automatically. Only one of the *\code{dadt}* or *\code{alpha}* inputs
#'   should be entered.
#'
#' @usage get_dist(mod, dadt, alpha)
#'
#' @param mod list or data.frame. Object of class \code{attack_model},
#'   \code{attack_model_whale}, or \code{data.frame} exported from either of
#'   those functions
#' @param dadt numeric. dadt value to find distance exceeded at.
#' @param alpha numeric. alpha value to find distance exceeded at.
#'
#' @return numeric value of distance in whatever units were used to create the
#'   model.
#'
#' @author Nicholas Carey - \email{nicholascarey@gmail.com}
#'
#' @export

get_dist <- function(mod, dadt = NULL, alpha = NULL){

  ## Checks
  if(!is.null(dadt) && !is.null(alpha)) stop("Enter only one of dadt or alpha, not both.")
  if(is.null(dadt) && is.null(alpha)) stop("Enter a dadt or alpha value.")

  if(class(mod) == "attack_model" || class(mod) == "attack_model_whale"){
    df <- mod$final_model
  } else if(is.data.frame(mod) && "distance_nose" %in% names(mod)){
    df <- mod
  } else {
    stop("Input not recognised.")}

  ## find dadt or alpha
  if(!is.null(dadt)) {
    index <- first_over(dadt, df$dadt) # find first time exceeded
    # Make index whichever is closest - 1st or 2nd of these two values
    index <- index + (closest(dadt, df$dadt[(index-1):index])-2)
    }

  if(!is.null(alpha)) {
    index <- first_over(alpha, df$alpha)
    index <- index + (closest(alpha, df$alpha[(index-1):index])-2)
    }

  if(class(mod) == "attack_model") dist <- df$distance_nose[index]
  if(class(mod) == "attack_model_whale") dist <- df$distance_low_jaw[index]

  ## if df and distance_low_jaw present, must be attack_model_whale
  if(class(mod) == "data.frame" && ("distance_low_jaw" %in% names(df)) == TRUE) dist <- df$distance_low_jaw[index]
  ## if not, must be attack_model
  if(class(mod) == "data.frame" && ("distance_low_jaw" %in% names(df)) == FALSE) dist <- df$distance_nose[index]


  if("distance_low_jaw" %in% names(df) == TRUE) message("attack_model_whale input - distance is from low jaw tip")
  if("distance_low_jaw" %in% names(df) == FALSE) message("attack_model input - distance is from nose")

  return(dist)

}
