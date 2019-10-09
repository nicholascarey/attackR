#' @title Extract attacker time to reach prey when a specific **{α}** or
#'   **{dα/dt}** value is exceeded
#'
#' @description \code{get_time} returns the time to reach the prey by the
#'   predator when a specific **{α}** or **{dα/dt}** value is reached.
#'
#' @details Input can be an object of class \code{attack_model},
#'   \code{attack_model_whale}, or \code{data.frame} exported from either of
#'   those functions. **{α}** and **{dα/dt}** columns are identified
#'   automatically. Only one of the *\code{dadt}* or *\code{alpha}* inputs
#'   should be entered.
#'
#' @usage get_time(mod, dadt, alpha)
#'
#' @param mod list or data.frame. Object of class \code{attack_model},
#'   \code{attack_model_whale}, or \code{data.frame} exported from either of
#'   those functions
#' @param dadt numeric. dadt value to find time exceeded at.
#' @param alpha numeric. alpha value to find time exceeded at.
#'
#' @return numeric value of time in whatever units were used to create the
#'   model.
#'
#' @author Nicholas Carey - \email{nicholascarey@gmail.com}
#'
#' @export

get_time <- function(mod, dadt = NULL, alpha = NULL){

  ## Checks
  if(!is.null(dadt) && !is.null(alpha)) stop("Enter only one of dadt or alpha, not both.")
  if(is.null(dadt) && is.null(alpha)) stop("Enter a dadt or alpha value.")

  if(class(mod) == "attack_model" || class(mod) == "attack_model_whale"){
    df <- mod$final_model
  } else if(is.data.frame(mod) && "time_rev" %in% names(mod)){
    df <- mod
  } else {
    stop("Input not recognised.")}

  ## find dadt or alpha
  ## find dadt or alpha
  if(!is.null(dadt)) {
    index <- first_closest(dadt, df$dadt) # find first time exceeded
    }

  if(!is.null(alpha)) {
    index <- first_closest(alpha, df$alpha)
    }

  if(class(mod) == "attack_model") time <- df$time_rev[index]
  if(class(mod) == "attack_model_whale") time <- df$time_rev[index]

  ## if df and time_rev present, must be attack_model_whale
  if(class(mod) == "data.frame" && ("time_rev" %in% names(df)) == TRUE) time <- df$time_rev[index]
  ## if not, must be attack_model
  if(class(mod) == "data.frame" && ("time_rev" %in% names(df)) == FALSE) time <- df$time_rev[index]


  if("distance_low_jaw" %in% names(df) == TRUE) message("attack_model_whale input - time is from low jaw tip")
  if("distance_low_jaw" %in% names(df) == FALSE) message("attack_model input - time is from nose")

  return(time)

}
