#' @title Extract attacker time when a specific alpha or dA/dt value is exceeded
#'
#' @description \code{get_time} returns the time to reach prey of the predator
#'   when a specific dA/dt or alpha value is reached or exceeded.
#'
#' @details Input can be an object of class \code{attack_model},
#'   \code{attack_model_whale}, or \code{data.frame} exported from either of
#'   those functions. Alpha and dA/dt columns are identified automatically. Only
#'   one of dA/dt or alpha should be entered.
#'
#' @usage diam_on_screen(x, dAdt, alpha)
#'
#' @param mod list or data.frame. Object of class \code{attack_model},
#'   \code{attack_model_whale}, or \code{data.frame} exported from either of
#'   those functions
#' @param dAdt numeric. dAdt value to find time exceeded at.
#' @param alpha numeric. alpha value to find time exceeded at.
#'
#' @return numeric value of time in whatever units were used to create the
#'   model.
#'
#' @author Nicholas Carey - \email{nicholascarey@gmail.com}
#'
#' @export

get_time <- function(mod, dAdt = NULL, alpha = NULL){

  ## Checks
  if(!is.null(dAdt) && !is.null(alpha)) stop("Enter only one of dAdt or alpha, not both.")
  if(is.null(dAdt) && is.null(alpha)) stop("Enter a dAdt or alpha value.")

  if(class(mod) == "attack_model" || class(mod) == "attack_model_whale"){
    df <- mod$final_model
  } else if(is.data.frame(mod) && "time_rev" %in% names(mod)){
    df <- mod
  } else {
    stop("Input not recognised.")}

  ## find dAdt or alpha
  if(!is.null(dAdt)) index <- first_over(dAdt, df$dAdt)
  if(!is.null(alpha)) index <- first_over(alpha, df$alpha)

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
