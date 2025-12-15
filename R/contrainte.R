#The system's behavior is governed by the following initial parameters:

# alpha is the Launch Angle of the projectile in the (x,z) plane.
# Vo is the magnitude of the Propulsion Velocity
# Ve is the magnitude of the Ejection Velocity of the replenishment fluid, which is collinear with V0.
# alpha_min is the lowest possible alpha value
# alpha_max is highest possible alpha value
# Vo_min is the lowest possible Vo value
# Vo_max is the highest possible Vo value
# Ve_min is the lowest possible Ve value
# Ve_max is the highest possible Ve value

ALPHA_MIN <- 10
ALPHA_MAX <- 86
Vo_MIN <- 10
Vo_MAX <- 60
Ve_MIN <- 8
Ve_MAX <- 35

## A FUNCTION SPECIFICALLY DESIGNED TO MONITOR THE VARIOUS CONSTRAINTS APPLIED TO THE OPTIMIZATION PARAMETERS
#' Title contraints
#'
#' @param alpha the Launch Angle of the projectile in the (x,z) plane
#' @param Vo the magnitude of the Propulsion Velocity
#' @param Ve the magnitude of the Ejection Velocity of the replenishment fluid
#' @param alpha_min the lowest possible alpha value
#' @param alpha_max the highest possible Vo value
#' @param Vo_min the lowest possible Vo value
#' @param Vo_max the highest possible Vo value
#' @param Ve_min the lowest possible Ve value
#' @param Ve_max the highest possible Ve value
#'
#' @returns TRUE if the main conditions on the decision parameters status are right and FALSE if not
#' @export
#'
#' @examples
#' contrainte(45, 12, 9, 10, 86, 10, 60, 8, 35)
#' contrainte(45, 12, 9)
contrainte <- function(alpha, Vo, Ve, alpha_min = ALPHA_MIN, alpha_max = ALPHA_MAX, Vo_min = Vo_MIN, Vo_max = Vo_MAX, Ve_min = Ve_MIN, Ve_max = Ve_MAX){
  if(alpha >= alpha_min & alpha <= alpha_max & Vo >= Vo_min & Vo <= Vo_max & Ve >= Ve_min & Ve <= Ve_max){
    return(TRUE)
  }else{
    return(FALSE)
  }
}
# Heavy Ball Method functions are initialized with starting values and can be customized for specific situations
