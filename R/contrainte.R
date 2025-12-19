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

ALPHA_TEST <- 35
Vo_TEST <- 12
Ve_TEST <- 20
ALPHA_MIN <- 0
ALPHA_MAX <- 2*pi
Vo_MIN <- 30
Vo_MAX <- 90
Ve_MIN <- 7
Ve_MAX <- 24
HEAVY_ALPHA <- 0.001
HEAVY_GAMMA <- 0.83
LIMITE <- 150
TOLERANCE <- 0.006
DATE <- 10

ABCISSE_CIBLE <- 12779.2843
M_N <- 12
M_I <- 5
LAMBDA <- 0.02
V_E <- 12
Ro <- 0.01
A_X <- -8.33
C_X <- 4.345
V_R <- 0.47
ALPHA <- 0.25*pi
V_0 <- 40

## A FUNCTION SPECIFICALLY DESIGNED TO MONITOR THE VARIOUS CONSTRAINTS APPLIED TO THE OPTIMIZATION PARAMETERS
#' contraints
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
