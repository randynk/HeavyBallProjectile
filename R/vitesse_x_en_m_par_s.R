#The system's behavior is governed by the following initial parameters:

# alpha is the Launch Angle of the projectile in the (x,z) plane.
# ro is the Density of Atmospheric Air pressing against the projectile's surface.
# Cx represents the Wind Drag Coefficient acting on the projectile's surface along the x-axis,
# Ax is the corresponding Reference Area subject to atmospheric wind fluctuations along the x-axis.
# Vr is the magnitude of the Wind Flow Velocity acting on the projectile.
# Vo is the magnitude of the Propulsion Velocity
# Ve is the magnitude of the Ejection Velocity of the replenishment fluid, which is collinear with V0.
# m0 represents the Initial Mass of the Replenishment Fluid prior to ejection,
# M0 is the Net Structural Mass (or Dry Mass) of the projectile
# lambda is the Fluid Expulsion Parameter (or Escape Parameter) governing the rate of fluid loss.

M_N <- 12
M_I <- 5
LAMBDA <- 0.02
V_E <- 9
Ro <- 0.01
A_X <- -8.33
C_X <- 4.345
V_R <- 0.47
ALPHA <- 60
V_0 <- 12

# FUNCTION THAT RETURNS THE HORIZONTAL COMPONENT OF VELOCITY (the speed in the x-direction) DRIVING THE PROJECTILE AT TIME t
#' Title ORIZONTAL VELOCITY
#'
#' @param t the time date in seconds
#' @param lambda the Fluid Expulsion Parameter
#' @param m_0 the Initial Mass of the Replenishment Fluid prior to ejection
#' @param M_0 the Net Structural Mass
#' @param Ve the magnitude of the Ejection Velocity of the replenishment fluid
#' @param ro the Density of Atmospheric Air pressing against the projectile's surface along the x-axis
#' @param Cx the Wind Drag Coefficient acting on the projectile's surface along the x-axis
#' @param Ax the corresponding Reference Area subject to atmospheric wind fluctuations along the x-axis
#' @param Vr the magnitude of the Wind Flow Velocity acting on the projectile
#' @param alpha the Launch Angle of the projectile in the x,z plane
#' @param Vo the magnitude of the Propulsion Velocity
#'
#' @returns THE HORIZONTAL COMPONENT OF VELOCITY
#' @export
#'
#' @examples
#' vitesse_x(51, 0.008, 9, 21, 9, 0.01, 4.12, -7.8, 12, 0.5*pi, 39)
#' vitesse_x(65, 0.003, 8, 18, 23, 0.08, 6.12, -12.8, 25, 0.25*pi, 22)
vitesse_x <- function(t, lambda = LAMBDA, m_0 = M_I, M_0 = M_N, Ve = V_E, ro = Ro, Cx = C_X, Ax = A_X, Vr = V_R, alpha = ALPHA, Vo = V_0){
  return(((M_0 + m_0)*Vo*cos(alpha) + Ve*m_0*(1-exp(-lambda*t)) - 0.5*ro*Cx*Ax*Vr*Vr*t)/(M_0 + m_0*exp(-lambda*t)))
}

# FUNCTION THAT RETURNS THE ROUNDED VALUE OF THE HORIZONTAL VELOCITY COMPONENT (the speed in the x-direction) DRIVING THE PROJECTILE.
#' Title PRINTING THE ROUNDED VALUE OF HORIZONTAL VELOCITY
#'
#' @param t the time date in seconds
#' @param lambda the Fluid Expulsion Parameter
#' @param m_0 the Initial Mass of the Replenishment Fluid prior to ejection
#' @param M_0 the Net Structural Mass
#' @param Ve the magnitude of the Ejection Velocity of the replenishment fluid
#' @param ro the Density of Atmospheric Air pressing against the projectile's surface along the x-axis
#' @param Cx the Wind Drag Coefficient acting on the projectile's surface along the x-axis
#' @param Ax the corresponding Reference Area subject to atmospheric wind fluctuations along the x-axis
#' @param Vr the magnitude of the Wind Flow Velocity acting on the projectile
#' @param alpha the Launch Angle of the projectile in the x,z plane
#' @param Vo the magnitude of the Propulsion Velocity
#'
#' @returns the Propulsion Velocity string value with the unit "m/s"
#' @export
#'
#' @examples
#' vitesse_x(51, 0.008, 9, 21, 9, 0.01, 4.12, -7.8, 12, 0.5*pi, 39)
#' vitesse_x(65, 0.003, 8, 18, 23, 0.08, 6.12, -12.8, 25, 0.25*pi, 22)
vitesse_x_en_m_par_s <- function(t, lambda = LAMBDA, m_0 = M_I, M_0 = M_N, Ve = V_E, ro = Ro, Cx = C_X, Ax = A_X, Vr = V_R, alpha = ALPHA, Vo = V_0){
  return(cat(round(vitesse_x(t)), "m/s environ"))
}

# functions are initialized with starting values and can be customized for specific situations
