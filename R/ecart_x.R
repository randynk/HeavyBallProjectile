library(gsl)

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

# FUNCTION THAT RETURNS HOW FAR THE PROJECTILE HAS TRAVELED ALONG THE HORIZONTAL (x) AXIS

#' DISTANCE ALONG X
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
#' @returns the projectile position along x at time t
#' @import gsl
#' @export
#'
#' @examples
#' distance_x(51, 0.008, 9, 21, 9, 0.01, 4.12, -7.8, 12, 0.5*pi, 39)
#' distance_x(65, 0.003, 8, 18, 23, 0.08, 6.12)
#' distance_x(51)
#' distance_x(65, 0.003, 8)
distance_x <- function(t, lambda = LAMBDA, m_0 = M_I, M_0 = M_N, Ve = V_E, ro = Ro, Cx = C_X, Ax = A_X, Vr = V_R, alpha = ALPHA, Vo = V_0){# distance rpère l'abcisse de la position du projectile à la date t
  distance <- (((M_0 + m_0)*Vo*cos(alpha) + m_0*Ve)/(lambda*M_0))*log((M_0*exp(lambda*t) + m_0)/(M_0 + m_0))
  distance <- distance + (Ve/lambda)*log((M_0 + m_0*exp(-lambda*t))/(M_0 + m_0))
  distance <- distance + ((ro*Cx*Ax*Vr*Vr)/(2*lambda*M_0))*(((1/lambda)*dilog((-M_0*exp(lambda*t))/m_0))-((1/lambda)*dilog(-M_0/m_0))+(t*log(m_0/(M_0*exp(lambda*t) + m_0))))
  return(distance)
}

# FUNCTION THAT RETURNS THE ABSOLUTE DIFFERENCE (OR ABSOLUTE ERROR)
# BETWEEN THE PROJECTILE'S CURRENT HORIZONTAL POSITION (x-coordinate)
# AND THE TARGET'S HORIZONTAL POSITION (x-coordinate)
#' ABSOLUTE DISTANCE ERROR
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
#' @param abcisse_cible the target horizontal position
#'
#' @returns THE ABSOLUTE DIFFERENCE (OR ABSOLUTE ERROR) BETWEEN THE PROJECTILE'S CURRENT HORIZONTAL POSITION AND THE TARGET'S HORIZONTAL POSITION
#' @export
#'
#' @examples
#' ecart_x(51, abcisse_cible)
#' ecart_x(21, abcisse_cible)
#' ecart_x(51)
#' ecart_x(6, abcisse_cible)
ecart_x <- function(t, lambda = LAMBDA, m_0 = M_I, M_0 = M_N, Ve = V_E, ro = Ro, Cx = C_X, Ax = A_X, Vr = V_R, alpha = ALPHA, Vo = V_0, abcisse_cible = ABCISSE_CIBLE){
  return(abs(distance_x(t) - abcisse_cible))
}

# Heavy Ball Method functions are initialized with starting values and can be customized for specific situations
