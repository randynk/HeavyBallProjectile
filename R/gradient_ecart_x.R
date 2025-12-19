#The system's behavior is governed by the following initial parameters:

# alpha is the Launch Angle of the projectile in the (x,z) plane.
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

# THE DETERMINISTIC FORM OF THE GRADIENT FOR OUR ERROR FUNCTION (Ecart()), WHICH WE ARE SEEKING TO MINIMIZE.
#' GRADIENT FUNCTION
#'
#' @param t the time date in seconds
#' @param lambda the Fluid Expulsion Parameter
#' @param m_0 the Initial Mass of the Replenishment Fluid prior to ejection
#' @param M_0 the Net Structural Mass
#' @param Ve the magnitude of the Ejection Velocity of the replenishment fluid
#' @param alpha  the Launch Angle of the projectile in the x,z plane
#' @param Vo the magnitude of the Propulsion Velocity
#'
#' @returns the gradient of our error function
#' @export
#'
#' @examples
#' gradient_ecart_x(13)
#' gradient_ecart_x(13, 0.02, 9, 32)
#' gradient_ecart_x(13, 0.02)
gradient_ecart_x <- function(t, lambda = LAMBDA, m_0 = M_I, M_0 = M_N, Ve = V_E, alpha = ALPHA, Vo = V_0){
  gradient_alpha <- (-(M_0+m_0)*Vo*sin(alpha)/(lambda*M_0))*log((M_0*exp(lambda*t) + m_0)/(M_0+m_0))
  gradient_Vo <- ((M_0 + m_0)*cos(alpha)/(lambda*M_0))*log((M_0*exp(lambda*t) + m_0)/(M_0 + m_0))
  gradient_Ve <- (m_0/M_0)*(2*log((M_0*exp(lambda*t) + m_0)/(M_0 + m_0)) - lambda*t)
  return(gradient_alpha + gradient_Vo + gradient_Ve)
}

# Heavy Ball Method functions are initialized with starting values and can be customized for specific situations
