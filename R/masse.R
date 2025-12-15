#The system is defined by the following initial parameters:

# m_0 represents the Initial Mass of the Replenishment Fluid prior to ejection, and
# M_0 is the Net Structural Mass (or Dry Mass) of the projectile.
# lambda is the fluid expulsion parameter

M_N <- 12
M_I <- 5
LAMBDA <- 0.02

# FUNCTION RETURNING THE TOTAL MASS OF THE PROJECTILE AT TIME t
#' Title TOTAL MASS
#'
#' @param t the time date in seconds
#' @param lambda is the fluid expulsion parameter
#' @param m_0 the Initial Mass of the Replenishment Fluid prior to ejection in Kg
#' @param M_0 the Net Structural Mass
#'
#' @returns THE TOTAL MASS OF THE PROJECTILE AT TIME t
#' @export
#'
#' @examples
#' masse(34, 0.03, 8, 12)
#' masse(49, 0.06, 4, 15)
masse <- function(t, lambda = LAMBDA, m_0 = M_I, M_0 = M_N){
  return(M_0 + m_0*exp(-lambda*t))
}

# functions are initialized with starting values and can be customized for specific situations
