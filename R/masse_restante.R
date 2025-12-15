#The system is defined by the following initial parameters:

# m_0 represents the Initial Mass of the Replenishment Fluid prior to ejection
# lambda is the fluid expulsion parameter

M_I <- 5
LAMBDA <- 0.02

# FUNCTION RETURNING THE REMAINING MASS WITHIN THE PROJECTILE (MASS OF REPLENISHMENT FLUID PRESENT) AT TIME t
#' Title REMAINING MASS WITHIN THE PROJECTILE
#'
#' @param t the time date in seconds
#' @param lambda the fluid expulsion parameter
#' @param m_0 represents the Initial Mass of the Replenishment Fluid prior to ejection in Kg
#'
#' @returns THE REMAINING MASS WITHIN THE PROJECTILE
#' @export
#'
#' @examples
#' masse_restante(15, 0.02, 8)
#' masse_restante(30, 0.005, 12)
masse_restante <- function(t, lambda = LAMBDA, m_0 = M_I){
  return(m_0*exp(-lambda*t))
}

# functions are initialized with starting values and can be customized for specific situations
