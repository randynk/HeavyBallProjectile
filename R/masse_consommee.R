#The system is defined by the following initial parameters:

# m_0 represents the Initial Mass of the Replenishment Fluid prior to ejection
# lambda is the fluid expulsion parameter

M_I <- 5
LAMBDA <- 0.02

# FUNCTION RETURNING THE CONSUMED REPLENISHMENT MASS AT TIME t
#' Title CONSUMED REPLENISHMENT MASS
#'
#' @param t the time date in seconds
#' @param lambda the fluid expulsion parameter
#' @param m_0 represents the Initial Mass of the Replenishment Fluid prior to ejection in Kg
#'
#' @returns THE CONSUMED REPLENISHMENT MASS AT TIME t
#' @export
#'
#' @examples
#' masse_consommee(15, 0.07, 9)
#' masse_consommee(10, 0.04, 8)
masse_consommee <- function(t, lambda = LAMBDA, m_0 = M_I){
  return(m_0*(1-exp(-lambda*t)))
}

# functions are initialized with starting values and can be customized for specific situations
