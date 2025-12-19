#The system is defined by the following initial parameters:

# m_0 represents the Initial Mass of the Replenishment Fluid prior to ejection
# lambda is the fluid expulsion parameter

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

# FUNCTION RETURNING THE CONSUMED REPLENISHMENT MASS AT TIME t
#' CONSUMED REPLENISHMENT MASS
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
