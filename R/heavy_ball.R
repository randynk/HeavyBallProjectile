library(ggplot2)
library(tidyr)

#The system's behavior is governed by the following initial parameters:

# alpha is the Launch Angle of the projectile in the (x,z) plane.
# Vo is the magnitude of the Propulsion Velocity
# Ve is the magnitude of the Ejection Velocity of the replenishment fluid, which is collinear with V0.
# m0 represents the Initial Mass of the Replenishment Fluid prior to ejection,
# M0 is the Net Structural Mass (or Dry Mass) of the projectile
# lambda is the Fluid Expulsion Parameter (or Escape Parameter) governing the rate of fluid loss.
# alpha_min is the lowest possible alpha value
# alpha_max is highest possible alpha value
# Vo_min is the lowest possible Vo value
# Vo_max is the highest possible Vo value
# Ve_min is the lowest possible Ve value
# Ve_max is the highest possible Ve value
# heavy_alpha the algorithm second main parameter
# heavy_gamma the algorithm first main parameter
# limite the step limit of the loop algorithm
# tolerance error tolerance

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
#' @import ggplot2
#' @import tidyr
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

# FUNCTION THAT RETURNS THE OPTIMAL VALUES OF THE OPTIMAL PARAMETERS
#' HEAVY BALL ALGORITHM
#'
#' @param t the time date in seconds
#' @param heavy_alpha the algorithm second main parameter
#' @param heavy_gamma the algorithm first main parameter
#' @param alpha_min the lowest possible alpha value
#' @param alpha_max the highest possible Vo value
#' @param Vo_min the lowest possible Vo value
#' @param Vo_max the highest possible Vo value
#' @param Ve_min the lowest possible Ve value
#' @param Ve_max the highest possible Ve value
#' @param limite the step limit of the loop algorithm
#' @param tolerance error tolerance
#' @param lambda the Fluid Expulsion Parameter
#' @param m_0 the Initial Mass of the Replenishment Fluid prior to ejection
#' @param M_0 the Net Structural Mass
#'
#' @returns the optimal parameters, a dataframe, a picture, an csv file, the convergence parameters
#' @export
#'
#'
heavy_ball <- function(t = DATE, heavy_alpha = HEAVY_ALPHA, heavy_gamma = HEAVY_GAMMA, alpha_min = ALPHA_MIN, alpha_max = ALPHA_MAX, Vo_min = Vo_MIN, Vo_max = Vo_MAX, Ve_min = Ve_MIN, Ve_max = Ve_MAX, limite = LIMITE, tolerance = TOLERANCE, alpha_test = ALPHA_TEST, Vo_test = Vo_TEST, Ve_test = Ve_TEST, abcisse_cible = ABCISSE_CIBLE, lambda = LAMBDA, m_0 = M_I, M_0 = M_N){

  temps <- seq(0, 5, 0.05)
  # WE INITIALIZE THE DECISION VARIABLES
  alpha <- alpha_max
  Vo <- Vo_max
  Ve <- Ve_max
  convergence <- 0

  # WE PREPARE THE ARRAYS (or DATA TABLES) THAT WILL PROGRESSIVELY STORE THE VALUES OF OUR DECISION PARAMETERS
  historique_gradient <- numeric(limite)

  historique_alpha <- c(alpha)
  historique_Vo <- c(Vo)
  historique_Ve <- c(Ve)

  V_k <- 0

  for (etape in 1:limite) {

    # WE COMPUTE THE GRADIENT AT EVERY STEP
    grad <- gradient_ecart_x(t, lambda, m_0, M_0, Ve, alpha, Vo)

    V_k <- heavy_gamma*V_k - heavy_alpha*grad

    alpha <- alpha + V_k
    Vo <- Vo + V_k
    Ve <- Ve + V_k

    historique_alpha <- c(historique_alpha, alpha)
    historique_Vo <- c(historique_Vo, Vo)
    historique_Ve <- c(historique_Ve, Ve)

    historique_gradient[etape] <- grad

    cat("\n", etape, " :: ", contrainte(alpha, Vo, Ve), " :: ", abs(grad), ", alpha ::", alpha, ", Vo :: ", Vo, ", Ve :: ", Ve)
    if(abs(grad)<tolerance & contrainte(alpha, Vo, Ve)==TRUE){# WE COMPARE THE GRADIENT TO THE LIMIT VALUE AT EVERY STEP

      # WE COLLECT ALL THE VALUES OF THE DECISION VARIABLES TESTED UP TO THE POINT OF CONVERGENCE.
      # THESE VALUES WILL ALLOW US TO PLOT (or VISUALIZE) THEIR CONVERGENCE PATHS
      historique_gradient <- historique_gradient[1:etape]
      historique_alpha <- historique_alpha[1:etape]
      historique_Vo <- historique_Vo[1:etape]
      historique_Ve <- historique_Ve[1:etape]
      evolution <- c(seq(1:etape))

      # WE CREATE THE CORRESPONDING DATA FRAME
      projectile <- data.frame(
        evolution = evolution,
        alpha = historique_alpha,
        Vo = historique_Vo,
        Ve = historique_Ve,
        Gradient = historique_gradient
      )

      #View(projectile)

      # TURNING THE LARGE FRAME INTO A LONG FRAME EASIER TO MANAGE
      projectile_long <- pivot_longer(
        projectile,
        cols = c(alpha, Vo, Ve, Gradient),
        names_to = "parametre",
        values_to = "valeur"
      )

      # CREATING AND SAVING THE PLOTS ON A PICTURE
      ggplot(projectile_long, aes(x = evolution, y = valeur)) +
        geom_line(color = "blue", linewidth = 1) +  # linewidth au lieu de size
        facet_wrap(~ parametre, scales = "free_y", ncol = 2) +
        labs(title = "Évolution des paramètres", x = "Itération", y = "Valeur") +
        theme_minimal()

      ggsave("graphiques_projectile.png", width = 10, height = 8, dpi = 300)

      #write.csv(projectile, "Donnees_projectile.csv")
      convergence <- etape

      break
    }
  }# WE DISPLAY (or PRINT) THE OPTIMAL PARAMETERS FOUND BY THE ALGORITHM

  projectiles <- data.frame(
    temps = temps,
    ecart_non_optimal = distance_x(t = temps, alpha = alpha_test, Vo = Vo_test, Ve = Ve_test) - abcisse_cible,
    ecart_optimal = distance_x(t = temps, alpha = historique_alpha[convergence], Vo = historique_Vo[convergence], Ve = historique_Ve[convergence] - abcisse_cible)
  )

  # TURNING THE LARGE FRAME INTO A LONG FRAME EASIER TO MANAGE
  projectiles_long <- pivot_longer(projectiles,
                                   cols = c(ecart_non_optimal, ecart_optimal),
                                   names_to = "type",
                                   values_to = "erreur")

  # CREATING AND SAVING  THE COMPARISON PLOT ON A PICTURE
  graph <- ggplot(projectiles_long, aes(x = temps, y = erreur, color = type)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = c("ecart_non_optimal" = "red",
                                  "ecart_optimal" = "blue"),
                       labels = c("Non optimale", "Optimale")) +
    labs(x = "Temps (s)",
         y = "Erreur sur l'abcisse",
         title = "Comparaison des erreurs",
         color = "Type de trajectoire") +
    theme_minimal()

  ggsave("graphique_projectiles.png", width = 10, height = 8, dpi = 300)
  print(graph)

  return(cat("\n\nConvergence atteinte à l'iteration ", convergence, "\n", "Les parametres optimaux sont :\nalpha :: ", historique_alpha[convergence]/pi, "π , ", "\nVo    :: ", historique_Vo[convergence], "\nVe    :: ", historique_Ve[convergence]))
}

# Heavy Ball Method functions are initialized with starting values and can be customized for specific situations
