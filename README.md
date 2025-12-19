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

# Heavy Ball Method functions are initialized with starting values and can be customized for specific situations

# You can easily manage it your self

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
