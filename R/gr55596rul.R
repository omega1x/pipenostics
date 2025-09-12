# arguments
maop <- 1.6  # maximum allowable operating pressure - MAOP, [MPa]
d <- 530  # nominal (osmyside?) diameter of pipe, [mm]
wth <- 7  # nominal wall thickness of pipe, [mm]

# measured maximum depth of the corroded area, [mm]:
depth <- c(1.7, 0.7, 0.9, 2.0, 1.2, 1.0, 1.3, 1.5, 0.6, 1.1, 1.5)

maximum_allowable_stress_at20 <- 147  # [Mpa]  # allowed stress

# ultimate tensile strength (\emph{smys}) or specified minimum tensile strength
# (\emph{SMTS}) as a characteristic of steel strength, [\emph{MPa}].
smys <- 420

lifetime <- 18  # [years]

confidence_level <- 0.9 # [0.88, 0.99]

# code
S_0 <- 0.05  # recommended pipe wall width tolerance in GOST R 55596-2013, []

# recommended strength reduction coefficient for seamless pipe in
# GOST R 55596-2013, []
phi <- 1

n <- length(depth)
U <- qnorm(confidence_level)

delta <- 1 - (wth - depth) / wth  # formula D.1 modified for depths
delta_mean <- mean(delta) # formula D.2
S_d <- sqrt(var(delta) - S_0^2)  # formula D.4, combined with formula D.3
S_R <- maop * d / (2 * phi * maximum_allowable_stress_at20 + maop) # formula 7.1

# Formula D.5:
a <- .25 * S_R / wth * maximum_allowable_stress_at20 / smys
b <- delta_mean + U * S_d * (1 + U * sqrt(U^(-2) / (n - 2) + 1 / (2 * n - 8)))

gr55596rul <- lifetime * a / b


# test that
stopifnot(
  round(gr55596rul, 1) == 2.7
)
