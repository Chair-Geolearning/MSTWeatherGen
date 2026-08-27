# Define spatial and temporal resolutions
source("MSTWeatherGen/R/NSMuST_test3v.R")

# Define spatial and temporal resolutions
spatial_resolution  = 0.4
temporal_resolution = 1

# Generate spatial grid for simulation Versuion originale
s1_seq = seq(0, 10, by = spatial_resolution)
s2_seq = seq(0, 8, by = spatial_resolution)
grid_spatial = expand.grid(s1 = s1_seq, s2 = s2_seq)
spatial_coordinates = as.matrix(grid_spatial) 
ns <- nrow(spatial_coordinates)

## SPATIAL 
# Version pour faire correspondre a nos nombre de location ie 108
# Charger les 108 coordonnées originales pour faire correspondre en terme de dates fictives
utils::data(
  "coordinates",
  package = "MSTWeatherGen"
)
spatial_coordinates <- as.matrix(
  coordinates[, c("longitude", "latitude")]
)
ns <- nrow(spatial_coordinates)

## TEMPORELLE
# Define temporal sequence A changer ici la taille ici, correpondance avec la taille de data sur la plage 2018-2021
t_seq = seq(1, 1461, by = temporal_resolution)
nt = length(t_seq)  
# Dates : 2018-01-01 à 2021-12-31
dates_Z1 <- seq(
  from = as.Date("2018-01-01"),
  to   = as.Date("2021-12-31"),
  by   = "day"
)
nt <- length(dates_Z1)
# Temps numérique utilisé par le simulateur
temporal_resolution <- 1

t_seq <- seq(
  from = 1,
  by = temporal_resolution,
  length.out = nt
)


p = 3 # Number of variables
L = 3000 # Number of waves

# Définition des Matern
rs = c(0.5,1,2) # paramètres a dans le papier
nu = c(0.5,1,2) 
# Define Matern covariance function parameters
matern_fun1 = function(t_val, s_coord) {
  list(nu=nu[1], r=rs[1])
}
matern_fun2 = function(t_val, s_coord) {
  list(nu=nu[2], r=rs[2])
}
matern_fun3 = function(t_val, s_coord) {
  list(nu=nu[3], r=rs[3])
}
SpectralPars = list(matern_fun1, matern_fun2, matern_fun3)


# Define covariance structure (between variables) function
Sigma_fun = function(t_val, s_coord) {
  M = matrix(0.85,ncol=3,nrow=3)
  diag(M) = 1
  return(M)
}

# Define anisotropy functions for Isotropic case
aniso_fun1 = aniso_fun2 = aniso_fun3 = function(t_val, s_coord) {
  diag(2)
}
anisotropies = list(aniso_fun1, aniso_fun2, aniso_fun3)


# Define simulation parameters
params = list(
  p = p,
  nt = nt,
  t = t_seq,
  A = c(0.2,0.5,0.8), # paramètres A_i
  a = 1/2,   # a=d
  alpha = 1, # b=e, with c=f=1
  delta = 0, # does not exist in MSTWeatherGen
  b = 1, # non-separability - does not exist in MSTWeatherGen
  anisotropies = anisotropies,
  SpectralPars = SpectralPars,
  Sigma_fun    = Sigma_fun
)

# Run spatio-temporal simulation 
set.seed(14347)
Z1 = SimulateParsimNS(
  L                  = L,
  ns                 = ns,
  nt                 = nt,
  p                  = p,
  params             = params,
  spatial_coordinates = spatial_coordinates,
  SpectralDensity = MaternSpectralDensity3D,
  parallelize        = F, 
  batch_size = 100,
  n_cores = 4
)

r_t = c(0.2,0.4,0.6)
rho  = Sigma_fun(1,1)
Z_T1 = TemporalSim(nt,r_t,rho,a=rep(1,p),nu=rep(1,p),A=rep(0,p))
plot(1:30,Z_T1[,1],type="l")
points(1:30,Z_T1[,2],type="l",lty=2)
points(1:30,Z_T1[,3],type="l",lty=3)

Z_T2 = TemporalSim(nt,r_t,rho,a=rs,nu=nu,A=c(0.2,0.5,0.8))
plot(1:30,Z_T2[,1],type="l")
points(1:30,Z_T2[,2],type="l",lty=2)
points(1:30,Z_T2[,3],type="l",lty=3)

Z = Z1
for (i in seq_len(ns)){
  for (k in seq_len(p)){
    Z[,i,k] = Z_T2[,k]*Z1[,i,k] + Z_T1[,k]
  }
}









# Plot the variable
x_coords = spatial_coordinates[, 1]
y_coords = spatial_coordinates[, 2]

df_v1 = data.frame(
  ZZ1      = Z[1, , 2],
  X        = x_coords,
  Y        = y_coords,
  Variable = paste("V1"),
  Time     =  1
)

df_v2 = data.frame(
  ZZ2      = Z[1, , 3],
  X        = x_coords,
  Y        = y_coords,
  Variable = paste("V3"),
  Time     =  1
)


df_list = list()
df_list[[length(df_list) + 1]] = df_v1
df_list[[length(df_list) + 1]] = df_v2
df = bind_rows(df_list)
df = df %>% mutate(Time = "Time 1")

time_steps_vec = 1
df_var1 = df %>% filter(Variable == "V1")
df_var2 = df %>% filter(Variable == "V3")

color_scale_fun = function(...) scale_fill_viridis_c(option = "H", ...)
theme_fun = theme_bw

p_1 = ggplot(df_v1, aes(x = X, y = Y, fill = ZZ1)) +
  geom_raster() +
  color_scale_fun() +
  #facet_wrap(~ Time, nrow = 1) +
  theme_fun() +
  labs(
    title = paste("nu=1 ; time=1"),
    x = "x coordinate",
    y = "y coordinate",
    fill = NULL
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    strip.text = element_text(size = 10)
  )


p_2 = ggplot(df_v2, aes(x = X, y = Y, fill = ZZ2)) +
  geom_raster() +
  color_scale_fun() +
  #facet_wrap(~ Time, nrow = 1) +
  theme_fun() +
  labs(
    title = paste("nu=1 ; time = 3"),
    x = "x coordinate",
    y = "y coordinate",
    fill = NULL
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    strip.text = element_text(size = 10)
  ) 
pl = p_1 + p_2 + plot_layout(guides = 'collect')
pl

ggplot2::ggsave(plot =pl,"X:/MSTWeatherGEN/Simu2times.pdf", width = 28, height = 14, units = "cm")
