devtools::load_all()
library(ggplot2); library(patchwork); library(ggpubr)

# ══════════════════════════════════════════════════════════════════════════════
#  FONCTIONS 
# ══════════════════════════════════════════════════════════════════════════════

SimulateParsimNS = function(L, ns, nt, p, params, spatial_coordinates, SpectralDensity,
                            parallelize = FALSE, n_cores = 2, batch_size = 100) {
  Sig1 = SimulateGauIncrements(params)
  aniso_precomp_list = lapply(params$anisotropies, precompute_aniso,
                              t_vals = params$t, spatial_coordinates = spatial_coordinates)
  aniso_5D = array(0, dim = c(p, nt, ns, 2, 2))
  det_array_all = array(0, dim = c(p, nt, ns))
  for (i in seq_len(p)) {
    aniso_list_t = aniso_precomp_list[[i]]
    for (t_idx in seq_len(nt)) for (s_idx in seq_len(ns)) {
      AA = aniso_list_t[[t_idx]][[s_idx]]
      aniso_5D[i, t_idx, s_idx, , ] = AA
      det_array_all[i, t_idx, s_idx] = det(AA)
    }
  }
  rm(aniso_precomp_list); gc()
  matern_precomp_list = lapply(params$SpectralPars, precompute_matern_pars,
                               t_vals = params$t, spatial_coordinates = spatial_coordinates)
  nu_3D = array(0, dim = c(p, nt, ns)); r_3D = array(0, dim = c(p, nt, ns))
  for (i in seq_len(p)) {
    nu_3D[i, , ] = matern_precomp_list[[i]]$nu
    r_3D[i,  , ] = matern_precomp_list[[i]]$r
  }
  rm(matern_precomp_list); gc()
  SigmaFun_precomp = precompute_sigma_fun(params$Sigma_fun, params$t, spatial_coordinates)
  SigmaFun_4D = array(0, dim = c(nt, ns, p, p))
  for (t_idx in seq_len(nt)) for (s_idx in seq_len(ns))
    SigmaFun_4D[t_idx, s_idx, , ] = SigmaFun_precomp[[t_idx]][[s_idx]]
  rm(SigmaFun_precomp); gc()
  d = dim(spatial_coordinates)[2]; n = nt * ns
  cst = (2 * sqrt(pi))^(-2)
  
  single_iteration = function(iter) {
    Wl   = matrix(Sig1 %*% rnorm(nt * p), nrow = nt, ncol = p, byrow = FALSE)
    Xil  = SimulateXi(1); Vl = rnorm(d); phil = runif(1, 0, 2 * pi); Al = rnorm(p)
    t_Vl_Vl = sqrt(sum(Vl^2)); temp_Wl = (1 / sqrt(2)) * t_Vl_Vl * Wl
    omegal = sqrt(2 * Xil) * Vl; pdfXi_Xil = pdfXi(Xil); x = sqrt(2) * Vl
    pdfi = GaussianSpectralDensity(x, Sigma = diag(d))
    localZ = array(0, dim = c(nt, ns, p)); temp = spatial_coordinates %*% omegal
    for (i_ in seq_len(p)) {
      spec_raw = SpectralDensity(Xil, nu_3D[i_, , ], r_3D[i_, , ])
      localSpec_mat = spec_raw / pdfXi_Xil
      x1 = x[1]; x2 = x[2]
      quad_form = x1^2 * aniso_5D[i_, , , , ][,,1,1] +
        2*x1*x2 * aniso_5D[i_, , , , ][,,1,2] +
        x2^2   * aniso_5D[i_, , , , ][,,2,2]
      det_array = matrix(det_array_all[i_, , ], nrow = nt, ncol = ns)
      gauss_mat = cst * sqrt(det_array) * exp(-quad_form / 4) / pdfi[1]
      scale_local_mat = matrix(
        crossprod(Al, matrix(aperm(SigmaFun_4D[, , i_, ], c(3,1,2)), nrow = p)),
        nrow = nt, ncol = ns) * sqrt(localSpec_mat)
      cosarg_mat = temp_Wl[, i_] + matrix(temp[,1], nrow=nt, ncol=ns, byrow=TRUE) + phil
      localZ[,, i_] = scale_local_mat * sqrt(gauss_mat) * cos(cosarg_mat)
    }
    localZ
  }
  
  if (!parallelize) {
    Z_accum = array(0, dim = c(nt, ns, p))
    for (ell in seq_len(L)) {
      iter = single_iteration(ell)
      if (all(!is.finite(iter))) { iter = 0; L = L - 1 }
      Z_accum = Z_accum + iter
    }
    return(sqrt(2 / L) * Z_accum)
  } else {
    library(parallel)
    Z_accum = array(0, dim = c(nt, ns, p)); valid_count = 0
    for (batch in seq_len(ceiling(L / batch_size))) {
      idx = seq((batch-1)*batch_size+1, min(batch*batch_size, L))
      Z_batch = mclapply(idx, single_iteration, mc.cores = n_cores)
      ok = sapply(Z_batch, function(x) all(is.finite(x)))
      for (ell in which(ok)) Z_accum = Z_accum + Z_batch[[ell]]
      valid_count = valid_count + sum(ok)
    }
    return(sqrt(2 / valid_count) * Z_accum)
  }
}

# ─────────────────────────────────────────────────────────────────────────────

TemporalSim = function(nt, r_t, rho, a, nu, A) {
  p = length(r_t)
  W = array(0, dim = c(nt, p))
  rt_ij  = sqrt(0.5 * outer(r_t^2, r_t^2, '+'))
  w      = sqrt(outer(r_t, r_t, '*')) / rt_ij
  beta   = rho * w
  nu_ij  = 0.5 * outer(nu, nu, '+')
  nu_factor = gamma(nu_ij) / outer(sqrt(gamma(nu)), sqrt(gamma(nu)), '*')
  a2_ij  = 0.5 * outer(a^2, a^2, '+')
  a_factor = outer(a^nu, a^nu, '*') / sqrt(a2_ij)^nu_ij
  A_ij   = sqrt(outer(1 - A^2, 1 - A^2, '*'))
  beta   = beta * a_factor * nu_factor * A_ij
  for (k in seq_len(p)) {
    Cov = exp(-r_t[k] * abs(outer(seq_len(nt), seq_len(nt), '-')))
    W[, k] = t(chol(Cov)) %*% rnorm(nt)
  }
  W %*% chol(beta)
}

# ─────────────────────────────────────────────────────────────────────────────
# Fonction de Said pour plot les graphes

plot_multivariate_st_covariances <- function(vgm, names) {
  # Create an empty plot placeholder for upper triangle
  empty_plot <- ggplot2::ggplot() +
    ggplot2::theme_void()
  
  # Initialize an empty plot matrix
  plot_matrix <- matrix(list(empty_plot), nrow = length(names), ncol = length(names))
  
  # Fill the matrix with plots, lower triangle and diagonal
  for (i in 1:length(names)) {
    for (j in 1:i) {
      variable1 <- names[i]
      variable2 <- names[j]
      filtered_vgm <- subset(vgm, v1 == variable1 & v2 == variable2 | v1 == variable2 & v2 == variable1)
      
      p <- ggplot2::ggplot(filtered_vgm, ggplot2::aes(x = dist, color = factor(lagtime))) +
        ggplot2::geom_line(ggplot2::aes(y = vgmsim_true/2), linetype = "dashed", linewidth = 0.6) + # Division ici par deux de cov Gneiting pour facteur d'echelle
        ggplot2::geom_point(ggplot2::aes(y = cov)) +
        ggplot2::labs(title = paste(variable1, "vs", variable2), x = "", y = "") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 8),
          legend.position = "none"
        )
      last_row <- length(names)
      mid_col <- round(length(names) / 2)
      # Add xlab to the bottom middle plot
      if (i == last_row && j == mid_col) {
        p <- p + ggplot2::labs(x = "Distance (degree)") +
          ggplot2::theme(axis.text.x = ggplot2::element_text(), axis.ticks.x = ggplot2::element_line())
      }
      
      # Add ylab to the middle left plot
      if (i == mid_col && j == 1) {
        p <- p + ggplot2::labs(y = "Covariance") +
          ggplot2::theme(axis.text.y = ggplot2::element_text(), axis.ticks.y = ggplot2::element_line())
      }
      plot_matrix[j, i] <- list(p)
    }
  }
  # Print the arranged plot layout
  
  legend_plot <- ggplot2::ggplot(filtered_vgm, ggplot2::aes(x = dist, color = factor(lagtime))) +
    ggplot2::geom_line(ggplot2::aes(y = vgmsim)) +
    ggplot2::geom_point(ggplot2::aes(y = cov)) +
    ggplot2::labs(title = paste(variable1, "vs", variable2), x = "", y = "") +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_discrete("Time lag (day)") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 8))
  leg <- ggpubr::get_legend(legend_plot)
  num_vars <- length(names)
  
  plot_matrix[length(names), 1] <- list(leg)
  
  # Convert matrix to patchwork layout
  plot_layout <- patchwork::wrap_plots(plot_matrix)
  
  return(plot_layout)
}

# ══════════════════════════════════════════════════════════════════════════════
# PARAMÈTRES VRAIS
# ══════════════════════════════════════════════════════════════════════════════

tmax      <- 3
nb_z      <- 15 # nombre de simulations
nb_estimation <- nb_z

names     <- c("v1", "v2", "v3")

rs_true   <- c(0.2, 0.5, 1.0)
nu_true   <- c(0.5, 1.0, 2.0)
r_t_true  <- c(0.2, 0.4, 0.6)
A_true    <- c(0, 0, 0)
rho_cross <- 0.7

nb_points_distance <- 20   # NOmbre de points pour la cov empirique

ep <- generate_variable_index_pairs(names)

# Mise en forme de maniere propre des parametres
parm_true <- do.call(rbind, lapply(seq_len(nrow(ep)), function(i) {
  vi1 <- which(names == ep[i, "v1"])
  vi2 <- which(names == ep[i, "v2"])
  data.frame(
    v1 = ep[i, "v1"], v2 = ep[i, "v2"],
    a = 0.5, b = 1, c = 1, d = 0.5, e = 1,
    Ai = A_true[vi1], Aj = A_true[vi2],
    aii = rs_true[vi1], ajj = rs_true[vi2],
    nuii = nu_true[vi1], nujj = nu_true[vi2],
    rho1ij = ifelse(vi1 == vi2, 1, rho_cross),
    r2ii = r_t_true[vi1], r2jj = r_t_true[vi2],
    r1ii = r_t_true[vi1], r1jj = r_t_true[vi2],
    rho2ij = ifelse(vi1 == vi2, 1, rho_cross)
  )
}))

# ══════════════════════════════════════════════════════════════════════════════
# DONNÉES ET PETITES FONCTIONS
# ══════════════════════════════════════════════════════════════════════════════

# GRILLE SPATIALE
spatial_resolution  = 0.4
s1_seq = seq(0, 10, by = spatial_resolution)
s2_seq = seq(0, 8,  by = spatial_resolution)
grid_spatial = expand.grid(s1 = s1_seq, s2 = s2_seq)
spatial_coordinates = as.matrix(grid_spatial)   # matrice → pour generate_Z

t_seq = seq(1, 40, by = 1)
dates = seq(as.Date('2018-01-01'), as.Date('2018-01-01') + (length(t_seq) - 1), by = 'day')
wt_id = 1:length(t_seq)

# data.frame avec noms longitude/latitude → pour empirical_st_cov_pair et build_vgm_for_plot
coordinates = as.data.frame(spatial_coordinates)
names(coordinates) = c('longitude', 'latitude')

# ─────────────────────────────────────────────────────────────────────────────

generate_Z <- function(t_seq, spatial_coordinates){
  ns = nrow(spatial_coordinates)
  nt = length(t_seq)
  
  p = 3 # Number of variables observed
  L = 3000 # Number of waves
  
  # Définition des Matern
  rs = rs_true # paramètres aii dans le papier (anciennement 0.5,1,2) On a divise par deux
  nu = nu_true # nuii
  
  # Define Matern covariance function parameters
  matern_fun1 = function(t_val, s_coord) { list(nu=nu[1], r=rs[1]) }
  matern_fun2 = function(t_val, s_coord) { list(nu=nu[2], r=rs[2]) }
  matern_fun3 = function(t_val, s_coord) { list(nu=nu[3], r=rs[3]) }
  SpectralPars = list(matern_fun1, matern_fun2, matern_fun3)
  
  # Define covariance structure (between variables) function
  Sigma_fun = function(t_val, s_coord) {
    M = matrix(0.7,ncol=3,nrow=3)
    diag(M) = 1
    return(M)
  }
  
  # Define anisotropy functions for Isotropic case
  aniso_fun1 = aniso_fun2 = aniso_fun3 = function(t_val, s_coord) { diag(2) }
  anisotropies = list(aniso_fun1, aniso_fun2, aniso_fun3)
  
  # Define simulation parameters
  params = list(
    p = p, nt = nt, t = t_seq,
    A = A_true, # paramètres A_i
    a = 1/2,      # a=d
    alpha = 1,    # b=e, with c=1
    delta = 0,    # does not exist in MSTWeatherGen
    b = 1,        # non-separability - does not exist in MSTWeatherGen
    anisotropies = anisotropies,
    SpectralPars = SpectralPars,
    Sigma_fun    = Sigma_fun,
    rs = rs, nu = nu)
  
  # Run spatio-temporal simulation
  Z1 = SimulateParsimNS(
    L                   = L,
    ns                  = ns,
    nt                  = nt,
    p                   = p,
    params              = params,
    spatial_coordinates = spatial_coordinates,   # matrice ns×2
    SpectralDensity     = MaternSpectralDensity3D,
    parallelize         = F,
    batch_size          = 100,
    n_cores             = 4
  )
  
  dimnames(Z1)[[3]] <- c("v1","v2","v3")
  
  r_t = r_t_true # r¹ii et r²ii
  rho  = Sigma_fun(1,1)
  Z_T1 = TemporalSim(nt, r_t, rho, a=rep(1,p), nu=rep(1,p), A=rep(0,p))
  Z_T2 = TemporalSim(nt, r_t, rho, a=rs, nu=nu, A=A_true)
  
  # 1. Construction de Z (boucle sur i ET k)
  Z = Z1
  for (i in seq_len(ns)) {
    for (k in seq_len(p)) {
      Z[,i,k] = 0.707*(Z_T2[,k]*Z1[,i,k] + Z_T1[,k])
    }
  }
  
  # 2. Standardisation (boucle sur k seulement, APRÈS construction) A checker.
  for (k in seq_len(p)) {
    Zk <- Z[,,k]
    Z[,,k] <- (Zk - mean(Zk)) / sd(Zk)
  }
  
  return(Z)
}

# ─────────────────────────────────────────────────────────────────────────────
# FONCTION QUI PERMET DE METTRE EN FORME LA COVARIANCE THEORIQUE POUR LA FONCTION DE SAID 
build_vgm_for_plot <- function(parm_mean, coordinates, lagstime = 0:tmax, nb_points_distance = 10) {
  dst       <- round(spatial_distance_matrix(coordinates), 3)
  dist_grid <- sort(unique(dst[dst > 0]))
  #dist_grid <- dist_grid[seq(1, length(dist_grid), length.out = nb_points_distance)]
  dist_grid <- seq(0, max(dist_grid)/2, length.out = nb_points_distance)
  
  # Une ligne de parm_mean = une paire (v1, v2)
  do.call(rbind, lapply(seq_len(nrow(parm_mean)), function(idx) {
    par_row <- parm_mean[idx, ]
    
    # Vecteur de paramètres attendu par Gneiting()
    par_vec <- c(
      par_row$a, par_row$b, par_row$c, par_row$d, par_row$e,
      par_row$Ai,   par_row$Aj,
      par_row$aii,  par_row$ajj,
      par_row$nuii, par_row$nujj,
      par_row$rho1ij,
      par_row$r2ii, par_row$r2jj,
      par_row$r1ii, par_row$r1jj
    )
    
    do.call(rbind, lapply(lagstime, function(u) {
      data.frame(
        v1      = par_row$v1,
        v2      = par_row$v2,
        lagtime = u,
        dist    = dist_grid,
        cov     = NA_real_,
        vgmsim  = Gneiting(h = dist_grid, u = u,
                           par = par_vec, rho2ij = par_row$rho2ij)
      )
    }))
  }))
}

# ─────────────────────────────────────────────────────────────────────────────
# FONCTION QUI PERMET DE CALCULER LA COVARIANCE EMPIRIQUE DE 0 A DIST MAX /2 

empirical_st_cov_pair <- function(data_v1, data_v2, wt_id, dates,
                                  coordinates, lagstime = 0:tmax,nb_points_distance = 10) {
  
  
  dst       <- round(as.matrix(dist(coordinates)), 3) #length(dst) = 298116
  dist_grid <- sort(unique(dst[dst >= 0])) #length(dist_grid) = 278
  #dist_cov <- seq(from=0, to=max(dist_grid), length.out=nb_points_distance)  
  #length(dist_grid) = nb_points_distance = 10 ici
  dist_cov <- seq(0, max(dist_grid)/2, length.out = nb_points_distance)
  
  #tol <- max(dist_grid) / (2 * (nb_points_distance - 1)) #max-min/2*nb_points_distance : demi largeur en qqsorte entre deux points h.
  tol <- (max(dist_grid)/2) / (2 * (nb_points_distance - 1))   # = 0.3
  Ti <- generate_temporal_index_pairs(wt_id, dates, tmax = max(lagstime))
  
  do.call(rbind, lapply(lagstime, function(u) {
    Ti_u <- Ti[Ti[, "u"] == u, , drop = FALSE] # On selectionne les Ti qui valent time lag u donc a time fixe, ie stationnaire la
    #cov depend que de h
    
    data.frame(
      lagtime = u,
      dist    = dist_cov,
      cov     = sapply(dist_cov, function(h) {
        # Pour chaque point h de la grille de distances :
        # On cherche toutes les paires de stations (s1, s2) dont la distance
        # tombe dans l'intervalle [h - tol, h + tol]
        # abs(dst - h) = écart entre la vraie distance et le centre du bin h
        # <= tol        = on garde seulement les bonnes paires 
        sp_idx <- which(abs(dst - h) <= tol, arr.ind = TRUE) 
        if (nrow(Ti_u) == 0 || nrow(sp_idx) == 0) return(NA_real_)
        
        g  <- expand.grid(ti = seq_len(nrow(Ti_u)), si = seq_len(nrow(sp_idx)))
        t1 <- Ti_u[g$ti, "t1"];  t2 <- Ti_u[g$ti, "t2"]
        s1 <- sp_idx[g$si, 1];   s2 <- sp_idx[g$si, 2]
        
        cov(data_v1[cbind(t1, s1)],
            data_v2[cbind(t2, s2)])
      })
    )
  }))
}

# ══════════════════════════════════════════════════════════════════════════════
# CALCULS ET GRAPHE
# ══════════════════════════════════════════════════════════════════════════════

# ── Génération des Z (structure estim_results comme dans la vignette) SANS ESTIMATIONS─────────
estim_results <- list()

for (it in 1:nb_estimation) {
  print(paste("Génération Z :", it))
  Z <- generate_Z(t_seq, spatial_coordinates)
  estim_results[[it]] <- list(
    estimation = NULL,
    Z = Z
  )
}

# ── COv EMP ───────────────────────────────────────────────────────
ep <- generate_variable_index_pairs(names)
# ── ON MOYENNE LES COVARIANCE EMP DE CHAQUE Z

emp_all_runs <- do.call(rbind, lapply(1:nb_estimation, function(k) {
  Z_k <- estim_results[[k]]$Z
  do.call(rbind, lapply(seq_len(nrow(ep)), function(i) {
    v1 <- ep[i, "v1"];  vi1 <- which(names == v1)
    v2 <- ep[i, "v2"];  vi2 <- which(names == v2)
    res <- empirical_st_cov_pair(Z_k[,,vi1], Z_k[,,vi2],
                                 wt_id, dates, coordinates,
                                 lagstime = 0:tmax,
                                 nb_points_distance = nb_points_distance)
    res$v1 <- v1;  res$v2 <- v2
    res
  }))
}))

emp_all <- aggregate(cov ~ v1 + v2 + lagtime + dist,
                     data = emp_all_runs,
                     FUN  = mean, na.rm = TRUE)

# ── COV THEO ───────────────────────────────────────────────────────
vgm_theo <- build_vgm_for_plot(
  parm_mean   = parm_true,    # data.frame : 1 ligne par paire (v1,v2)
  coordinates = coordinates,
  lagstime    = 0:tmax,
  nb_points_distance = nb_points_distance
)

vgm_true <- build_vgm_for_plot(parm_true, coordinates,
                               lagstime = 0:tmax, nb_points_distance = nb_points_distance)

# ── Merge et graphe ────────────────────────────────────────────────────────────
vgm <- merge(
  vgm_theo[, c("v1","v2","lagtime","dist","vgmsim")],
  emp_all  [, c("v1","v2","lagtime","dist","cov")],
  by = c("v1","v2","lagtime","dist"),
  all = TRUE
)

vgm <- merge(
  vgm,
  vgm_true[, c("v1","v2","lagtime","dist","vgmsim")],
  by = c("v1","v2","lagtime","dist"),
  all = TRUE,
  suffixes = c("", "_true")
)
# ── PLOT FINALE ───────────────────────────────────────────────────────

plot_multivariate_st_covariances(vgm, names = names)


# ══════════════════════════════════════════════════════════════════════════════
# BANDE DE FLUCTUATION (style Bollinger)
# ══════════════════════════════════════════════════════════════════════════════

nb_macro      <- 10   # nombre de macro-simulations (courbes)
nb_z_per_macro <- 10  # nombre de generate_Z par macro-simulation

macro_results <- list()

for (macro_it in seq_len(nb_macro)) {
  print(paste("Macro-simulation:", macro_it))
  
  emp_runs <- do.call(rbind, lapply(seq_len(nb_z_per_macro), function(it) {
    Z <- generate_Z(t_seq, spatial_coordinates)
    do.call(rbind, lapply(seq_len(nrow(ep)), function(i) {
      v1_ <- ep[i, "v1"];  vi1 <- which(names == v1_)
      v2_ <- ep[i, "v2"];  vi2 <- which(names == v2_)
      res <- empirical_st_cov_pair(Z[,,vi1], Z[,,vi2],
                                   wt_id, dates, coordinates,
                                   lagstime = 0:tmax,
                                   nb_points_distance = nb_points_distance)
      res$v1 <- v1_;  res$v2 <- v2_
      res
    }))
  }))
  
  emp_mean <- aggregate(cov ~ v1 + v2 + lagtime + dist,
                        data = emp_runs, FUN = mean, na.rm = TRUE)
  emp_mean$macro_it <- macro_it
  macro_results[[macro_it]] <- emp_mean
}

all_macro <- do.call(rbind, macro_results)

# Bande : min/max (ou quantiles) des 10 courbes moyennes
band <- do.call(data.frame, aggregate(cov ~ v1 + v2 + lagtime + dist,
                                      data = all_macro,
                                      FUN = function(x) c(lo = min(x), mid = mean(x), hi = max(x))))
names(band)[5:7] <- c("lo", "mid", "hi")

# Fusion avec théorique
band_vgm <- merge(band,
                  vgm_true[, c("v1","v2","lagtime","dist","vgmsim")],
                  by = c("v1","v2","lagtime","dist"))

# ── Fonction de plot avec bande ───────────────────────────────────────────────
plot_band_covariances <- function(bvgm, names) {
  empty_plot <- ggplot2::ggplot() + ggplot2::theme_void()
  plot_matrix <- matrix(list(empty_plot), nrow = length(names), ncol = length(names))
  
  for (i in seq_along(names)) {
    for (j in seq_len(i)) {
      v1_ <- names[i]; v2_ <- names[j]
      df  <- subset(bvgm, (v1 == v1_ & v2 == v2_) | (v1 == v2_ & v2 == v1_))
      
      p <- ggplot2::ggplot(df, ggplot2::aes(x = dist, color = factor(lagtime),
                                            fill  = factor(lagtime))) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), alpha = 0.25, color = NA) +
        ggplot2::geom_line(ggplot2::aes(y = mid), linewidth = 0.7) +
        ggplot2::geom_line(ggplot2::aes(y = vgmsim / 2), linetype = "dashed",
                           linewidth = 0.6, color = "black") +
        ggplot2::labs(title = paste(v1_, "vs", v2_), x = "", y = "") +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 8),
                       legend.position = "none")
      
      last_row <- length(names); mid_col <- round(length(names) / 2)
      if (i == last_row && j == mid_col)
        p <- p + ggplot2::labs(x = "Distance (degree)") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(), axis.ticks.x = ggplot2::element_line())
      if (i == mid_col && j == 1)
        p <- p + ggplot2::labs(y = "Covariance") +
        ggplot2::theme(axis.text.y = ggplot2::element_text(), axis.ticks.y = ggplot2::element_line())
      
      plot_matrix[j, i] <- list(p)
    }
  }
  
  legend_plot <- ggplot2::ggplot(subset(bvgm, v1 == names[1] & v2 == names[1]),
                                 ggplot2::aes(x = dist, color = factor(lagtime))) +
    ggplot2::geom_line(ggplot2::aes(y = mid)) +
    ggplot2::scale_color_discrete("Time lag (day)") +
    ggplot2::theme_minimal()
  plot_matrix[length(names), 1] <- list(ggpubr::get_legend(legend_plot))
  
  patchwork::wrap_plots(plot_matrix)
}

plot_band_covariances(band_vgm, names = names)

