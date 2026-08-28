#' @title Matern Covariance Function
#'
#' @description
#' Calculates the Matern covariance function for a given vector of distances. This function is intended for internal use within the package to compute spatial covariances with the Matern model. It is not exported for end-user interaction.
#'
#' @details
#' This function implements the methods described in Sections 2.4 in Equations 8 and 9 of the article
#' \strong{Stochastic Environmental Research and Risk Assessment, 2025} (DOI: 10.1007/s00477-024-02897-8).
#'
#' @param h Numeric vector of distances between points.
#' @param r Scalar range parameter of the Matern function, affecting the spatial correlation decay.
#' @param v Scalar smoothness parameter of the Matern function, controlling the smoothness of the resulting field.
#'
#' @return Numeric vector representing the covariance values calculated using the Matern function for the distances in `h`.
#'
#' @keywords internal
Matern <- function(h, r, v) {
  rt <- (2^(1 - v)) / gamma(v) * ((r * abs(h))^v) * besselK(r * abs(h), nu = v)
  rt[h == 0] <- 1 # Ensures that the covariance at distance 0 is 1.
  return(rt)
}

#' @title Gneiting's Spatio-Temporal Covariance Model
#'
#' @description
#' Computes the covariance based on Gneiting's spatio-temporal model. Intended for internal package use.
#'
#' @details
#'  This function implements the methods described in Section 2.4 in Equations 8 of the article
#' *Stochastic Environmental Research and Risk Assessment, 2025* (DOI: 10.1007/s00477-024-02897-8).
#'
#' @param h Numeric vector of spatial distances.
#' @param u Numeric vector of temporal distances.
#' @param par Numeric vector of parameters for the covariance function.
#' @param rho2ij Correlation parameter between variables i and j.
#'
#' @return Numeric vector of covariance values calculated using Gneiting's model.
#'
#' @keywords internal
Gneiting <- function(h, u, par, rho2ij) {
  # Same function as Gneiting, but with new names for variables
  
  if (!is.numeric(par)) par <- as.numeric(par)
  
  # Unpack parameters from the 'par' vector for clarity.
  a       <- par[.a]
  b       <- par[.b]
  c       <- par[.c]
  d       <- par[.d]
  e       <- par[.e]
  Ai      <- par[.Ai]
  Aj      <- par[.Aj]
  
  aii     <- par[.aii]   # portée Matérn variable i
  ajj     <- par[.ajj]   # portée Matérn variable j
  nuii    <- par[.nuii]  # lissage Matérn variable i
  nujj    <- par[.nujj]  # lissage Matérn variable j
  rho1ij  <- par[.rho1ij]  # correlation temporelle pure 
  r2ii    <- par[.r2ii]  # décroissance exp. spatiotemporelle variable i
  r2jj    <- par[.r2jj]  # décroissance exp. spatiotemporelle variable j
  r1ii    <- par[.r1ii]  # décroissance exp. temporelle variable i
  r1jj    <- par[.r1jj]
  
  # Cross parameters (calculated, never stored)
  nuij <- (nuii + nujj) / 2
  aij  <- sqrt((aii^2 + ajj^2) / 2)
  r1ij <- sqrt((r1ii^2 + r1jj^2) / 2)
  r2ij <- sqrt((r2ii^2 + r2jj^2) / 2)
  
  # Temporal pseudo-variogram \eta_{ij}
  etaij <- ((a * abs(u))^(2*b) + 1)^c -
    (Ai * Aj * ((d * abs(u))^(2*e) + 1)^(-c))
  
  # Weights
  w1 <- sqrt(r1ii * r1jj) / r1ij
  w2 <- sqrt(r2ii * r2jj) / r2ij
  
  # Cross_factor Spatio Temp
  cross_factor <- ((aii^nuii * ajj^nujj) / aij^(2*nuij)) *
    (gamma(nuij) / (gamma(nuii)^(1/2) * gamma(nujj)^(1/2))) *
    sqrt((1 - Ai^2) * (1 - Aj^2))
  
  beta1ij <- rho1ij * w1
  beta2ij <- rho2ij * w2 * cross_factor
  
  # Purely temporal component
  Temp <- exp(-r1ij * abs(u))
  
  # Spatio-temporal component
  SpatioTemp <- Matern(abs(h), r = sqrt(aij^2 / etaij), v = nuij) *
    exp(-r2ij * abs(u)) / etaij
  
  
  return(beta1ij * Temp + beta2ij * SpatioTemp)
}

#' @title Construct Covariance Parameters DataFrame
#'
#' @description
#' Creates a data frame of covariance parameters for all possible pairs of variables. This function is
#' designed for internal use, facilitating the organization of parameters for spatio-temporal modeling.
#'
#' @details
#'  This function implements the methods described in Section 2.4 of the article
#' \strong{Stochastic Environmental Research and Risk Assessment, 2025} (DOI: 10.1007/s00477-024-02897-8).
#'
#' @param par Named vector of parameters.
#' @param names Character vector of variable names.
#'
#' @return A data frame where each row corresponds to a pair of variables (including self-pairs) filled with values specified in the 'par' vector.
#' and their associated spatio-temporal covariance parameters.
#'
#' @keywords internal
create_df_param <- function(par, names) {
  # Generate all possible pairs of variable names, including self-pairs, for parameter definitions
  ep <- generate_variable_index_pairs(names)
  pairs <- paste(ep[, 1], ep[, 2], sep = "-")
  # Initialize a data frame to be populated with parameter values
  J <- length(pairs)
  u <- data.frame(v1 = ep$v1, v2 = ep$v2, stringsAsFactors = FALSE)

  # For Parameters order see indices.R to keep the right order
  # Assign common temporal parameters to all pairs
  u$a <- par["a"]
  u$b <- par["b"]
  u$c <- par["c"]
  u$d <- par["d"]
  u$e <- par["e"]
  
  # Loop through each pair to populate the data frame with corresponding parameter values
  for (i in seq_len(J)) {
    # Extract and assign specific parameters for each pair based on naming convention
    u$Ai[i] <- par[paste(u$v1[i], "Ai", sep = ":")]
    u$Aj[i] <- par[paste(u$v2[i], "Ai", sep = ":")]

    u$aii[i] <- par[paste(paste(u$v1[i], u$v1[i], sep = "-"), "aii", sep = ":")]
    u$ajj[i] <- par[paste(paste(u$v2[i], u$v2[i], sep = "-"), "aii", sep = ":")]

    u$nuii[i] <- par[paste(paste(u$v1[i], u$v1[i], sep = "-"), "nuii", sep = ":")]
    u$nujj[i] <- par[paste(paste(u$v2[i], u$v2[i], sep = "-"), "nuii", sep = ":")]

    u$rho1ij[i] <- par[paste(paste(u$v1[i], u$v2[i], sep = "-"), "rho1ij", sep = ":")]

    # r2ii, r2jj — spatio-temporal exponential decay (per variable)
    u$r2ii[i] <- par[paste(u$v1[i], "r2ii", sep = ":")]
    u$r2jj[i] <- par[paste(u$v2[i], "r2ii", sep = ":")]
    
    # r1ii, r1jj — purely temporal exponential decay (per variable)
    u$r1ii[i] <- par[paste(u$v1[i], "r1ii", sep = ":")]
    u$r1jj[i] <- par[paste(u$v2[i], "r1ii", sep = ":")]
    
    u$rho2ij[i] <- par[paste(paste(u$v1[i], u$v2[i], sep = "-"), "rho2ij", sep = ":")]
  }
  return(u)
}

#' @title Compute rho2 Correlations
#'
#' @description
#' This function calculates the beta correlation coefficients between variables based on the Gneiting function, adjusted for a correction term. It is intended for internal use within package functions to adjust initial correlation values using specified parameters.
#'
#' @details
#' This function implements the methods described in Section 2.4 in Equation 8 of the article
#' \strong{Stochastic Environmental Research and Risk Assessment, 2025} (DOI: 10.1007/s00477-024-02897-8).
#'
#' @param parm A data frame or list containing parameters for the Gneiting function.
#' @param names A vector of variable names (e.g., "temperature", "wind") to calculate correlations between.
#' @param cr Matrix of initial correlation values between the variables.
#'
#' @return Symmetric matrix of adjusted correlation coefficients (beta) between the variables.
#' where each element [i, j] represents the correlation coefficient
#'  between variables i and j, adjusted based on the Gneiting function and a correction term.
#'
#' @importFrom Matrix nearPD
#' @keywords internal
compute_rho2 <- function(parm, names, cr) {
  J <- length(names)
  rho2 <- matrix(0, ncol = J, nrow = J)
  colnames(rho2) <- rownames(rho2) <- names
  
    # ite on pair variables existing in names
  for(pair_indice in which( parm[which(parm$v1 == names)]$v2 == names) ) {
    v1  <- parm[pair_indice, "v1"]
    v2  <- parm[pair_indice, "v2"]
    parameters_pair <- as.numeric(parm[pair_indice,-c(1,2)])

    # w1,ij = sqrt(r1ii * r1jj) / r1ij
    r1ij   <- sqrt((parameters_pair[.r1ii]^2 + parameters_pair[.r2jj]^2) / 2)
    w1     <- sqrt(parameters_pair[.r1ii] * parameters_pair[.r2jj]) / r1ij
    rho1ij <- parameters_pair[.rho1ij]
      
    # cc = Gneiting(0, 0, par, rho2ij=1) = rho1ij*w1 + w2
    cc    <- Gneiting(0, 0, parameters_pair, rho2ij = 1)
      
    # denom = w2,ij = cc - rho1ij*w1
    denom <- cc - rho1ij * w1
      
    rho2ij <- (cr[v1, v2] - rho1ij * w1) / denom
    
    if (v1 == v2) {
      # La diagonale bornée entre 0 et 1. 
      rho2ij <- min(max(rho2ij, 10e-6), 0.999)
    } else {
      # Hors diagonale [-1, 1] 
      rho2ij <- min(max(rho2ij, -0.999), 0.999)
    }
    
    rho2[v1, v2] <- rho2[v2, v1] <- rho2ij
  }
  
  # Vérifier DP et corriger si nécessaire
  rho2 <- Matrix::nearPD(rho2)$mat

  return(rho2)
}

#' @title update rho2ij values in a names vector
#'
#' @description
#' extract values from rho2ij matrix for each variables pairs and save it in a named vector as 'variable1-variable2:rho2ij' index.
#'
#' @param par_all a named vector containing variables pairs rho2ij values and models values.
#' @param names character vector specifying the variable names for which rho2ij is in vector rho2ij and pair have to be updates.
#' @param rho2ij a square matrix containing rho2ij values indiced by pairs number
#'
#' @return a named vector with variables pair rho2ij vlaues updates and the other model values.
#'
#' @keywords internal
update_rho2_parameters <- function(par_all, names, rho2ij) {
  # Generate all possible pairs of variable names, including self-pairs, for parameter definitions
  pairs_ind <- generate_variable_index_pairs(names)

  for( pairs_it in seq_len(nrow(pairs_ind)) ) {
    v1 <- pairs_ind[pairs_it, 1]
    v2 <- pairs_ind[pairs_it, 2]
    
    if( !is.na(par_all[paste0(v1,"-",v2,":rho2ij")])) par_all[paste0(v1,"-",v2,":rho2ij")] <- rho2ij[v1,v2]
    else par_all[paste0(v2,"-",v1,":rho2ij")] <- rho2ij[v2,v1]
  }
  
  return(par_all)
  }

#' @title Extract Correction Terms Matrix
#'
#' @description
#' Extracts a matrix of correction terms ('rho1ij') for each pair of variables based on the model parameters provided in 'parm'. Designed for internal use to facilitate calculations involving correction terms in spatial or spatio-temporal modeling.
#'
#' @details
#' This function implements the methods described in Sections 2.4 in Equation 8 of the article
#' \strong{Stochastic Environmental Research and Risk Assessment, 2025} (DOI: 10.1007/s00477-024-02897-8).
#'
#' @param parm A data frame or list containing the model parameters, including 'rho1ij' values.
#' @param names Character vector specifying the variable names for which correction terms are to be calculated.
#'
#' @return A square matrix where each element [i, j] represents the correction term ('rho1ij') between the ith and jth variables, facilitating the adjustment of correlations or covariances between them.
#'
#' @keywords internal
extract_rho1 <- function(parm, names) {
  rho1 <- sapply(names, function(v1) {
    sapply(names, function(v2) {
      rho1ij <- parm$rho1ij[parm$v1 == v1 & parm$v2 == v2 | parm$v1 == v2 & parm$v2 == v1]
      return(rho1ij)
    })
  })
  rho1 <- matrix(rho1, nrow = length(names), ncol = length(names))  # ← forcer matrice (byrow=TRUE non necessaire matrice carré et symétrique)
  rownames(rho1) <- colnames(rho1) <- names
  return(rho1)
}

#' @title Compute Log-likelihood for Variable Pair
#'
#' @description
#' Calculates the log-likelihood for a given pair of variables using the Gneiting spatio-temporal covariance model. This function is part of the internal mechanism for optimizing model parameters based on observed data.
#'
#' @details
#' This function implements the methods described in Section 3.3 of the article
#' \strong{Stochastic Environmental Research and Risk Assessment, 2025} (DOI: 10.1007/s00477-024-02897-8).
#'
#' @param par Current parameters being optimized.
# beta Matrix of beta coefficients, precomputed.
#' @param parms Indices or names of parameters in 'par' to be updated.
#' @param pair A string indicating the pair of variables (e.g., "temperature-wind") being analyzed.
#' @param par_all Complete set of parameters for the model.
#' @param data 3D array of observed data, with dimensions corresponding to times, locations, and variables.
#' @param names Vector of variable names, indicating the variables' names (e.g., "temperature" and "wind").
#' @param Vi Matrix where each line corresponds for a possible combination of variables in "names".
#' @param h Vector of spatial distances for the pair.
#' @param u Vector of temporal distances for the pair.
#' @param uh Matrix containing pairs of spatial and temporal distances, and additional information.
#' @param ep A matrix or data frame defining pairs of variables.
#' @param cr Correlation matrix, initial or base correlations between variables.
#'
#' @return The log-likelihood value for the given pair of variables based on the current model parameters.
#'
#' @importFrom VGAM pbinorm
#' @importFrom stats rnorm pnorm
#' @keywords internal
## TODO a réécrire (semble identique à loglik)
loglik_pair <- function(par, parms, pair, par_all, data, names, Vi, h, u, uh, ep, cr) {
  
  # ancienne notation
  # semble identique à loglik

  J <- length(names) # Number of variables
  pairs <- paste(ep[, 1], ep[, 2], sep = "-") # Constructing pairs from 'ep' data frame

  par_all[parms] <- par # Update specific parameters in the complete set

  par <- par_all # Use the updated parameter set for computations
  sp <- unlist(strsplit(pair, "-")) # Split the pair string to individual variables
  v <- which(Vi[, 1] == sp[1] & Vi[, 2] == sp[2]) # Find the index of the pair in 'Vi'

  # Update and compute model parameters
  parm <- param(par, names)
  beta1 <- Matrix::nearPD(extract_beta1(parm, names))$mat # Compute ax correction terms
  rho2 <- try(compute_rho2(parm, names, cr), silent = T) # Compute rho2ij coefficients

  # Attempt Cholesky decompositions for 'beta1ij_mat' and 'rho2ij', checking for positive definiteness
  ae <- try(chol(beta1), silent = TRUE)
  be <- try(chol(rho2), silent = TRUE)

  if (!is.character(be) & (!is.character(ae))) {
    # Proceed if both 'beta1ij' and 'rho2ij' matrices are valid for further computations

    # Map parameters to each variable pair in 'Vi'
    parmm <- lapply(1:nrow(Vi), function(v) {
      as.numeric(parm[(parm$v1 == Vi[v, 1] & parm$v2 == Vi[v, 2]) | (parm$v1 == Vi[v, 2] & parm$v2 == Vi[v, 1]), ][-c(1, 2)])
    })
    u <- uh[, 1]
    h <- uh[, 2]

    # Initializing log-likelihood components
    l1 <- l2 <- l3 <- l4 <- 0
    par <- parmm[[v]] # Parameters for the current pair

    # Parameter constraints check; return a large value if constraints are violated
    if (any(par[c(1:21, 23:24)] < 0) | any(par[c(2, 3, 5, 7:13)] > 1)) {
      return(abs(rnorm(1)) * 1e+20)
    } else {
      # Compute covariance for the pair
      cij <- Gneiting(h = h, u = u, par = par, rho2ij = beta[sp[1], sp[2]])
      delta <- 1 - cij^2
      # Extract observed values for the pair from 'data'
      v1 <- data[, , Vi[v, 1]]
      v1 <- v1[cbind(uh[, 3], uh[, 5])]
      v2 <- data[, , Vi[v, 2]]
      v2 <- v2[cbind(uh[, 4], uh[, 6])]

      # Detailed computation of log-likelihood components for various cases
      # Identifying cases based on the variable type (e.g., Precipitation) and the presence of zero values

      dz <- !(h == 0 & u == 0 & Vi[v, 1] == Vi[v, 2])
      cij <- cij[dz]
      delta <- delta[dz]
      v1 <- v1[dz]
      v2 <- v2[dz]
      uh <- uh[dz, ]
      id1 <- (v1 == 0) & (!v2 == 0) & (sp[1] == "Precipitation")
      id2 <- (!v1 == 0) & (v2 == 0) & (sp[2] == "Precipitation")
      id4 <- (!v1 == 0) & (!v2 == 0)
      id3 <- (v1 == 0) & (v2 == 0) & (sp[1] == "Precipitation") & (sp[2] == "Precipitation")


      # Adjustments for infinite values in setting a practical lower limit
      uh[, 8][which(uh[, 8] == -Inf)] <- -2.282295 # Adjusting for log transform lower bounds
      uh[, 7][which(uh[, 7] == -Inf)] <- -2.282295

      # Case 1: Both variables have non-zero values
      if (!length(which(id4 == TRUE)) == 0) {
        l4 <- sum((-1 / 2) * (log(delta[id4]) + (v1[id4]^2 - (2 * cij[id4] * v1[id4] * v2[id4]) + v2[id4]^2) / delta[id4]))
      }

      # Case 2: First variable is non-zero and the second is zero, and the second is Precipitation
      if (!length(which(id2 == TRUE)) == 0) {
        l2 <- sum(log(pnorm((uh[id2, 8] - cij[id2] * v1[id2]) / sqrt(delta[id2]))))
      }

      # Case 3: First variable is zero and the second is non-zero, and the first is Precipitation
      if (!length(which(id1 == TRUE)) == 0) {
        l1 <- sum(log(pnorm((uh[id1, 7] - cij[id1] * v2[id1]) / sqrt(delta[id1]))))
      }

      # Case 4: Both variables are zero, and both are Precipitation
      if (!length(which(id3 == TRUE)) == 0) {
        l3 <- try(sum(log(pbinorm(uh[id3, 7], uh[id3, 8], var1 = 1, var2 = 1, cov12 = cij[id3]))), silent = TRUE)
        if (is.character(l3)) l3 <- -abs(rnorm(1)) * 1e+20 # Handle errors in computing bivariate normal CDF
      }

      # The negative log-likelihood
      return(-(l1 + l2 + l3 + l4))
    }
  } else {
    # Return a large value if Cholesky decomposition fails, indicating non-positive definiteness
    return(abs(rnorm(1)) * 1e+20)
  }
}

#' @title Total Log-Likelihood Calculation
#'
#' @description
#' Calculates the total log-likelihood for spatial or spatio-temporal data across all variable pairs.
#' Utilizes the Gneiting spatio-temporal covariance model to integrate log-likelihood contributions from each variable pair.
#' This function is core to the optimization process within model fitting.
#'
#' @details
#' This function implements the methods described in Section 3.3 of the article
#' \strong{Stochastic Environmental Research and Risk Assessment, 2025} (DOI: 10.1007/s00477-024-02897-8).
#'
#' @param par Vector of parameter estimates currently being optimized.
#' @param parms Indices or names of parameters within 'par' that are subject to update.
#' @param par_all Comprehensive list of all model parameters, including those not currently being optimized.
#' @param data 3D array of observed data across locations, times, and variables.
#' @param names Character vector of variable names.
#' @param Vi Matrix indicating all combinations of variables for analysis.
#' @param h Spatial distances vector for variable pairs.
#' @param u Temporal distances vector for variable pairs.
#' @param uh Combined matrix of spatial and temporal distances with additional identifiers.
#' @param ep Data frame defining variable pairs for analysis.
#' @param cr Initial correlation matrix across variables.
#  rho2: Precomputed rho2 cross-correlation matrix for all pairs.
#'
#' @return Total log-likelihood value for the observed data given the current model parameters.
#'
#' @importFrom VGAM pbinorm
#' @importFrom parallel mclapply
#' @importFrom stats rnorm pnorm
#' @keywords internal
loglik <- function(par, parms, par_all, data, names, Vi, h, u, uh, ep, cr) {
  J <- length(names)
  pairs <- paste(ep[, 1], ep[, 2], sep = "-")
  
  par_all[parms] <- par
  
  parm <- create_df_param(par_all, names)
  parm <- create_df_param(update_rho1_parameters(par_all, names, extract_rho1(parm, names)), names)
  rho2 <- try(compute_rho2(parm, names, cr), silent = T)
  be   <- try(chol(rho2), silent = TRUE)
  
  if (!is.character(be)) {
    
    parmm <- lapply(1:nrow(Vi), function(v) {
      as.numeric(parm[(parm$v1 == Vi[v, 1] & parm$v2 == Vi[v, 2]) |
                        (parm$v1 == Vi[v, 2] & parm$v2 == Vi[v, 1]), ][, -c(1, 2)])
    })
    u <- uh[, 1]
    h <- uh[, 2]
    
    ncores <- getCores()
    if (.Platform$OS.type == "windows") {
      ll <- lapply(1:nrow(Vi), function(v) {
        l1 <- l2 <- l3 <- l4 <- 0
        par <- parmm[[v]]
        if (any(par[c(.a:.nujj, .r2ii:.r1jj)] < 0) | any(par[c(.Ai:.Aj)] > 1) | abs(par[.rho1ij]) > 1) {
          return(1e20)                                          # ← fini pour L-BFGS-B
        } else {
          cij <- Gneiting(h = h, u = u, par = par, rho2ij = rho2[Vi[v, 1], Vi[v, 2]])
          cij   <- pmax(pmin(cij, 0.9999999), -0.9999999)     # ← borner cij
          delta <- pmax(1 - cij^2, 1e-10)                     # ← éviter delta=0
          v1 <- data[, , Vi[v, 1]]
          v1 <- v1[cbind(uh[, 3], uh[, 5])]
          v2 <- data[, , Vi[v, 2]]
          v2 <- v2[cbind(uh[, 4], uh[, 6])]
          dz <- !(h == 0 & u == 0 & Vi[v, 1] == Vi[v, 2])
          cij   <- cij[dz]
          delta <- delta[dz]
          v1    <- v1[dz]
          v2    <- v2[dz]
          uh_dz <- uh[dz, ]                                    # ← uh_dz au lieu de uh
          
          id1 <- (v1 == 0) & (!v2 == 0) & (Vi[v, 1] == "Precipitation")
          id2 <- (!v1 == 0) & (v2 == 0) & (Vi[v, 2] == "Precipitation")
          id4 <- (!v1 == 0) & (!v2 == 0)
          id3 <- (v1 == 0) & (v2 == 0) & (Vi[v, 1] == "Precipitation") & (Vi[v, 2] == "Precipitation")
          uh_dz[, 8][which(uh_dz[, 8] == -Inf)] <- -2.282295
          uh_dz[, 7][which(uh_dz[, 7] == -Inf)] <- -2.282295
          
          if (!length(which(id1 == TRUE)) == 0) {
            l1 <- sum(log(pnorm((uh_dz[id1, 7] - cij[id1] * v2[id1]) / sqrt(delta[id1]))), na.rm = TRUE)
          }
          if (!length(which(id2 == TRUE)) == 0) {
            l2 <- sum(log(pnorm((uh_dz[id2, 8] - cij[id2] * v1[id2]) / sqrt(delta[id2]))), na.rm = TRUE)
          }
          if (!length(which(id3 == TRUE)) == 0) {
            rho_bound <- pmin(pmax(cij[id3], -0.99999999), 0.99999999)
            l3 <- sum(log(pbinorm(uh_dz[id3, 7], uh_dz[id3, 8], var1 = 1, var2 = 1, cov12 = rho_bound)), na.rm = TRUE)
          }
          if (!length(which(id4 == TRUE)) == 0) {
            l4 <- sum((-1 / 2) * (log(delta[id4]) + (v1[id4]^2 - (2 * cij[id4] * v1[id4] * v2[id4]) + v2[id4]^2) / delta[id4]), na.rm = TRUE)
          }
          
          result_pair <- l1 + l2 + l3 + l4
          if (!is.finite(result_pair)) return(1e20)            # ← protection finale paire
          return(result_pair)
        }
      })
    } else {
      ll <- parallel::mclapply(1:nrow(Vi), function(v) {
        l1 <- l2 <- l3 <- l4 <- 0
        par <- parmm[[v]]
        if (any(par[c(1:11, 13:16)] < 0) | any(par[c(6:7)] > 1) | abs(par[12]) > 1) {
          return(1e20)                                          # ← fini pour L-BFGS-B
        } else {
          cij <- Gneiting(h = h, u = u, par = par, rho2ij = rho2[Vi[v, 1], Vi[v, 2]])
          cij   <- pmax(pmin(cij, 0.9999999), -0.9999999)     # ← borner cij
          delta <- pmax(1 - cij^2, 1e-10)                     # ← éviter delta=0
          v1 <- data[, , Vi[v, 1]]
          v1 <- v1[cbind(uh[, 3], uh[, 5])]
          v2 <- data[, , Vi[v, 2]]
          v2 <- v2[cbind(uh[, 4], uh[, 6])]
          dz <- !(h == 0 & u == 0 & Vi[v, 1] == Vi[v, 2])
          cij   <- cij[dz]
          delta <- delta[dz]
          v1    <- v1[dz]
          v2    <- v2[dz]
          uh_dz <- uh[dz, ]                                    # ← uh_dz au lieu de uh
          
          id1 <- (v1 == 0) & (!v2 == 0) & (Vi[v, 1] == "Precipitation")
          id2 <- (!v1 == 0) & (v2 == 0) & (Vi[v, 2] == "Precipitation")
          id4 <- (!v1 == 0) & (!v2 == 0)
          id3 <- (v1 == 0) & (v2 == 0) & (Vi[v, 1] == "Precipitation") & (Vi[v, 2] == "Precipitation")
          uh_dz[, 8][which(uh_dz[, 8] == -Inf)] <- -2.282295
          uh_dz[, 7][which(uh_dz[, 7] == -Inf)] <- -2.282295
          
          if (!length(which(id1 == TRUE)) == 0) {
            l1 <- sum(log(pnorm((uh_dz[id1, 7] - cij[id1] * v2[id1]) / sqrt(delta[id1]))), na.rm = TRUE)
          }
          if (!length(which(id2 == TRUE)) == 0) {
            l2 <- sum(log(pnorm((uh_dz[id2, 8] - cij[id2] * v1[id2]) / sqrt(delta[id2]))), na.rm = TRUE)
          }
          if (!length(which(id3 == TRUE)) == 0) {
            rho_bound <- pmin(pmax(cij[id3], -0.99999999), 0.99999999)
            l3 <- sum(log(pbinorm(uh_dz[id3, 7], uh_dz[id3, 8], var1 = 1, var2 = 1, cov12 = rho_bound)), na.rm = TRUE)
          }
          if (!length(which(id4 == TRUE)) == 0) {
            l4 <- sum((-1 / 2) * (log(delta[id4]) + (v1[id4]^2 - (2 * cij[id4] * v1[id4] * v2[id4]) + v2[id4]^2) / delta[id4]), na.rm = TRUE)
          }
          
          result_pair <- l1 + l2 + l3 + l4
          if (!is.finite(result_pair)) return(1e20)            # ← protection finale paire
          return(result_pair)
        }
      }, mc.cores = ncores, mc.set.seed = FALSE)
    }
    
    result <- -sum(unlist(ll))
    if (!is.finite(result)) return(1e20)                       # ← protection finale globale
    return(result)
    
  } else {
    return(1e20)                                               # ← fini pour L-BFGS-B
  }
}

#' @title Log-Likelihood for Spatial Data
#'
#' @description
#' Calculates the log-likelihood for spatial data based on the Matérn covariance function. This function plays a pivotal role in estimating spatial parameters for geostatistical models in spatial models.
#'
#' @details
#' This function implements the methods described in Sections 2.4 and 3.3 of the article
#' \strong{Stochastic Environmental Research and Risk Assessment, 2025} (DOI: 10.1007/s00477-024-02897-8).
#'
#' @param par Vector containing parameters for the Matérn covariance function: range (`par[1]`) and smoothness (`par[2]`). Both parameters must be positive.
#' @param data data 3D array of observed spatial data.
#' @param h Vector of spatial distances between observations, used in the covariance function.
#' @param uh Matrix specifying indices for pairing spatial observations for which the log-likelihood is calculated.
#' @param v Index of the variable within `data` for which the log-likelihood is computed.
#'
#' @return Log-likelihood value for the spatial data under the Matérn covariance model.
#'
#' @importFrom VGAM pbinorm
#' @importFrom stats rnorm pnorm
#' @keywords internal
loglik_spatial <- function(par, data, h, uh, v) {
  # Penalize negative parameters to enforce model constraints.
  if (par[1] < 0 | par[2] < 0) {
    return(abs(rnorm(1)) * 1e+20)
  } else {
    # Initialize components of the log-likelihood calculation.
    l1 <- l2 <- l3 <- l4 <- 0

    # Compute covariances using the Matérn function based on spatial distances 'h'.
    cij <- Matern(h, r = par[1], v = par[2])
    delta <- 1 - cij^2

    # Extract paired observations for variable 'v' based on spatial-temporal indices in 'uh'.
    v1 <- data[, , v]
    v1 <- v1[cbind(uh[, 3], uh[, 5])]
    v2 <- data[, , v]
    v2 <- v2[cbind(uh[, 4], uh[, 6])]

    # Exclude stationary points to focus on spatial variation.
    dz <- !(h == 0)
    cij <- cij[dz]
    delta <- delta[dz]
    v1 <- v1[dz]
    v2 <- v2[dz]

    # Identify scenarios based on zero and non-zero observations and compute respective components.
    id1 <- (v1 == 0) & (!v2 == 0)
    id2 <- (!v1 == 0) & (v2 == 0)
    id4 <- (!v1 == 0) & (!v2 == 0)
    id3 <- (v1 == 0) & (v2 == 0)

    # Aggregate log-likelihood components considering the identified scenarios.
    if (!length(which(id4 == T)) == 0) {
      l4 <- sum((-1 / 2) * (log(delta[id4]) + (v1[id4]^2 - (2 * cij[id4] * v1[id4] * v2[id4]) + v2[id4]^2) / delta[id4]))
    } else if (!length(which(id2 == T)) == 0) {
      l2 <- sum(log(pnorm((-cij[id2] * v1[id2]) / sqrt(delta[id2]))))
    } else if (!length(which(id1 == T)) == 0) {
      l1 <- sum(log(pnorm((-cij[id1] * v2[id1]) / sqrt(delta[id1]))))
    } else if (!length(which(id3 == T)) == 0) {
      l3 <- sum(pbinorm(uh[id3, 7], uh[id3, 8], var1 = 1, var2 = 1, cov12 = cij[id3]))
    }

    # Return the aggregated negative log-likelihood, adjusting for errors or infinite values.
    ll <- try(-(l1 + l2 + l3 + l4), silent = TRUE)
    if (is.character(ll) || is.infinite(ll)) ll <- abs(rnorm(1)) * 1e+20
    return(ll)
  }
}

#' @title Compute Spatio-Temporal Covariances
#'
#' @description
#' Calculates spatial and temporal covariances for given spatio-temporal data, facilitating the understanding of spatial and temporal variability in the context of different weather types.
#'
#' @details
#' This function implements the methods described in Section 2.4 of the article
#' \strong{Stochastic Environmental Research and Risk Assessment, 2025} (DOI: 10.1007/s00477-024-02897-8).
#'
#' @param data 3D array representing time, location, and variable dimensions of the spatio-temporal data.
#' @param wt_id Indices of weather types for which covariances are computed.
#' @param locations Matrix of spatial locations for the data points.
#' @param ds Precomputed distance matrix or NULL to compute distances from 'locations'.
#' @param dates Vector of dates corresponding to the time dimension of the data.
#' @param lagstime Vector of time lags for covariance computation.
#' @param dist Vector of spatial distances for covariance computation.
#' @param covgm Logical flag to compute cross-covariances (default TRUE).
#'
#' @return Data frame containing computed covariances for specified spatial distances and time lags, facilitating the analysis of spatial and temporal patterns in the data.
#'
#' @importFrom stats cov
#' @keywords internal
spacetime_cov <- function(data, wt_id, locations, ds = NULL, dates, lagstime, dist, covgm = TRUE) {
  # Validate input data structure
  if (covgm && length(dim(data)) < 3) {
    stop("data must be a 3D array when 'covgm' flag is set.")
  }

  # Compute distance matrix if not provided
  if (is.null(ds)) {
    ds <- round(as.matrix(dist(locations)), 3)
  }

  # Compute covariance for zero distance to establish a baseline
  id <- cbind(wt_id, wt_id)
  idx <- which(ds == 0, arr.ind = TRUE)
  e <- expand.grid(1:nrow(id), 1:nrow(idx))
  ide <- id[e[, 1], ]
  idxe <- idx[e[, 2], ]

  if (covgm) {
    x1 <- data[, , 2]
    x2 <- data[, , 1]
  } else {
    x1 <- c(data[cbind(ide[, 1], idxe[, 1])])
    x2 <- c(data[cbind(ide[, 2], idxe[, 2])])
  }

  # Baseline covariances for normalization
  c1 <- cov(x1[cbind(ide[, 1], idxe[, 1])], x1[cbind(ide[, 2], idxe[, 2])])
  c2 <- cov(x2[cbind(ide[, 1], idxe[, 1])], x2[cbind(ide[, 2], idxe[, 2])])

  # Loop over lag times to compute covariances at different spatial distances
  vgm <- lapply(lagstime, function(u) {
    id <- cbind(wt_id - u, wt_id)
    diff <- dates[wt_id] - dates[wt_id - u]
    id <- id[diff == u, ]

    cv <- sapply(1:length(dist), function(i) {
      d <- dist[i]
      idx <- which(ds > d - 1 & ds < d + 1, arr.ind = TRUE)
      e <- expand.grid(1:nrow(id), 1:nrow(idx))
      ide <- id[e[, 1], ]
      idxe <- idx[e[, 2], ]

      if (covgm) {
        x1 <- data[, , 2]
        x2 <- data[, , 1]
      } else {
        x1 <- c(data[cbind(ide[, 1], idxe[, 1])])
        x2 <- c(data[cbind(ide[, 2], idxe[, 2])])
      }

      # Compute normalized covariance
      return(cov(x1[cbind(ide[, 1], idxe[, 1])], x2[cbind(ide[, 2], idxe[, 2])]) / sqrt(c1 * c2))
    })

    return(data.frame(lagtime = u, dist = dist, cov = cv))
  })

  # Combine and return results
  return(do.call(rbind, vgm))
}

#' @title Generate Covariance Matrices for Spatio-Temporal Model
#'
#' @description
#' Creates covariance matrices for each time lag and pair of variables using Gneiting's function, based on provided model parameters and spatial locations. These matrices are essential for multivariate space-time modeling, reflecting the covariance structure across space and time.
#'
#' @param par Parameters for the Gneiting covariance function, including details for variable pairs.
#' @param coordinates Matrix or data frame containing spatial coordinates for each location.
#' @param names Vector of variable names involved in the covariance calculations.
#' @param M Maximum time lag considered in the model.
#'
#' @return A list of covariance matrices for each time lag up to M, and for each pair of variables, where each matrix represents the spatial covariance structure for a given time lag and variable pair.
#'
#' @keywords internal
cov_matrices <- function(par, coordinates, names, M) {
  Nt <- M + 1 # Number of time points considered
  Ns <- nrow(coordinates) # Number of spatial locations
  Nv <- length(names) # Number of variables

  # Generate all combinations of time points and spatial locations
  d <- expand.grid(t1 = 1:Nt, t2 = 1:Nt, s1 = 1:Ns, s2 = 1:Ns)

  # Calculate time lags (u) and spatial distances (h) between all pairs of points
  u <- d$t1 - d$t2
  h <- ds(d$s1, d$s2, coordinates) # Calculate distances based on coordinates

  # Initialize a list to store covariance matrices
  cp <- lapply(1:Nt, function(t1) {
    cp_v1 <- lapply(names, function(v1) {
      cp_v2 <- lapply(names, function(v2) {
        # Retrieve parameters for the current pair of variables and calculate covariance
        cov_params <- par[(par$v1 == v1 & par$v2 == v2) | (par$v2 == v1 & par$v1 == v2), -c(1, 2)]
        rho2ij <- par$rho2ij[(par$v1 == v1 & par$v2 == v2) | (par$v2 == v1 & par$v1 == v2)]
        cov <- Gneiting(h, u, cov_params, rho2ij)

        # Filter to the current time point and reshape the covariance values into a matrix
        up <- (d$t1 == t1) & (d$t2 == 1)
        dd <- d[up, ]
        co <- cov[up]
        cv <- matrix(0, ncol = Ns, nrow = Ns)
        for (i in 1:nrow(dd)) {
          cv[dd$s1[i], dd$s2[i]] <- co[i]
        }
        return(cv)
      })
      return(do.call(rbind, cp_v2))
    })
    return(do.call(cbind, cp_v1))
  })
  return(cp)
}
