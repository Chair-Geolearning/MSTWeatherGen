#' Check if a matrix is positive
#' 
#' @description
#' This function checks if a symmetric matrix is positive definite
#' by verifying that all its eigenvalues are strictly positive
#' (greater than a small tolerance).
#' 
#' @param M a square and symetric matrix 
#' @param tol tolerance (default -1e-10)
#' 
#' @return a boolean
is_positive_definite <- function(M, tol = -1e-10) {
    eigenvalues <- eigen(M, symmetric = TRUE)$values
    return(all(eigenvalues >= tol))   
}
 
#' Check make a matrix positive
#' 
#' @description
#' This function transforms a symmetric matrix into a positive definite matrix
#' by replacing any negative or excessively small eigenvalues with 
#' a specified small positive value (epsilon).
#' 
#' @param M a square and symetric matrix 
#' @param epsilon threshold (default 1e-9)
#' 
#' @return the matrix M positive
make_positive_definite <- function(M, epsilon = 1e-9) {
    # Spectral decomposition
    eigen_decomp <- eigen(M, symmetric = TRUE)
    values <- eigen_decomp$values
    vectors <- eigen_decomp$vectors
    
    # Replace negative or too small eigenvalues
    corrected_values <- pmax(values, epsilon)
    
    # Reconstruct the matrix
    M_corrected <- vectors %*% diag(corrected_values) %*% t(vectors)
    return(M_corrected)
}