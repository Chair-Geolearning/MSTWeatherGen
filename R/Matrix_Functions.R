#' Check if a Matrix is Positive Definite
#' 
#' @description
#' Checks whether a symmetric matrix is positive definite by verifying that all
#' its eigenvalues exceed a given tolerance. Positive definiteness guarantees,
#' among other properties, that the matrix is invertible and suitable for use
#' as a covariance or correlation matrix.
#' 
#' @param M a square, symmetric numeric matrix
#' @param tol tolerance (default 1e-10)
#'
#' @return a boolean
is_positive_definite <- function(M, tol = 1e-9) {
  # Retrieve eigenvalues only .
  eigenvalues <- eigen(M, symmetric = TRUE)$values
  
  # TRUE only if every eigenvalue is at or above the tolerance
  return(all(eigenvalues >= tol))
}

#' Make a Matrix Positive Definite
#'
#' @description
#' This function transforms a symmetric matrix into a positive definite matrix
#' by replacing any negative or excessively small eigenvalues with
#' a specified small positive value (epsilon).
#'
#' @param M a square, symmetric numeric matrix.
#' @param epsilon a small positive numeric threshold below which eigenvalues
#' are replaced(default -1e-9).
#' @return a symmetric positive definite corrected matrix of the 
#' same dimensions as the original 
#' @details
#' The correction relies on the eigendecomposition M = V . D . V',
#' where V is the matrix of eigenvectors and D the diagonal matrix of
#' eigenvalues. Negative or near-zero eigenvalues in D are moved to
#' epsilon before reconstructing the matrix.
#' The result is numerically close to the original matrix M when the
#' original matrix is already near positive definite.
make_positive_definite <- function(M, epsilon = 1e-9) {
  # Decompose M into eigenvalues and eigenvectors (exploiting symmetry)
  eigen_decomp <- eigen(M, symmetric = TRUE)
  values <- eigen_decomp$values  # numeric vector of eigenvalues
  vectors <- eigen_decomp$vectors # column matrix of corresponding eigenvectors

  # Replace any eigenvalue below epsilon with epsilon,
  # guaranteeing strict positive definiteness and remove the negative or too small eigenvalues
  corrected_values <- pmax(values, epsilon)

  ##Reconstruct the matrix: M_corrected = V * diag(lambda) * Vt
  M_corrected <- vectors %*% diag(corrected_values) %*% t(vectors)
  return(M_corrected)
}
