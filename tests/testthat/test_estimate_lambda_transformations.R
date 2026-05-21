# Libraries:
library(testthat)

# Data :
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2018-01-01"),as.Date("2021-12-31"), by="day")
names = c("Precipitation", "Wind", "Temp_max")

# Data without precipitation :
names_no_prec <- c("Wind", "Temp_max")
data_no_prec  <- data[, , 2:3, drop = FALSE]

data_univarie <- data[, , 3, drop = FALSE]   # Temp_max uniquement
names_univ    <- c("Temp_max")

data_univarie2 <- data[, , 2, drop = FALSE]   # Wind uniquement
names_univ2    <- c("Wind")

set.seed(1)
wt <- resultperm$cluster
K  <- length(unique(wt))

res_univ <- estimate_lambda_transformations(
  data        = data_univarie,
  wt          = wt,
  names       = names_univ,
  coordinates = coordinates
)

res_univ2 <- estimate_lambda_transformations(
  data        = data_univarie2,
  wt          = wt,
  names       = names_univ2,
  coordinates = coordinates
)


# Retrieving results:
set.seed(1)
wt <- resultperm$cluster
K <- length(unique(wt))

# ----- Testing -----            
res <- estimate_lambda_transformations(
  data = data,
  wt = wt,
  names = names,
  coordinates = coordinates
)

# Cas SANS précipitation (nouveaux tests)
res_no_prec <- estimate_lambda_transformations(
  data        = data_no_prec,
  wt          = wt,
  names       = names_no_prec,
  coordinates = coordinates
)

ns <- dim(data)[2]

# ── AVEC Précipitation ────────────────────────────────────────────────────────
# 0. 
test_that("estimate_lambda_transformations returns a list with expected components", {
  
  expect_type(res, "list")
  expect_equal(length(res), 2)
  expect_true(all(c("lambda_transformations", "threshold_precip") %in% names(res)))
  expect_equal(length(res$lambda_transformations), length(unique(wt)))
})

# 1.  
test_that("lambda_transformations has correct internal structure", {
  ns <- dim(data)[2]
  nv <- length(names)
  
  lapply(seq_len(K), function(k) {
    expect_equal(length(res$lambda_transformations[[k]]), nv)
    
    lapply(seq_len(nv), function(v) {
      expect_equal(length(res$lambda_transformations[[k]][[v]]), ns)
      
      lapply(seq_len(ns), function(j) {
        expect_true(
          "q" %in% names(res$lambda_transformations[[k]][[v]][[j]])
        )
      })
    })
  })
})

# 2. 
test_that("threshold_precip is numeric and positive", {
  ns <- dim(data)[2]
  
  lapply(1:K, function(k) {
    expect_equal(length(res$threshold_precip[[k]]), ns)
  })

  expect_type(res$threshold_precip, "list")
  expect_true(all(is.finite(unlist(res$threshold_precip)))) # 3.Check of the finitude if the threshold
})

# ── SANS Précipitation ────────────────────────────────────────────────────────
# 5.
test_that("sans Precipitation : même structure de sortie globale", {
  expect_type(res_no_prec, "list")
  expect_equal(length(res_no_prec), 2)
  expect_true(all(c("lambda_transformations", "threshold_precip") %in% names(res_no_prec)))
  expect_equal(length(res_no_prec$lambda_transformations), K)
})

# 6.
test_that("sans Precipitation : threshold_precip est une liste de matrices nulles (1 x ns)", {
  expect_type(res_no_prec$threshold_precip, "list")
  expect_length(res_no_prec$threshold_precip, K)
  
  lapply(seq_len(K), function(k) {
    expect_equal(
      dim(res_no_prec$threshold_precip[[k]]),
      c(1L, ns),
      info = paste("shape incorrecte pour k =", k)
    )
    expect_true(
      all(res_no_prec$threshold_precip[[k]] == 0),
      info = paste("valeurs non nulles pour k =", k)
    )
  })
})

# 7.
test_that("sans Precipitation : lambda_transformations contient exactement 2 variables", {
  nv_no_prec <- length(names_no_prec)
  
  lapply(seq_len(K), function(k) {
    expect_equal(
      length(res_no_prec$lambda_transformations[[k]]),
      nv_no_prec,
      info = paste("mauvais nombre de variables pour k =", k)
    )
    lapply(seq_len(nv_no_prec), function(v) {
      expect_equal(length(res_no_prec$lambda_transformations[[k]][[v]]), ns)
    })
  })
})

# ── Données univariées sans précipitation avec Temp_max ─────────────────────────────────────

# 1.
test_that("[univ] la sortie est une liste avec les deux composantes", {
  expect_type(res_univ, "list")
  expect_named(res_univ, c("lambda_transformations", "threshold_precip"),
               ignore.order = TRUE)
})

# 2.
test_that("[univ] lambda_transformations contient exactement 1 variable par weather type", {
  lapply(seq_len(K), function(k) {
    expect_equal(
      length(res_univ$lambda_transformations[[k]]), 1,
      info = paste("k =", k)
    )
  })
})

# 3.
test_that("[univ] chaque variable contient ns transformations", {
  lapply(seq_len(K), function(k) {
    expect_equal(
      length(res_univ$lambda_transformations[[k]][[1]]), ns,
      info = paste("k =", k)
    )
  })
})

# 4.
test_that("[univ] chaque transformation contient un champ q", {
  lapply(seq_len(K), function(k) {
    lapply(seq_len(ns), function(j) {
      expect_true(
        "q" %in% names(res_univ$lambda_transformations[[k]][[1]][[j]]),
        info = paste("k =", k, "j =", j)
      )
    })
  })
})

# 5.
test_that("[univ] threshold_precip est une liste de matrices nulles (1 x ns)", {
  expect_type(res_univ$threshold_precip, "list")
  expect_length(res_univ$threshold_precip, K)
  
  lapply(seq_len(K), function(k) {
    expect_equal(
      dim(res_univ$threshold_precip[[k]]), c(1L, ns),
      info = paste("shape incorrecte pour k =", k)
    )
    expect_true(
      all(res_univ$threshold_precip[[k]] == 0),
      info = paste("valeurs non nulles pour k =", k)
    )
  })
})

# 6.
test_that("[univ] q vaut -Inf pour une variable continue sans zéros", {
  lapply(seq_len(K), function(k) {
    lapply(seq_len(ns), function(j) {
      expect_equal(
        res_univ$lambda_transformations[[k]][[1]][[j]]$q, -Inf,
        info = paste("k =", k, "j =", j)
      )
    })
  })
})

# ── Données univariées sans précipitation avec Wind ─────────────────────────────────────

# 1.
test_that("[univ] la sortie est une liste avec les deux composantes", {
  expect_type(res_univ2, "list")
  expect_named(res_univ2, c("lambda_transformations", "threshold_precip"),
               ignore.order = TRUE)
})

# 2.
test_that("[univ] lambda_transformations contient exactement 1 variable par weather type", {
  lapply(seq_len(K), function(k) {
    expect_equal(
      length(res_univ2$lambda_transformations[[k]]), 1,
      info = paste("k =", k)
    )
  })
})

# 3.
test_that("[univ] chaque variable contient ns transformations", {
  lapply(seq_len(K), function(k) {
    expect_equal(
      length(res_univ2$lambda_transformations[[k]][[1]]), ns,
      info = paste("k =", k)
    )
  })
})

# 4.
test_that("[univ] chaque transformation contient un champ q", {
  lapply(seq_len(K), function(k) {
    lapply(seq_len(ns), function(j) {
      expect_true(
        "q" %in% names(res_univ2$lambda_transformations[[k]][[1]][[j]]),
        info = paste("k =", k, "j =", j)
      )
    })
  })
})

# 5.
test_that("[univ] threshold_precip est une liste de matrices nulles (1 x ns)", {
  expect_type(res_univ2$threshold_precip, "list")
  expect_length(res_univ2$threshold_precip, K)
  
  lapply(seq_len(K), function(k) {
    expect_equal(
      dim(res_univ2$threshold_precip[[k]]), c(1L, ns),
      info = paste("shape incorrecte pour k =", k)
    )
    expect_true(
      all(res_univ2$threshold_precip[[k]] == 0),
      info = paste("valeurs non nulles pour k =", k)
    )
  })
})

# 6.
test_that("[univ] q vaut -Inf pour une variable continue sans zéros", {
  lapply(seq_len(K), function(k) {
    lapply(seq_len(ns), function(j) {
      expect_equal(
        res_univ2$lambda_transformations[[k]][[1]][[j]]$q, -Inf,
        info = paste("k =", k, "j =", j)
      )
    })
  })
})
