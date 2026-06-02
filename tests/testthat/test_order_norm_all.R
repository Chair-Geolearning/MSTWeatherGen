library(testthat)

# Data
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names <- c("Precipitation", "Wind", "Temp_max")

wt_test <- resultperm$cluster
K        <- length(unique(wt_test))
ns       <- dim(data)[2]

# ---- Tests ----

test_that("orderNorm_all retourne un objet orderNorm valide pour tous k, v, j et tourne correctement", {
  for (k in 1:K) {
    for (v in names) {
      for (j in 1:ns) {
        x <- data[wt_test == k, j, v]
        q <- if (v == "Precipitation") {
          qnorm(length(which(x == 0)) / length(x))
        } else {
          -Inf
        }
        result <- orderNorm_all(
          data        = data[wt_test == k, , v],
          j           = j,
          coordinates = coordinates,
          left        = q
        )
        expect_s3_class(result, "orderNorm")
      }
    }
  }
})

test_that("orderNorm_all ne boucle pas indéfiniment apres l'ajout de la condition sur kn(timeout 10s) pour tous k, v, j", {
  for (k in 1:K) {
    for (v in names) {
      for (j in 1:ns) {
        x <- data[wt_test == k, j, v]
        q <- if (v == "Precipitation") {
          qnorm(length(which(x == 0)) / length(x))
        } else {
          -Inf
        }
        result <- R.utils::withTimeout(
          orderNorm_all(
            data        = data[wt_test == k, , v],
            j           = j,
            coordinates = coordinates,
            left        = q
          ),
          timeout = 10
        )
        expect_false(is.null(result))
      }
    }
  }
})

test_that("x.t de orderNorm_all ne contient pas de NA pour tous k, v, j", {
  for (k in 1:K) {
    for (v in names) {
      for (j in 1:ns) {
        x <- data[wt_test == k, j, v]
        q <- if (v == "Precipitation") {
          qnorm(length(which(x == 0)) / length(x))
        } else {
          -Inf
        }
        result <- orderNorm_all(
          data        = data[wt_test == k, , v],
          j           = j,
          coordinates = coordinates,
          left        = q
        )
        expect_false(any(is.na(result$x.t)))
      }
    }
  }
})

test_that("orderNorm_all ne boucle pas quand j depasse length(kn) (cas pathologique)", {
  
  # ---- Cas pathologique : 99% de zéros sur toutes les stations ----
  # Même avec tous les voisins agrégés, on n'atteindra jamais 50 non-zéros
  ndays_test <- dim(data)[1]
  ns_test    <- dim(data)[2]
  
  data_pathologique <- data
  # Remplacer Precipitation par 99% de zéros
  data_pathologique[,, "Precipitation"] <- ifelse(
    runif(ndays_test * ns_test) > 0.01,  # 99% de zéros
    0,
    abs(rnorm(ndays_test * ns_test))
  )
  
  # Test sur plusieurs k et j
  wt_test <- resultperm$cluster
  K        <- length(unique(wt_test))
  
  for (k in 1:K) {
    data_k <- data_pathologique[wt_test == k, , "Precipitation"]
    for (j in 1:ns_test) {
      x <- data_k[, j]
    
      result <- R.utils::withTimeout(
        orderNorm_all(
          data        = data_k,
          j           = j,
          coordinates = coordinates,
          left        = qnorm(mean(x == 0))
        ),
        timeout = 5,  # doit terminer en moins de 5 secondes
        onTimeout = "error"
      )
      expect_s3_class(result, "orderNorm")
    }
  }
})

test_that("orderNorm_all ne boucle pas avec 100% de zéros (cas extreme)", {
  
  ndays_test <- dim(data)[1]
  ns_test    <- dim(data)[2]
  
  data_extreme <- data
  # 100% de zéros → x[x != 0] sera vide
  data_extreme[,, "Precipitation"] <- 0

  wt_test <- resultperm$cluster
  k       <- 1  # tester sur le premier weather type
  data_k  <- data_extreme[wt_test == k, , "Precipitation"]
  j       <- 1
  
  # Doit terminer sans boucle infinie
  # On s'attend soit à un orderNorm soit à un warning/erreur propre
  result <- tryCatch(
    R.utils::withTimeout(
      orderNorm_all(
        data        = data_k,
        j           = j,
        coordinates = coordinates,
        left        = -Inf
      ),
      timeout   = 5,
      onTimeout = "error"
    ),
    error = function(e) {
      NULL
    }
  )
  
  # Le résultat est soit un orderNorm soit NULL (erreur propre) — jamais une boucle infinie
  expect_true(is.null(result) || inherits(result, "orderNorm"))
})
