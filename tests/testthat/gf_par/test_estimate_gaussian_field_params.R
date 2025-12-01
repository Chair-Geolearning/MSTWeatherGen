# Librairies:
library(testthat)
library(MSTWeatherGen)  

# Donnees:
data("data", package = "MSTWeatherGen")
data("coordinates", package = "MSTWeatherGen")
names = c("Precipitation", "Wind", "Temp_max")
dates = seq(as.Date("2018-01-01"),as.Date("2021-12-31"), by="day")
names = c("Precipitation", "Wind", "Temp_max")


resultperm <- readRDS("resultperm2.rds")

set.seed(1)
wt <- resultperm$cluster
K <- length(unique(wt))

# --- Tests sous-fonction Estimation gf ----

# Récupération des dimensions réelles
Nt <- dim(data)[1]
Ns <- dim(data)[2]
Nv <- dim(data)[3]

test_that("estimation_gf fonctionne avec des données du package MSTWeatherGen", {
  # Données du package MSTWeatherGen
  data("data", package = "MSTWeatherGen")
  data("coordinates", package = "MSTWeatherGen")
  
  # Création de la séquence de dates
  dates <- seq(as.Date("2018-01-01"), as.Date("2021-12-31"), by = "day")
  names <- c("Precipitation", "Wind", "Temp_max")  # Noms des variables
  
  # Paramètres supplémentaires
  wt_id <- rep(1:3, length.out = length(dates))  # Simuler 3 types de météo pour chaque jour
  max_it <- 50  # Nombre d'itérations pour l'optimisation
  tmax <- 5  # Maximum temporal lag
  n1 <- 2  # Paramètre pour la granularité spatiale
  n2 <- 2  # Paramètre pour la granularité spatiale
  ax <- matrix(runif(100), ncol = 10)  # Covariance correction terms simulés
  cr <- matrix(runif(100), ncol = 10)  # Matrice de corrélation simulée
  threshold_precip <- c(0, 10, 20, 30)  # Seuils de précipitation simulés
  
  # Appel de la fonction avec les données
  result <- estimation_gf(
    data = data, 
    wt_id = wt_id, 
    max_it = max_it, 
    dates = dates, 
    tmax = tmax, 
    names = names, 
    coordinates = coordinates, 
    n1 = n1, 
    n2 = n2, 
    ax = ax, 
    cr = cr, 
    threshold_precip = threshold_precip
  )
  
  # Vérification que la fonction retourne une liste avec les éléments attendus
  expect_type(result, "list")
  expect_true("parm" %in% names(result))
  expect_true("par_all" %in% names(result))
  
  # Vérification des dimensions de la matrice de paramètres
  # Le nombre de variables dans 'names' est 3, donc la dimension attendue est 3x3
  expect_equal(dim(result$parm), c(3, 3))  # Les dimensions de 'parm' doivent correspondre aux variables
  expect_equal(length(result$par_all), 100)  # La longueur de 'par_all' doit être 100 si c'est un vecteur de taille 100
})
