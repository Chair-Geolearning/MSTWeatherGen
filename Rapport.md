# Analyse des Bottlenecks à partir de Rprof et profVis

## 1. Partie Estimation

**Fonctions à surveiller :**
- `optimize_spatial_parameter`
- `optimize_temporal_parameter`
- `weather_type : data_compression`
- `estimate_lambda_transformation`

## 2. Partie Simulation

### 2.1 Matrices / Covariances
**Fonctions à surveiller :**
- `covmatrice`
- `besselK`
- `Gneiting`
- `Matern`

### 2.2 Résultats / Transformations
**Fonctions à surveiller :**
- `apply_inverse_transformations`
- `inv_orderNorm_Transf`
- `stats::approx`
- `withCallingHandlers`
- `as.vector`
- `regularize.values`


# Ressources nécessaires en mémoire stockée RAM

## Plage de dates utilisée

Le test a été réalisé sur la plage globale de dates suivante :

```r
dates = seq(as.Date("2011-01-01"), as.Date("2021-12-31"), by = "day")
```

## Paramètres du système de test

- **R** : version 4.5.2  
- **CPU** : Intel© Core™ Ultra 7 165H × 16  
- **RAM** : 30 Go  
- **OS** : Linux Mint 22.2  
- **MSTWeatherGen** : V1.0.0  


Les mesures CPU ont été réalisées avec `Rprof()` sur des appels complets des fonctions principales.  
La mémoire a été mesurée avec `mem_change()` du package **pryr**.

## Données en entrée

| Objet         | Taille (`object.size`) | Taille (`obj_size`) |
|--------------|------------------------|---------------------|
| `data`        | 10 679 496             | 10.69 MB            |
| `coordinates` | 3 016                  | 3.02 kB             |
| `dates`       | 16 344                 | 16.34 kB            |
| `seasons`     | 3 440                  | 2.50 kB             |

## Estimation et simulation

| Catégorie           | Objet | Taille (`object.size`) | Taille (`obj_size`) |
|---------------------|--------|------------------------|---------------------|
| Estimation          | `swg`  | 246 842 192            | 64.24 MB            |
| Simulation (matrices) | `bk`   | 60 500 432             | 60.49 MB            |
| Simulation (résultats) | `sim`  | 10 679 496             | 10.67 MB            |


## Tableau récapitulatif global

| Catégorie              | Objet         | Taille (`object.size`) | Taille (`obj_size`) | % du total obj_size|
|------------------------|---------------|------------------------|---------------------|--------------------|
| Données d'entrée       | `data`        | 10 679 496             | 10.69 MB            | 7.19 %             |
| Données d'entrée       | `coordinates` | 3 016                  | 3.02 kB             | ~0.00 %            |
| Données d'entrée       | `dates`       | 16 344                 | 16.34 kB            | 0.01 %             |
| Données d'entrée       | `seasons`     | 3 440                  | 2.50 kB             | ~0.00 %            |
| Estimation             | `swg`         | 246 842 192            | 64.24 MB            | 43.23 %            |
| Simulation (matrices)  | `bk`          | 60 500 432             | 60.49 MB            | 40.71 %            |
| Simulation (résultats) | `sim`         | 10 679 496             | 10.67 MB            | 7.18 %             |

On constate que ce sont principalement les matrices bk et l’estimation swg qui occupent le plus de mémoire.

# Profilage CPU : 

## Partie Estimation : 

| Fonction         | Temps self (s) | % self | Temps total (s) | % total | Commentaire                 |
| ---------------- | -------------- | ------ | --------------- | ------- | --------------------------- |
| `mcfork`         | 1289.32        | 70.89% | 1298.30         | 71.38%  | Parallelisation majeure     |
| `selectChildren` | 171.30         | 9.42%  | 172.10          | 9.46%   | Parallelisation |
| `La.svd`         | 17.68          | 0.97%  | 17.76           | 0.98%   | Calcul SVD                  |
| `[.data.frame`   | 16.40          | 0.90%  | 65.94           | 3.63%   | Sous-ensemble data frame    |
| `$<-.data.frame` | 15.08          | 0.83%  | 17.70           | 0.97%   | Modification colonne        |
| `paste`          | 14.38          | 0.79%  | 20.58           | 1.13%   | Concaténation de chaînes    |
| `FUN`            | 12.74          | 0.70%  | 1818.54         | 99.99%  | Parallelisationdu workflow |
| `param`          | 12.44          | 0.68%  | 123.64          | 6.80%   | Paramètres internes         |
| `[[.data.frame`  | 11.12          | 0.61%  | 31.72           | 1.74%   | Accès aux colonnes          |
| `structure`      | 8.76           | 0.48%  | 12.98           | 0.71%   | Gestion de structure        |

## Partie Simulation (matrices) : 

| Fonction       | Self time | % self | Commentaire                                                  |
| -------------- | --------- | ------ | ------------------------------------------------------------ |
| `$`            | 5.18 s    | 25.29% | Accès aux colonnes data.frame. Très coûteux ici.             |
| `FUN`          | 5.10 s    | 24.90% | Fonction principale du workflow. Contient la logique lourde. |
| `besselK`      | 2.58 s    | 12.60% | Calculs de Bessel K pour covariances (Matern/Gneiting).      |
| `Gneiting`     | 2.04 s    | 9.96%  | Fonction Gneiting spatio-temporelle.                         |
| `[.data.frame` | 1.54 s    | 7.52%  | Sous-ensemble de data frames.                                |


## Partie Simulation (résultats) : 

| Fonction           | Self time | % self  | Commentaire                        |
|-------------------|-----------|---------|-------------------------------------|
| `FUN`              | 13.36 s   | 17.82%  | Logique itérative principale sim_seasons|
| `mean.default`     | 9.00 s    | 12.01%  | Calculs de moyennes                |
| `lapply`           | 6.70 s    | 8.94%   | Gestion des listes sim_seasons     |
| `isTRUE`           | 3.62 s    | 4.83%   | Tests logiques fréquents           |
| `stopifnot`        | 3.02 s    | 4.03%   | Vérifications coûteuses            |
| `data.frame`       | 2.38 s    | 3.18%   | Création d’objets                  |
| `tapply`           | 2.14 s    | 2.85%   | Opérations groupées                |
| `apply_inv_trans`  | 2.10 s    | 2.80%   | Transformations inverses           |
| `stats::approx`    | 1.54 s    | 2.05%   | Interpolation linéaire             |
| `length`           | 1.52 s    | 2.03%   | Vérification de taille             |

## Comparaison : 

| Phase                     | Temps total CPU (s) | % total workflow | Commentaire principal              |
|---------------------------|------------------|----------------|------------------------------------------|
| Estimation                | 1818.54          | 96.6%          | Workflow principal + parallélisation     |
| Simulation (matrices / bk)| 10.80            | 0.57%          | Accès colonnes, BesselK, Gneiting         |
| Simulation (résultats)    | 53.44            | 2.84%          | Logique itérative sim_seasons et calculs de moyennes|

# Profiliage RAM: 

Le profiling mémoire a été réalisé avec la fonction mem_change() du package pryr. Les résultats montrent de faibles variations de mémoire 
(de l’ordre de quelques kilooctets) au cours des différentes étapes (estim, bk, sim), avec des allocations et libérations cohérentes avec 
le fonctionnement normal de R. Toutefois, ces résultats restent difficiles à interpréter finement, car mem_change() ne fournit qu’une variation 
globale de la mémoire.

| Phase                     | Variation mémoire (kB) | Type d'opération        | Commentaire principal                          |
|---------------------------|------------------------|--------------------------|-----------------------------------------------|
| Estimation (estim)        | +10.4  à +20.5         | Allocations + libération | rien de spécial ni d’anormal                  |
| Simulation (matrices / bk)| +23.9 à +43.6          | Allocations  + libération| rien de spécial ni d’anormal                  |
| Simulation (résultats)    | +23.3 / -32.9          | Allocations + libération | rien de spécial ni d’anormal                  |
