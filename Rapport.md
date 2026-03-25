# Analyse des Bottlenecks à partir de Rprof et profVis : 

## 1. Partie Estimation : 

Les fonctions a surveiller : 

-optimize_spatial_parameter
-optimize_temporal_parameter
-weather_type : data_compression
-estimate_lambda_transformation


## 2. Partie Simulation : 

Les fonctions a surveiller : 

-covmatrice
-besselK
-Gneiting
-Matern

Les fonctions a surveiller : 

-apply_inverse_transformations
-inv_orderNorm_Transf
-stats::approx
-withCallingHandlers
-as.vector
-regularize.values


# Ressources nécessaires en mémoire stockée

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

| Catégorie              | Objet         | Taille (`object.size`) | Taille (`obj_size`) | % du total |
|------------------------|---------------|------------------------|---------------------|------------|
| Données d'entrée       | `data`        | 10 679 496             | 10.69 MB            | 7.19 %     |
| Données d'entrée       | `coordinates` | 3 016                  | 3.02 kB             | ~0.00 %    |
| Données d'entrée       | `dates`       | 16 344                 | 16.34 kB            | 0.01 %     |
| Données d'entrée       | `seasons`     | 3 440                  | 2.50 kB             | ~0.00 %    |
| Estimation             | `swg`         | 246 842 192            | 64.24 MB            | 43.23 %    |
| Simulation (matrices)  | `bk`          | 60 500 432             | 60.49 MB            | 40.71 %    |
| Simulation (résultats) | `sim`         | 10 679 496             | 10.67 MB            | 7.18 %     |

On constate que ce sont principalement les matrices bk et l’estimation swg qui occupent le plus de mémoire.

# Profilage en RAM : 



