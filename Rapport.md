# Analyse des Bottlenecks à partir de Rprof et profVis : 

## 1. Partie Estimation : 

Les fonctions a surveiller : 

-optimize_spatial_parameter
-optimize_temporal_parameter
-weather_type : data_compression
-estimate_lambda_transformation
-

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


# Ressources nécessaires

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

Estimation : 
> object.size(swg)
246842192 bytes

> object.size(seasons)
3440 bytes

Simulation : 
Matrices bk
> object.size(bk)
60500432 bytes

Simulation results
> object.size(sim)
10679496 bytes


