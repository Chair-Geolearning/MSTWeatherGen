# MSTWeatherGen <img src="man/figures/MSTWeatherGen.png" align="right" alt="" width="120" />

<!-- badges: start -->
[![Check](https://github.com/Chair-Geolearning/MSTWeatherGen/actions/workflows/r-package-check.yml/badge.svg)](https://github.com/Chair-Geolearning/MSTWeatherGen/actions/workflows/r-package-check.yml)  
[![GitHub Release](https://img.shields.io/github/v/release/Chair-Geolearning/MSTWeatherGen)]( https://github.com/Chair-Geolearning/MSTWeatherGen/releases)
<!-- badges: end -->

## Overview

{MSTWeatherGen} Multivariate and Space-Time stochastic Weather Generator R package is designed for the simulation of multivariate spatio-temporal meteorological variables. It provides tools for estimating the model parameters and generating synthetic weather data that can be used for a variety of applications, including climate research, agricultural or hydrological modeling.  

For more details on the methodology, see the publication: 
<blockquote>
  <p>
    Obakrim, S. <em>et&nbsp;al.</em> “A multivariate and space-time stochastic weather generator using a latent Gaussian framework,”
    <em> Stochastic Environmental Research and Risk Assessment</em>, 2025.
    <a href="https://doi.org/10.1007/s00477-024-02897-8">doi:10.1007/s00477-024-02897-8</a>
  </p>
</blockquote>


## Installation

You can install `{MSTWeatherGen}` from GitHub as follows:
```r
# install.packages("remotes")
remotes::install_github("Chair-Geolearning/MSTWeatherGen")
```

> The __main__ branch (default branch) is stable.

Download a special release : [![GitHub Release](https://img.shields.io/github/v/release/Chair-Geolearning/MSTWeatherGen)]( https://github.com/Chair-Geolearning/MSTWeatherGen/releases)


### Development

Active all unit tests set "R_TEST_ALL" global variable to TRUE.

Disable vignette building (that can take to much time) use : ```--no-build-vignettes``` at build and ```--ignore-vignettes``` at check.

## Getting Started

To learn how to use the `{MSTWeatherGen}` package, please refer to the detailed vignette available [here](https://sobakrim.github.io/MSTWeatherGen/articles/MSTWeatherGen.html).

## Funding
This work was made possible thanks to the Geolearning Chair (Lien: ) funded by Andra, BNP-Paribas, CCR and the SCOR Foundation for Science. A preliminary version was supported by funding from the French National Research Agency (ANR) as part of the BEYOND project (Contract No. 20-PCPA-0002).

<img alt="Chair Geolearning" src="man/figures/GL-logo-vertical-baseline@4x-100.jpg"  width="120"/> <img alt="Chair Geolearning" src="man/figures/geolearning_andra_logo_W.png"  width="120"/> <img alt="Chair Geolearning" src="man/figures/geolearning_bnp_logo_W.png"  width="120"/> <img alt="Chair Geolearning" src="man/figures/geolearning_ccr_logo_W.png"  width="120"/> 
<img alt="Chair Geolearning" src="man/figures/geolearning_scor_logo_W.png"  width="120"/>

## License

The package MSTWeatherGen is under GNU GPL V3.   
See [LICENSE](LICENSE) file.  

## Authors

- Ahmed Boualam (Author)
- Said Obakrim  (Author)
- Jean-François Rey (Maintainer)

## Contributors

- Lionel Benoit 
- Denis Allard

