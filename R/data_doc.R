#' SAFRAN meteorological data for a region in southern France for precipitation, wind and maximum temperature. 
#'
#' @name data
#' @docType data
#' @author Said Obakrim 
#' @references \url{https://www.umr-cnrm.fr/spip.php?article788}
#' @keywords data
#' @format a 3-dimensional array [days,coordinates,variables] as index
#' \describe{
#'   \item{days index}{first dimension : 4018 days index from 2011/01/01 to 2021/12/31}
#'   \item{coordinates index}{second dimension : 108 coordinates index}
#'   \item{variables index}{third dimension : 3 variables index precipitation, wind and maximum temperature}
#' }
#' @examples \dontrun{
#' data("data", package="MSTWeatherGen")
#' data("coordinates", package = "MSTWeatherGen")
#' variables = c("Precipitation", "Wind", "Temp_max")
#' days = seq(as.Date("2011-01-01"),as.Date("2021-12-31"), by="day")
#' # First date, coordinates et the 3 variables
#' data[1,1,]
#' Precipitation          Wind      Temp_max 
#'          0.0           0.8           7.7 
#' }
NULL

#' Geographical coordinates of locations included in the meteorological data (a region in southern France). 
#'
#' @name coordinates
#' @docType data
#' @author Said Obakrim 
#' @references \url{https://www.umr-cnrm.fr/spip.php?article788}
#' @keywords data
#' @format a data-frame with 108 rows as coordinates points and 2 columns as c("longitude","latitude") WGS84 (epsg:4326) projection
#' @examples \dontrun{
#' data("coordinates",package="MSTWeatherGen")
#' str(coordinate)
#' 'data.frame':	108 obs. of  2 variables:
#' $ longitude: num  5.08 5.18 5.27 5.37 5.47 ...
#' $ latitude : num  44 44 44 44 44 ...
#' }
NULL