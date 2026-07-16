#' Season Indices Calculation
#'
#' Calculates the indices of dates within a specified season across multiple years.
#' This function handles leap years and seasons that span the end of one year and the beginning of the next.
#'
#' This function implements the methods described in Section 5 of the article
#' *Stochastic Environmental Research and Risk Assessment, 2025* (DOI: 10.1007/s00477-024-02897-8).
#'
#' @param dates A vector of dates to evaluate.
#' @param season A list specifying the season with 'min_day', 'max_day', 'min_month', and 'max_month'.
#' @param Year Optional. If specified, only returns indices for the given year.
#'
#' @return A vector of indices corresponding to dates within the specified season.
#'         If 'Year' is specified, only returns indices for that year.
#'
#' @keywords internal
#' @importFrom lubridate year leap_year month
season_indices <- function(dates, season, Year) {
  years <- unique(lubridate::year(dates))
  years <- c(years, max(years) + 1)
  md <- lapply(years, function(Year) {
    if (Year == years[1]) {
      is_leap <- lubridate::leap_year(Year)

      # Adjust min_day for non-leap years if necessary
      adjusted_min_day <- ifelse(!is_leap && season$min_month == 2 && season$min_day == 29, 28, season$min_day)
      adjusted_max_day <- ifelse(!is_leap && season$max_month == 2 && season$max_day == 29, 28, season$max_day)

      # Check if the season spans the end of the year
      if (season$min_month > season$max_month || (season$min_month == season$max_month && adjusted_min_day > adjusted_max_day)) {
        d1 <- as.Date(paste(Year, min(lubridate::month(dates[lubridate::year(dates) %in% years[1]])), adjusted_min_day, sep = "-"))
        d2 <- as.Date(paste(Year, season$max_month, adjusted_max_day, sep = "-"))
      } else {
        d1 <- as.Date(paste(Year, season$min_month, adjusted_min_day, sep = "-"))
        d2 <- as.Date(paste(Year, season$max_month, adjusted_max_day, sep = "-"))
      }

      return(seq(d1, d2, by = "day"))
    } else {
      if (Year == max(years)) {
        Year <- Year - 1
        is_leap <- lubridate::leap_year(Year)

        # Adjust min_day for non-leap years if necessary
        adjusted_min_day <- ifelse(!is_leap && season$min_month == 2 && season$min_day == 29, 28, season$min_day)
        adjusted_max_day <- ifelse(!is_leap && season$max_month == 2 && season$max_day == 29, 28, season$max_day)

        # Check if the season spans the end of the year
        if (season$min_month > season$max_month || (season$min_month == season$max_month && adjusted_min_day > adjusted_max_day)) {
          d1 <- as.Date(paste(Year, season$min_month, adjusted_min_day, sep = "-"))
          d2 <- max(dates)
          if (d1 > d2) {
            return(NULL)
          } else {
            return(seq(d1, d2, by = "day"))
          }
        } else {
          d1 <- as.Date(paste(Year, season$min_month, adjusted_min_day, sep = "-"))
          d2 <- as.Date(paste(Year, season$max_month, adjusted_max_day, sep = "-"))
        }
      } else {
        # Check for leap year
        is_leap <- lubridate::leap_year(Year)

        # Adjust min_day for non-leap years if necessary
        adjusted_min_day <- ifelse(!is_leap && season$min_month == 2 && season$min_day == 29, 28, season$min_day)
        d1 <- as.Date(paste(Year, season$min_month, adjusted_min_day, sep = "-"))

        # Adjust max_day for non-leap years if necessary
        if (season$min_month > season$max_month) {
          is_leap <- lubridate::leap_year(Year)
          adjusted_max_day <- ifelse(!is_leap && season$max_month == 2 && season$max_day == 29, 28, season$max_day)
          is_leap <- lubridate::leap_year(Year - 1)
          adjusted_min_day <- ifelse(!is_leap && season$min_month == 2 && season$min_day == 29, 28, season$min_day)
          d2 <- as.Date(paste(Year, season$max_month, adjusted_max_day, sep = "-"))
          d1 <- as.Date(paste(Year - 1, season$min_month, adjusted_min_day, sep = "-"))
        } else {
          is_leap <- lubridate::leap_year(Year)
          adjusted_max_day <- ifelse(!is_leap && season$max_month == 2 && season$max_day == 29, 28, season$max_day)
          d2 <- as.Date(paste(Year, season$max_month, adjusted_max_day, sep = "-"))
        }

        return(seq(d1, d2, by = "day"))
      }
    }
  })
  md <- do.call(c, md)
  md <- format(md, "%Y-%m-%d CET")
  dates <- format(dates, "%Y-%m-%d CET")
  if (missing(Year)) {
    return(which(dates %in% md))
  } else {
    return(which(dates %in% md & lubridate::year(dates) == Year))
  }
}
#' Filter Seasonal Data
#'
#' Filters weather data and corresponding dates for a specified season.
#'
#' This function implements the methods described in Section 5 of the article
#' *Stochastic Environmental Research and Risk Assessment, 2025* (DOI: 10.1007/s00477-024-02897-8).
#'
#' @param data A 3D array of weather data with dimensions [time, location, variable].
#' @param dates A vector of dates corresponding to the time dimension of the data.
#' @param season A list specifying the season with 'min_day', 'max_day', 'min_month', and 'max_month'.
#' @param names Names of the variables in the data array to be used for analysis.
#'
#' @return A list containing the filtered weather data ('data_filtered') and corresponding dates ('dates_filtered').
#'
#' @keywords internal
filter_season_data <- function(data, dates, season, names) {
  # Ensure 'dates' is converted to Date class
  dates <- as.Date(dates)

  # Obtain indices for the specified season
  # Assuming 'season_indices' is a function that returns indices of dates within the specified season
  season_indices <- season_indices(dates, season)

  # Filter data and dates based on season indices
  data_filtered <- data[season_indices, , , drop = FALSE]
  dates_filtered <- dates[season_indices]

  # Further filter data to include only the specified variables (names)
  data_filtered <- data_filtered[, , names, drop = FALSE]

  # Order the filtered data by dates
  data_filtered <- data_filtered[order(dates_filtered), , , drop = FALSE]

  # Return the filtered and ordered data along with the corresponding dates
  return(list(data_filtered = data_filtered, dates_filtered = dates_filtered))
}
#' Haversine Distance Calculation
#'
#' Computes the Haversine distance between two points given their coordinates.
#'
#' @param point1 Coordinates of the first point in decimal degrees (lon, lat).
#' @param point2 Coordinates of the second point in decimal degrees (lon, lat).
#'
#' @return The Haversine distance between the two points in kilometers.
#'
#' @keywords internal
haversine <- function(point1, point2) {
  # Define the Haversine function
  # Convert degrees to radians
  lon1 <- c(point1[, 1] * pi / 180)
  lat1 <- c(point1[, 2] * pi / 180)
  lon2 <- c(point2[, 1] * pi / 180)
  lat2 <- point2[, 2] * pi / 180

  # Differences in coordinates
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1

  # Haversine formula
  a <- as.numeric(sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2)
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))

  # Radius of Earth in kilometers
  R <- 6371

  # Distance in kilometers
  d <- R * c

  return(d)
}

#' Calculate Haversine Distance Between Two Coordinates
#'
#' Computes the Haversine distance between two coordinates using the geosphere package.
#'
#' This function implements the methods described in Section 3.3 of the article
#' *Stochastic Environmental Research and Risk Assessment, 2025* (DOI: 10.1007/s00477-024-02897-8).
#'
#' @param i Index of the first coordinate.
#' @param j Index of the second coordinate.
#' @param coordinates Matrix with columns for the longitude and latitude of each location.
#'
#' @return The Haversine distance between the two coordinates in kilometers.
#'
#' @importFrom geosphere distHaversine
#' @keywords internal
ds <- function(i, j, coordinates) {
  return(geosphere::distHaversine(coordinates[i, ], coordinates[j, ]) / 1000)
  # return(sqrt(rowSums((coordinates[i,]-coordinates[j,])^2)))
}

#' Calculate Insolation Clearness Index (ICI)
#'
#' Computes the Insolation Clearness Index (ICI) based on the provided radiation and time.
#'
#' @param radiation Measured radiation values.
#' @param time Time corresponding to the radiation measurements.
#'
#' @return The Insolation Clearness Index (ICI).
#'
#' @keywords internal
calculate_ICI <- function(radiation, time) {
  ## ICI : Insolation Clearness Index
  day_of_year <- as.integer(format(time, "%j"))
  TOA_irradiance <- 1361 + 0.033 * cos((360 * day_of_year) / 365)
  # Calculate the Insolation Clearness Index (ICI)
  ICI <- radiation / TOA_irradiance

  return(ICI)
}

#' Explore or summarize estimation output swg
#'
#' Summarize `MSTWeatherGen_Estim()`` function output (swg object class).
#'
#' @param swg A swg class, as a list containing the results of the
#'  `MSTWeatherGen_Estim` function, including estimated parameters and
#'  other outputs relevant to weather generation, such as weather type
#'  classifications and spatial dependencies.
#' Each season contains a list containing dates, scale parameters,
#'  weather types, transition probabilities, lambda transformations,
#' and parameters for the Gaussian field model for the specified season.
#' @param season vector of seasons names,
#'  default NULL show summarize seasons
#' @param all a boolean to output all values as str (default = FALSE)
#' 
#' @examples
#' \dontrun{
#'   swg <- MSTWeatherGen_estim()
#'   summary(swg)
#'   summary.swg(swg)
#' }
#' @return invisible(NULL)
#' @export
summary.swg <- function(swg, season = NULL, all = FALSE) {
  variable_name <- deparse(substitute(swg))

  if (!(is.list(swg) && is.list(swg$swg))) {
    stop("Input wrong format, list of list is needed", call. = FALSE)
  }

  cat("Estimated Variables:", paste0(swg$names, collapse = " , "), "\n")
  cat(
    "Weather types names:",
    paste0(swg$names_weather_types, collapse = " , "), "\n"
  )
  cat("Number of seasons:", length(swg$swg), "\n")

  all_seasons <- names(swg$swg)
  if (is.null(season)) {
    season <- all_seasons
  }

  for (s in season) {
    if (!(s %in% all_seasons)) {
      warning("Season '", s, "' not found, skipping.")
      next
    }
    list_s <- swg$swg[[s]]
    prefix <- paste0(variable_name, "$swg$", s)

    cat("\n---\n")
    cat(prefix, "\n")
    cat("Season", s, "\n")

    # Dates
    cat(
      paste0(prefix,"$dates\n"),
      "\tStart", as.character(as.Date(list_s$dates[1])), "\n",
      "\tEnd", as.character(as.Date(list_s$dates[length(list_s$dates)])), "\n",
      "\tNb of dates:", length(list_s$dates), "\n"
    )
    cat("Unique days : ",dim(list_s$scale_parm$mu[[1]])[1],'\n')
    cat("Nb of Coordinates : ",dim(list_s$scale_parm$mu[[1]])[2],'\n')

    # Weather Types
    wt_counts <- table(list_s$wt)
    cat(paste0(prefix,"$wt\n"),
      "Number of weather types", length(wt_counts),
      "-> values [", paste(names(wt_counts), collapse = " "), "]", "\n"
    )

    for (wt_name in names(wt_counts)) {
      cat("\t", wt_name, wt_counts[[wt_name]], "\n")
    }
    cat("\t", "T", sum(wt_counts), "\n")
    if (all) str(list_s$wt)
    # transitions
    cat("Weather type transitions Matrix : ")
    cat(paste0(prefix, "$transitions"), "\n")
    if (all) str(list_s$transitions)
    # scale_param
    cat(paste0("Central tendency : ",prefix, "$scale_parm$mu"), "\n")
    cat("\t", paste0("$", names(list_s$scale_parm$mu)), "\n")
    if (all) str(list_s$scale_parm$mu)
    cat(paste0("Standard deviation : ", prefix, "$scale_parm$sd"), "\n")
    cat("\t", paste0("$", names(list_s$scale_parm$sd)), "\n")
    if (all) str(list_s$scale_parm$sd)
    
    # lmbd
    cat(paste0(prefix, "$lmbd"), "\n")
    if (all) str(list_s$lmbd)
    # gf_par
    cat(paste0(prefix, "$gf_par[[1:", length(list_s$gf_par), "]]"), "\n")
    cat("\t", paste0("$", names(list_s$gf_par[[1]])), "\n")
    if (all) str(list_s$gf_par)
  }

  invisible(NULL)
}
