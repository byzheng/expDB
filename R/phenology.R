# Functions related with phenology

#' Approximate Date for a Target Zadoks Stage
#'
#' This function estimates the date corresponding to a specified target Zadoks stage
#' by interpolating between known Zadoks stages and their associated dates.
#' Phenology Analysis Functions
#'
#' This module provides methods for analyzing phenological data, particularly focusing on the Zadoks scale.
#'
#' @details
#' The following two methods are implemented:
#'
#' 1. **Relative Date for Observed Target (Zadoks Rounded):**
#'    - If the target phenological stage is observed in the dataset (after rounding Zadoks values), 
#' the function will use the relative date corresponding to this observation. 
#' This allows for precise tracking of phenological events based on actual recorded data.
#'
#' 2. **Linear Interpolation for Target within Observed Range:**
#'    - If the target Zadoks stage is not directly observed but falls within the range of observed stages, 
#' the function estimates the date using linear interpolation between the nearest observed stages. 
#' This provides an approximate date for the target stage based on the trend of observed data.
#'
#' @note
#' Ensure that the input data includes Zadoks stage observations and corresponding dates for accurate analysis.
#' @param zadoks A numeric vector of Zadoks growth stages (should be between 51 and 69).
#' @param target A single numeric value indicating the target Zadoks stage for 
#' which to estimate the date (should be between 51 and 69).
#' @param date A vector of dates (class \code{Date}) corresponding to each Zadoks stage.
#' @param target A single numeric value indicating the target Zadoks stage for which to estimate the date.
#'
#' @return A \code{Date} object representing the interpolated date for the target Zadoks stage, 
#' or \code{NA} if the target is outside the observed range.
#'
#' @examples
#' zadoks <- c(51, 54, 55, 59)
#' date <- as.Date(c("2020-08-15", "2020-08-17", "2020-08-18", "2020-08-21"))
#' target <- 55
#' approx_zadoks(zadoks, date, target)
#' approx_zadoks(zadoks[-3], date[-3], target)
#'
#' @export
approx_zadoks <- function(zadoks, date, target) {
    stopifnot(is.numeric(zadoks))
    stopifnot(inherits(date, "Date"))
    stopifnot(is.numeric(target))
    stopifnot(length(zadoks) == length(date))
    stopifnot(length(target) == 1)
    if (any(duplicated(date))) stop("Duplicate dates found.")
    if (any(duplicated(target))) stop("Duplicate target values found.")
    if (is.unsorted(zadoks) || is.unsorted(date)) stop("zadoks and date must be in increasing order.")
    if (target < 51 || target > 69) {
        stop("Target Zadoks stage must be between 51 and 69.")
    }
    if (any(zadoks < 51 | zadoks > 69)) {
        stop("All Zadoks stages must be between 51 and 69.")
    }
    if (round(target) %in% round(zadoks)) {
        y <- date[which(zadoks == target)]
        return(y)
    }
    if (target > min(zadoks) && target < max(zadoks)) {
        y <- as.Date(stats::approx(zadoks, as.numeric(date), target)$y, origin = "1970-01-01")
        return(y)
    }
    return(NA)
}
