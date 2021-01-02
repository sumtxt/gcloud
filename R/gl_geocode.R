#' Geolocation via Google Translate 
#' 
#' Slick function to geolocate via the Google Maps API
#' 
#' @param str vector of strings 
#' @param region bias results towards a region
#' @param key API key
#' 
#' @returns 
#' A list of \code{tibbles} with list columns. 
#' 
#' @examples
#' 
#' library(tidyverse)
#' 
#' gl_geocode(c("Konstanz", "Mannheim", "Köln"), key=your_api_key, region='DE')
#' 
#' # tidyr 
#' tibble(str=c("Konstanz", "Mannheim", "Köln")) %>% 
#' 	mutate(geo=gl_geocode(str,
#' 	 region='DE', key=api_key)) %>% 
#'  unnest(col='geo')
#'  
#' @importFrom purrr map
#' @importFrom rlang dots_list
#' @import httr
#' @importFrom tibble as_tibble
#' @export
gl_geocode <- function(str,region,key){
	if(is.null(region)) { 
		r <- purrr:::map(str, ~gl_geocode_(address=., key=key))
	} else {
		r <- purrr:::map(str, ~gl_geocode_(address=., key=key, region=region))
	}
	return(r)
	}

gl_geocode_ <- function(...){
  param <- rlang::dots_list(..., .preserve_empty = FALSE)
  base <- "https://maps.googleapis.com/maps/api/geocode/json"
  res <- httr::content(httr::GET(url=base, query=param))
  return(tibble::as_tibble(res)) 
  }
