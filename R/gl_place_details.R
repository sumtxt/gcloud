#' Google Place ID API
#' 
#' Slick function to retrieve information from Google Place API
#' 
#' @param place_id Google Place ID
#' @param fields fields to retrieve 
#' @param key API key
#' 
#' @details 
#' Available fields are listed in the API Dev Guides \url{https://developers.google.com/places/web-service/details}.
#' 
#' @returns 
#' A list. 
#' 
#' @examples
#' 
#' gl_place_details(
#'  place_id='ChIJ3wZkgmIbkUcRYEgkDb9IjMY', 
#'  key=your_api_key, fields='formatted_address,geometry')
#' 
#' @importFrom purrr map
#' @importFrom rlang dots_list
#' @import httr
#' @importFrom tibble as_tibble
#' 
#' @export
gl_place_details <- function(place_id,key,fields){
	gl_place_details_(place_id=place_id, key=key, fields=fields)
	}

gl_place_details_ <- function(...){
  param <- rlang::dots_list(..., .preserve_empty = FALSE)
  base <- "https://maps.googleapis.com/maps/api/place/details/json"
  res <- httr::content(httr::GET(url=base, query=param))
  return(res) 
  }