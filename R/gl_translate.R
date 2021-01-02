#' Translate via Google Translate 
#' 
#' Slick function to retrieve translation from the Google Translate API
#' 
#' @param str vector of strings 
#' @param target target language code
#' @param key API key
#' 
#' @returns 
#' A list of \code{tibbles}. 
#' 
#' @examples
#' 
#' library(tidyverse)
#' 
#' gl_translate(c("Bonjour", "hallo", "ola"), target='en', key=your_api_key)
#' 
#' # tidyr 
#' tibble(str=c("Bonjour", "hallo", "ola")) %>% 
#' 	mutate(translated=gl_translate(str,
#' 	 target='en', key=api_key)) %>% 
#'  unnest(col='translated')
#'  
#' @importFrom purrr map
#' @importFrom rlang dots_list
#' @import httr
#' @importFrom tibble as_tibble
#'  
#' @export
gl_translate <- function(str,target,key){
	purrr:::map(str, ~gl_translate_(q=., key=key, target=target))
	}

gl_translate_ <- function(...){
	param <- rlang::dots_list(..., .preserve_empty = FALSE)
	base <- "https://translation.googleapis.com/language/translate/v2"
	res <- httr::content(httr::POST(url=base, body=param))
	txt <- res[['data']][['translations']][[1]][['translatedText']]
	lng <- res[['data']][['translations']][[1]][['detectedSourceLanguage']]
	return(tibble::tibble(text=txt, lng=lng))
}
