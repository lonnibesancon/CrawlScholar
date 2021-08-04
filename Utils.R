###' Returns the ASCII string of a specific string
###' 
###' Used to find a scholar's position in an author list
###'
###' @param string the string to convert
###' @param publication_list the list of publication from a scholar (optional)
###'
###' @return converted: the converted string
get_ascii_string <- function(string){
  converted <- iconv(string, to='ASCII//TRANSLIT', sub='')
  return (converted)
}


###' Composes a google scholar URL based on an ID
###' 
###'
###' @param scholar_id a valid google scholar ID of a scholar profile
###' @param start_index the index from which to start showing publications after they have been ordered
###' @nb_publications the number of publications to display on the page
###' @return the composed URL
###' @author Lonni Besançon
compose_scholar_url <- function(scholar_id, start_index = 0, nb_publications= 100){
  site <- "http://scholar.google.com/citations?user="
  arguments <- paste("&cstart=",start_index,"&pagesize=",nb_publications, sep="")
  return (paste(site,scholar_id,arguments,sep=""))
}

###' Composes a google scholar URL for a publication based on an ID
###' 
###'
###' @param scholar_id a valid google scholar ID of a scholar profile
###' @param publication_id a valid google scholar publication ID
###'
###' @return the composed URL
###' @author Lonni Besançon
compose_publication_url <- function(scholar_id, publication_id){
  pub_page <- "https://scholar.google.com/citations?view_op=view_citation&hl=en"
  citation_for_view_url <- paste("citation_for_view=",scholar_id,":",publication_id, sep="")
  scholar_id_url <- paste("user=",scholar_id, sep="")
  return(paste(pub_page,scholar_id_url,citation_for_view_url, sep="&"))
  
}

###' Returns the index of the best match if found
###' 
###' The function considers that a best match is found if the distance between the two strings is lower than a specific distance
###' All parameters are converted to lower_case if the paramater "case_sensitive" is set to FALSE
###'
###' @param string
###' @param list
###' @param max_distance the maximum distance between the input string and the list.
###' @param case_sensitive should the search be case sensitive. Default is FALSE
###' @param flush_cashe if the cash should be flushed
###'
###' @return the response from GET
###' 
###' @import R.cache
###' @author Lonni Besançon
get_index_best_matching_string <- function(string,list,max_distance,case_sensitive=FALSE, flush_cashe=FALSE){
  
  # Define the cache path
  cache.dir <- file.path(tempdir(), "r-scholar")
  setCacheRootPath(cache.dir)
  
  # Clear the cache if requested
  if (flush_cache){
    saveCache(NULL, list(string,max_distance))
  } 
  
  # Check if already cached
  list_of_distances <- loadCache(list(string,max_distance))
  
  # If not, get the data and save it to cache
  if (is.null(list_of_distances)) {
    list_of_distances <- stringdist(tolower(string),tolower(list))
    saveCache(list_of_distances, key=list(string, max_distance))
  }
  
  
  index_min <- -1
  min_value <- Inf
  
  for(i in 1:length(list_of_distances)){
    if(list_of_distances[i] < min_value){
      min_value <- list_of_distances[i]
      index_min <- i
    }
  }
  if(min_value < max_distance){
    return (index_min)
  }
  else{
    return (-1)
  }
}




###' Returns the response from a GET request on google scholar
###' 
###' Uses a given url to return the GET response from google scholar. 
###' URL must be non null and valid
###' Uses a sleep function so that we do not overwhelm google scholar with GET requests
###'
###' @param url a valid google scholar URL of a scholar profile
###'
###' @return the response from GET
###' @importFrom stringdist stringdist
###' @author Lonni Besançon
get_scholar_page <- function(url){
  sleep_time <- x1 <- runif(1, 1.1, 1.6)
  Sys.sleep(sleep_time)
  resp <- httr::GET(url)
  if (httr::status_code(resp) == 200) {
    return(resp)
  }
  else{
    error_message <- paste("The url you provided is incorrect. Here it is for your reference:",url,sep="\n")
    error(error_message)
  }
}

###' Returns a dataframe of the list of journal and their impact factor
###' 
###' Uses a given url to return the GET response from google scholar. 
###' URL must be non null and valid
###' Uses a sleep function so that we do not overwhelm google scholar with GET requests
###' 
###' @param url the URL where to find the impact factors. 
###' 
###' @notes uses the following list of impact factors by default: https://impactfactorforjournal.com/jcr-2021/
###'
###' @return a dataframe of the journal names and impact factors
###' @importFrom httr POST
###' @author Lonni Besançon
get_list_of_impact_factors <- function (url = "https://impactfactorforjournal.com/jcr-2021/"){
  
  resp <- httr::POST(url)
  page_html <- read_html(resp)
  tables <- html_table(page_html)
  length(tables)
  list_impact_factors <- c()
  for(i in 1:length(tables)){
    list_impact_factors <- rbind(list_of_journals, as.data.frame(tables[i]))
  }
  #We don't want the rank
  list_impact_factors <- subset(list_impact_factors,select =-1)
  #We want to remove the old headers from the reading of the HTML page
  list_impact_factors <- list_impact_factors[-1,]
  return (list_impact_factors)
}
