###' Returns the ASCII string of a specific string
###' 
###' Used to find a scholar's position in an author list
###'
###' @param string the string to convert
###' @param publication_list the list of publication from a scholar (optional)
###'
###' @return converted: the converted string
get_ascii_string <- function(string){
  latin=iconv(string,'utf8','latin1')
  converted <- iconv(latin,'latin1','ascii//translit')
  return (converted)
}


###' Composes a google scholar URL based on an ID
###' 
###'
###' @param id a valid google scholar ID of a scholar profile
###' @param start_index the index from which to start showing publications after they have been ordered
###' @nb_publications the number of publications to display on the page
###' @return the composed URL
###' @author Lonni Besançon
compose_scholar_url <- function(id, start_index = 0, nb_publications= 100){
  site <- "http://scholar.google.com/citations?user="
  arguments <- paste("&cstart=",start_index,"&pagesize=",nb_publications, sep="")
  print(paste(site,id,arguments,sep=""))
  return (paste(site,id,arguments,sep=""))
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


###' Returns the response from a GET request on google scholar
###' 
###' Uses a given url to return the GET response from google scholar. 
###' URL must be non null and valid
###' Uses a sleep function so that we do not overwhelm google scholar with GET requests
###'
###' @param url a valid google scholar URL of a scholar profile
###'
###' @return the response from GET
get_scholar_page <- function(url){
  sleep_time <- x1 <- runif(1, 1.1, 1.6)
  Sys.sleep(sleep_time)
  resp <- httr::GET(url)
  print(resp)
  if (httr::status_code(resp) == 200) {
    return(resp)
  }
  else{
    error_message <- paste("The url you provided is incorrect. Here it is for your reference:",url,sep="\n")
    error(error_message)
  }
  
  
}
