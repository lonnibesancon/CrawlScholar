##' Gets accurate information about a researcher's complete list of publications
##' 
##' Crawls through the list of publications of a scholar to get more accurate informations
##' The function uses an internal timer to avoid being banned from crawling google scholar
##'
##' @param author_id the id from the scholar, must be non null
##' @param publication_list the list of publication from a scholar (optional)
##'
##' @return publication_list the updated publication list with more accurate information
clean_publication_list <- function(publication_list, author_id){
  if(missing(author_id)) {
    error("Parameter 'author_id' should be set")
  }
  if(missing(publication_list)){
    warning("'publication_list' was not set, the function automatically fetched the publications from the scholar based on 'author_id'")
    publication_list <-get_publications(author_id)
  }
  
  for (i in 1:nrow(publication_list)){
    current_publication <- publication_list[i,]
    publication_list[i,] <- clean_publication_data(current_publication,author_id)
    Sys.sleep(sleep_time)
  }
  
  return (publication_list)
  
}


##' Gets accurate information about a specific publication
##'
##' Gets accurate information from the google scholar page of a publication
##' Completes and correct partial information gathered from a scholar's personnal page
##' Each page gives the following information 
##' title, link to PDF <-- we already have these so we ignore them
##' author list, publication date, venue, pages <-- we need to add/correct with these
##'
##' This function can be called on a specific publication only
##' Or can be used to clean a whole list of publications by the function 'clean_all_publications'
##'
##' @param publication a publication as obtained from a scholar's publication list, must be non null
##' @param author_id the id of the scholar, must be non null
##'
##' @return the updated publication with more accurate information
clean_publication_data <- function(publication, author_id){
  
  pub_page <- "https://scholar.google.com/citations?view_op=view_citation&hl=en"
  citation_for_view_url <- paste("citation_for_view=",author_id,":",publication$pubid, sep="")
  author_id_url <- paste("user=",author_id, sep="")
  pub_page <- paste(pub_page,author_id_url,citation_for_view_url, sep="&")
  if (is.null(pub_page)){
    errorMessage <- paste("The scholar page for this publication is empty\n The function tried to fetch the following page:\n'",pub_page,"'",sep="")
    stop(errorMessage)
  } 
  print(pub_page)
  print("https://scholar.google.com/citations?view_op=view_citation&hl=en&user=ulkW7fgAAAAJ&citation_for_view=ulkW7fgAAAAJ:u-x6o8ySG0sC")
  resp <- get_scholar_resp(pub_page)
  print(resp)
  if (is.null(resp)){
    errorMessage <- paste("The scholar page for this publication is empty\n The function tried to fetch the following page:\n'",pub_page,"'",sep="")
    stop(errorMessage)
  } 
  resp <- read_html(resp)
  
  #We can access all article information from the divs with the class "gsc_oci_value"
  #Need to remember to add "." to the class or returns null results
  values <- html_nodes(pub_page,".gsc_oci_value")
  publication$author <- html_text(values[1])
  publication$date <- html_text(values[2])
  publication$journal <- html_text(values[3])
  publication$number <- html_text(values[4])
  return(publication)
}