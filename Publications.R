##' Gets accurate information about a researcher's complete list of publications
##' 
##' Crawls through the list of publications of a scholar to get more accurate information
##' All publications without a publication_year will be removed from the list of publications
##' to ensure that we only preserve what should really be considered a publication
##' The function uses an internal timer to avoid being banned from crawling google scholar
##'
##' @param author_id the id from the scholar, must be non null
##' @param publication_list the list of publication from a scholar (optional)
##'
##' @return publication_list the updated publication list with more accurate information
clean_publication_list <- function(publication_list, author_id){
  sleep_time <- x1 <- runif(1, 1.1, 3.9)
  if(missing(author_id)) {
    error("Parameter 'author_id' should be set")
  }
  if(missing(publication_list)){
    warning("'publication_list' was not set, the function automatically fetched the publications from the scholar based on 'author_id'")
    publication_list <-get_publications(author_id)
  }
  if(is.null(publication_list)){
    warning("List of publications is for this scholar")
    return (NA);
  }
  
  #First we need to curate the publication list to avoid errors when fetching more accurate information
  publication_list <- curate_publication_list(publication_list)
  
  cleaned_list <- c()
  pb = txtProgressBar(min = 0, max = nrow(publication_list), initial = 0, style= 3)
  for (i in 1:nrow(publication_list)){
    setTxtProgressBar(pb,i)
    current_publication <- publication_list[i,]
    current_publication <- clean_publication_data(current_publication,author_id)
    cleaned_list <- rbind(cleaned_list, current_publication)
    Sys.sleep(sleep_time)
  }
  close(pb)
  return (cleaned_list)
  
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
  resp <- get_scholar_resp(pub_page)
  if (is.null(resp)){
    errorMessage <- paste("The scholar page for this publication is empty\n The function tried to fetch the following page:\n'",pub_page,"'",sep="")
    stop(errorMessage)
  } 
  resp_parsed <- read_html(resp)
  #We can access all article information from the divs with the class "gsc_oci_value"
  #Need to remember to add "." to the class or returns null results
  values <- html_nodes(resp_parsed,".gsc_oci_value")
  fields <- html_nodes(resp_parsed,".gsc_oci_field")
  fields <- html_text(fields)
  
  #Publication venue a particular case and there is already a function to handle this case that we call
  
  #We could put this whole thing in a loop if the package on CRAN adapts to a better naming of columns
  #As of now we leave as it is
  #Something with a list like this for instance that we just go through items_to_update <- c("Authors","Pages","Publication date","Description")
  #TODO check with CRAN package author
  
  index <- find_field_index("Authors",fields)
  if(index!=-1){
    publication$author <- html_text(values[index]) 
  }
  else{
    publication$author <- NA
  }
  
  index <- find_field_index("Pages",fields)
  if(index!=-1){
    publication$number <- html_text(values[index])
  }
  else{
    publication$number <- NA
  }
  
  index <- find_field_index("Publication date",fields)
  if(index!=-1){
    publication$date <- html_text(values[index])
  }
  else{
    publication$date <- NA
  }
  
  index <- find_field_index("Description",fields)
  if(index!=-1){
    publication$description <- html_text(values[index])
  }
  else{
    publication$description <- NA
  }
  
  #TODO journal is really not an ideal column name since it's the publication venue and it does not have to be a journal
  index <- find_venue_index(fields)
  if(index!=-1){
    publication$journal <- html_text(values[index])
  }
  else{
    publication$journal <- NA
  }
  
  citation_history <- fetch_publication_citation_history(resp_parsed)
  
  publication$citation_history <- citation_history

  
  #publication$author <- html_text(values[1])
  #publication$date <- html_text(values[2])
  #publication$journal <- html_text(values[3])
  #publication$number <- html_text(values[4])
  return(publication)
}

##' Gets citation history for a specific publication
##'
##' Extracts citation history from the google scholar page of a publication based on the barchart embedded in the page
##' @param resp_parsed HTML-parsed version of the response page
##'
##' @return the citation history of a specific publication as a string "year:citations;year:citations..."
fetch_publication_citation_history <- function(resp_parsed){
  years <- html_nodes(resp_parsed,".gsc_oci_g_t")
  cites_per_year <- html_nodes(resp_parsed,".gsc_oci_g_al")
  years <- html_text(years)
  cites_per_year <- html_text(cites_per_year)
  citation_history <- years
  citation_history <- vector(mode="list", length=length(years))
  names(citation_history) <- years
  citation_history<- ""
  for (i in 1:length(years)){
    year_info <- paste(years[i],":",cites_per_year[i], sep="")
    citation_history <- paste(citation_history,year_info, sep=";")
  }
  
  #We remove the first ";" at the begining of the string
  citation_history <- sub('.', '', citation_history)
  
  return (citation_history)
}


##' Look for the information of the publication venue within a publication details page on Google scholar
##' 
##' The case of the venue is quite peculiar and error-prone as depending on the publication type (and the accuracy of the record)
##' there are many different fields that could represent the publication venue
##' So far, the following have been found to contain the publication venue "Journal | Book | Source | Conference | Publisher"
##' The case of "Publisher" seems to be the last resort for some preprints. It can only be used if none of the other fields are found
##'
##' @param fields the html_text() version of the HTML nodes of the fields in the publication information page
##'
##' @return the index of the venue field, if found, -1 otherwise
find_venue_index<- function(fields){
  #First we create a list of all possible fields
  #"Publisher" should always be the last one of the list
  potential_venue_field <- c("Journal","Book","Source","Conference","Publisher")
  for (elem in potential_venue_field){
    index <- find_field_index(elem,fields)
    if(index != -1){
      return (index)
    }
  }
  
  return (-1)
}

##' Look for a specific field within a publication details page on Google scholar
##' 
##' If the field is found, the function will return its index. -1 if not
##'
##' @param field_to_find the string of the field to look for, cannot be null
##' @param fields the html_text() version of the HTML nodes of the fields in the publication information page
##'
##' @return the index if the field is found, -1 otherwise.
find_field_index<- function(field_to_find,fields){
  if(is.null(field_to_find) || is.na(field_to_find)){
    error("Parameter 'field_to_find' must be set")
    return (NA);
  }
  if(is.null(fields) || is.na(fields)){
    error("Parameter 'fields' must be set")
    return (NA);
  }
  
  for (index in 1:length(fields)){
    #(Un)comment lines below for debugging
    #mess <- paste("fields[index] = ",fields[index]," field_to_find = ",field_to_find,"fields[index] == field_to_find = ",(fields[index] == field_to_find))
    #print(mess)
    if(fields[index] == field_to_find){
      return (index)
    }
  }
  
  return (-1)
}


##' Remove publications that are likely to have been wrongly added by Google Sholar
##' 
##' Current parameters to check that a publication is "valid" is the fact that it has a publication year
##' TODO if the year is not given but the publication is cited, leave it in
##'
##' @param publication_list the list of publication from a scholar, must be non null
##'
##' @return publication_list the updated publication list with more accurate information
curate_publication_list <- function(publication_list){
  #if the scholar has no publications we return
  if(is.null(publication_list) || is.na(publication_list)){
    warning("List of publications is empty")
    return (NA);
  }
  index_of_publications_to_remove <- c()
  pb = txtProgressBar(min = 0, max = nrow(publication_list), initial = 0, style= 3)
  for (i in 1:nrow(publication_list)){
    if(is.na(publication_list$year[i])){
      index_of_publications_to_remove <- c(index_of_publications_to_remove, i)
    }
    setTxtProgressBar(pb,i)
  }
  close(pb)
  if(is.null(index_of_publications_to_remove)){
    print("All publications in the list for this scholar are considered valid, none removed")
    return (publication_list)
  }
  publication_list <- publication_list[-index_of_publications_to_remove,]
  result_message <- paste(length(index_of_publications_to_remove)," publications were removed from the list of publications (",index_of_publications_to_remove,")",sep="")
  print(result_message)
  return (publication_list)
}