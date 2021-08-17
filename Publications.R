###' Gets accurate information about a researcher's complete list of publications
###' 
###' Crawls through the list of publications of a scholar to get more accurate information
###' All publications without a publication_year will be removed from the list of publications
###' to ensure that we only preserve what should really be considered a publication
###' The function uses an internal timer to avoid being banned from crawling google scholar
###'
###' @param scholar_id the id from the scholar, must be non null
###' @param publication_list the list of publication from a scholar (optional)
###'
###' @return publication_list the updated publication list with more accurate information
get_publication_list <- function(scholar_id, publication_list){
  
  if(missing(scholar_id)) {
    stop("Parameter 'scholar_id' should be set")
  }
  
  
  #First we will need the name of the author for the rest of the processing 
  #Namely to find author's position in the paper
  #This will be passed to the clean_publication_data() function
  scholar_profile <- get_scholar_profile(scholar_id)
  
  
  if(missing(publication_list)){
    warning("'publication_list' was not set, the function automatically fetched the publications from the scholar based on 'scholar_id'")
    publication_list<-get_initial_publication_list(scholar_id, flush_cache = TRUE)
  }
  if(is.null(publication_list)){
    warning("List of publications is for this scholar")
    return (NA);
  }
  
  #First we need to curate the publication list to avoid errors when fetching more accurate information
  publication_list <- remove_publications_no_year(publication_list)
  
  cleaned_list <- c()
  pb = txtProgressBar(min = 0, max = nrow(publication_list), initial = 0, style= 3)
  for (i in 1:nrow(publication_list)){
    setTxtProgressBar(pb,i)
    current_publication <- publication_list[i,]
    current_publication <- clean_publication_data(current_publication,scholar_id,scholar_profile$name)
    cleaned_list <- rbind(cleaned_list, current_publication)
  }
  close(pb)
  return (cleaned_list)
  
}


###' Gets accurate information about a specific publication
###'
###' Gets accurate information from the google scholar page of a publication
###' Completes and correct partial information gathered from a scholar's personnal page
###' Each page gives the following information 
###' title, link to PDF <-- we already have these so we ignore them
###' author list, publication date, venue, pages <-- we need to add/correct with these
###'
###' This function can be called on a specific publication only
###' Or can be used to clean a whole list of publications by the function 'clean_all_publications'
###'
###' @param publication a publication as obtained from a scholar's publication list, must be non null
###' @param scholar_id the id of the scholar, must be non null
###' @param author_name the name of the scholar that we are interested in (optional). It is used to derive their position in the author list
###'
###' @return the updated publication with more accurate information
###' 
###' @importFrom rvest html_read html_attr html_nodes
###' @importFrom stringr strsplit
###' 
clean_publication_data <- function(publication, scholar_id, scholar_name){
  
  #pub_page <- "https://scholar.google.com/citations?view_op=view_citation&hl=en"
  #citation_for_view_url <- paste("citation_for_view=",scholar_id,":",publication$pubid, sep="")
  #scholar_id_url <- paste("user=",scholar_id, sep="")
  #pub_page <- paste(pub_page,scholar_id_url,citation_for_view_url, sep="&")
  pub_page <- compose_publication_url(scholar_id, publication$publication_id)
  if (is.null(pub_page)){
    errorMessage <- paste("The scholar page for this publication is empty\n The function tried to fetch the following page:\n'",pub_page,"'",sep="")
    stop(errorMessage)
  } 
  resp <- get_scholar_page(pub_page)
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
  
  index <- find_venue_index(fields)
  if(index!=-1){
    publication$venue <- html_text(values[index])
  }
  else{
    publication$venue <- NA
  }
  

  #Now we want to add author position and total number of authors
  authors <- strsplit(publication$author,", ")
  authors <- authors[[1]]
  publication$nb_authors <- length(authors)
  
  #We add author position for the scholar that we are interested in
  #There might special characters automatically left out in the publication metadata
  #So we look for ASCII equivalences
  for (i in  1:length(authors)){
    authors[i] <- get_ascii_string(authors[i])
  }
  scholar_name <- get_ascii_string(scholar_name)
  position <- match(scholar_name,authors)
  publication$position <- position
  
  
  #We want to retrieve the link to the publication itself
  link <- html_nodes(resp_parsed,".gsc_oci_title_link")
  if(length(link)!=0){
    link <- html_attr(link,"href")
  }
  else{
    link <- NA
  }
  publication$link <- link
  
  #We also want to get the link to all the other versions
  #This might come in handy to gather extra information, e.g., when a DOI is not available from this page, it might be from one of the other versions
  articles <- html_nodes(resp_parsed,".gsc_oms_link")
  alt_version <- NA
  if(length(articles)!=0){
    #versions now contains the N links at the bottom of the scholar page
    #We need to find, if any, the link to the "All N versions"
    index_of_version <- find_index_of_versions(articles)
    #Now we can store this in the alt_version of publication
    if(index_of_version!=-1){
      alt_version <- html_attr(articles[index_of_version],"href")  
    }
   
  }
  publication$alt_version <- alt_version
  
  
  
  
    
  #Finally we get the citation history of the publication
  citation_history <- fetch_publication_citation_history(resp_parsed)
  
  publication$citation_history <- citation_history

  
  #publication$author <- html_text(values[1])
  #publication$date <- html_text(values[2])
  #publication$journal <- html_text(values[3])
  #publication$number <- html_text(values[4])
  
  
  return(publication)
}

###' Gets citation history for a specific publication
###'
###' Extracts citation history from the google scholar page of a publication based on the barchart embedded in the page
###' @param resp_parsed HTML-parsed version of the response page
###'
###' @return the citation history of a specific publication as a string "year:citations;year:citations..."
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


###' Look for the information of the publication venue within a publication details page on Google scholar
###' 
###' The case of the venue is quite peculiar and error-prone as depending on the publication type (and the accuracy of the record)
###' there are many different fields that could represent the publication venue
###' So far, the following have been found to contain the publication venue "Journal | Book | Source | Conference | Publisher"
###' The case of "Publisher" seems to be the last resort for some preprints. It can only be used if none of the other fields are found
###'
###' @param fields the html_text() version of the HTML nodes of the fields in the publication information page
###'
###' @return the index of the venue field, if found, -1 otherwise
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

###' Look for a specific field within a publication details page on Google scholar
###' 
###' If the field is found, the function will return its index. -1 if not
###'
###' @param field_to_find the string of the field to look for, cannot be null
###' @param fields the html_text() version of the HTML nodes of the fields in the publication information page
###'
###' @return the index if the field is found, -1 otherwise.
find_field_index<- function(field_to_find,fields){
  if(is.null(field_to_find) || is.na(field_to_find)){
    stop("Parameter 'field_to_find' must be set")
    return (NA);
  }
  if(is.null(fields) || is.na(fields)){
    stop("Parameter 'fields' must be set")
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


###' Further remove publications that are likely to have been errors
###' 
###' After a full publication list has been generated, we should have data on the researcher's position in the author list
###' If the author was not found, it is likely this paper's metadata is erroneous or that the publication shouldn't have been added
###' to the researcher's page.
###' We remove it here
###'
###' @param publication_list the list of publication from a scholar, must be non null
###'
###' @return publication_list the updated publication list with more accurate information
###' @author Lonni Besançon
curate_publication_list <- function(publication_list){
  index_of_publications_to_remove <- c()
  for (i in 1:nrow(publication_list)){
    if(is.na(publication_list$position[i])){
      index_of_publications_to_remove <- c(index_of_publications_to_remove, i)
    }
  }
  if(is.null(index_of_publications_to_remove)){
    print("All publications in the list for this scholar are considered valid, none removed")
    return (publication_list)
  }
  publication_list <- publication_list[-index_of_publications_to_remove,]
  result_message <- paste("1 publication was removed from the list of publications (",index_of_publications_to_remove,")",sep="")
  print(result_message)
  
  return(publication_list)
}


###' Remove publications that are likely to have been wrongly added by Google Sholar
###' 
###' Current parameters to check that a publication is "valid" is the fact that it has a publication year
###' Such publications are therefore removed
###' The function is called by get_publication_list() automatically to avoid errors when fetching more detailed information
###' on a given publication
###'
###' @param publication_list the list of publication from a scholar, must be non null
###'
###' @return publication_list the updated publication list with more accurate information
###' @author Lonni Besançon
remove_publications_no_year <- function(publication_list){
  #if the scholar has no publications we return
  if(is.null(publication_list) || is.na(publication_list)){
    warning("List of publications is empty")
    return (NA);
  }
  index_of_publications_to_remove <- c()
  for (i in 1:nrow(publication_list)){
    if(is.na(publication_list$year[i])){
      index_of_publications_to_remove <- c(index_of_publications_to_remove, i)
    }
  }
  if(is.null(index_of_publications_to_remove)){
    print("All publications in the list for this scholar are considered valid, none removed")
    return (publication_list)
  }
  publication_list <- publication_list[-index_of_publications_to_remove,]
  result_message <- paste("1 publication was removed from the list of publications (",index_of_publications_to_remove,")",sep="")
  print(result_message)
  return (publication_list)
}


###' Gets the publications for a scholar
###'
###' Gets the publications of a specified scholar.
###' The function only fetches basic information such as title, year and number of citations.
###' All the other informations presented on this page are not accurate enough
###' Another function will fetch more accurate information
###' The function recursively calls itself if the number of publications from the scholar is greater than 100
###' In this case we recursively call the function on the next 100 until we are done crawling through all publications
###' 
###' @param id a string containing a valid Google Scholar ID.  
###' @param flush_cache should the cache be flushed? 
###' @param start_index the first publication to load
###' 
###' @return a data frame listing the publications and their details.
###' These include the publication title, author, journal, number,
###' cites, year, and the publication ID. By default, the list is sorted by citation.
###' @importFrom stringr str_extract str_sub str_trim str_replace
###' @importFrom xml2 read_html
###' @importFrom rvest html_nodes html_text html_attr
###' @import R.cache
###' @export
get_initial_publication_list <- function(scholar_id, flush_cache=FALSE, start_index = 0) {
  
  print("Fetching initial publication list from scholar's page")
  
  nb_publications <- 100 #The maximum number of publications we can have on a given scholar page
  sortby <-"citation"

  # Define the cache path
  cache.dir <- file.path(tempdir(), "schrawlar")
  setCacheRootPath(cache.dir)
  
  # Clear the cache if requested
  if (flush_cache) saveCache(NULL, list(scholar_id,start_index))
  
  # Check if already cached
  publication_list <- loadCache(list(scholar_id,start_index))
  
  # If not, get the data and save it to cache
  if (is.null(publication_list)) {
    
    url <- compose_scholar_url(scholar_id,start_index=start_index)
    page_html <- read_html(get_scholar_page(url))
    publications <- html_nodes(page_html,".gsc_a_at")
    
    
    #To recover the publication ID we extract it from the link in the page
    links <- html_attr(publications,"href")
    publication_ids <- c()
    #The publication_id is located in the URL after "citation_for_view"
    for(i in 1:length(links)){
      publication_ids[i] <- strsplit(links[i],"for_view=")[[1]][2]
      #The complete publication_id also contains the scholar_id
      #It is needed to construct a valid URL, but we still want to separate these two
      #They are separated by ":"
      publication_ids[i] <- strsplit(publication_ids[i],":")[[1]][2]
    }
    
    #Total citation information is located in a class="gsc_a_ac gs_ibl"
    citations <- html_text(html_nodes(page_html,".gsc_a_ac.gs_ibl"))
    #Year of publication is located in  a class="gsc_a_h gsc_a_hc gs_ibl"
    years <- html_text(html_nodes(page_html,".gsc_a_h.gsc_a_hc.gs_ibl"))
    #Title information is located in a class="gsc_a_at"
    titles <- html_text(html_nodes(page_html,".gsc_a_at"))
    
    publication_list <- data.frame(year=years,title=titles,citation=citations,publication_id=publication_ids)
    
    button <- html_nodes(page_html,"#gsc_bpf_more")
    
    #If the "show more" button is disabled, we don't have any more publications to parse
    is_button_disabled <- grepl("disabled", as.character(button), fixed = TRUE)
    if(!is_button_disabled){
      publication_list <-  rbind(publication_list,get_initial_publication_list(scholar_id,start_index=start_index+nb_publications,flush_cache=flush_cache))
    }
    
    # Now a final check to see if we are at the initial call of this function
    # If we are then we can save the results in the cache
    
    if (start_index == 0) {
      saveCache(publication_list, key=list(scholar_id, start_index))
    }
  }
  publication_list[publication_list==""] <- NA
  return(publication_list)
}



###' Gets the DOIs for a list of publications
###'
###' DOIs car be useful for other packages/API calls and so having them can help produce more data on a specific publication
###' If the link does not contain the DOI, might need other methods to extract the DOI from within the page that is in the link itself
###' If multiple DOIs are found it returns the list of all DOIs found
###' 
###' Sometimes the DOI is given in the official link to the publication
###' Sometimes it is in the webpage of the publication
###' Sometimes it could be only from "other versions" of the publication as stored by google scholar. This search is time consuming and only activated if deep_search is TRUE
###' 
###' @note Finding DOIs in a string is a very complex problem and one that cannot be solved with 100% accuracy. Current solution based on this https://www.findingyourway.io/blog/2019/03/13/2019-03-13_extracting-doi-from-text/
###' 
###' @param publication_list a list of publication as formatted by the function get_publication_list()
###' @param deep_search if set to true, the function will also look in alternative versions of the publication based on its google scholar page to try and find the DOI
###' 
###' @return the updated list
###' @importFrom stringr str_extract str_sub str_trim str_replace
###' @importFrom xml2 read_html
###' @importFrom rvest html_nodes html_text html_attr
###' @import R.cache
get_dois_for_publications <- function(publication_list, deep_search=TRUE){
  #The first step to get DOIs is to check if it is in the link of the publication itself
  for (i in 1:nrow(publication_list)){
    print(paste("In publication. I = ",i))
    if(grepl("osf.io",publication_list$link[i])){
      publication_list$doi[i] <- get_doi_from_osf(publication_list$link[i])
    }
    else{
      publication_list$doi[i]  <- get_dois_from_string(publication_list$link[i])  
    }
    
  }
  
  #The second step is to check the content of the link of the publication, this can take a while
  for (i in 1:nrow(publication_list)){
    print(paste("In link content I = ",i))
    if(is.na(publication_list$doi[i])){
      doi <- get_doi_in_link(publication_list$link[i])
      publication_list$doi[i] <- doi
    }
  }
  if(deep_search){
    for (i in 1:nrow(publication_list)){
      if(is.na(publication_list$doi[i])){
        print(paste0("In alt_link for I = ",i))
        if(!is.na(publication_list$alt_version[i])){
          print(paste("I = ",i))
          #First let's fetch the google scholar page with the other versions of the publications
          print(publication_list$alt_version[i])
          resp <- get_scholar_page(publication_list$alt_version[i])
          resp_parsed <- read_html(resp)
          
          #All links to other versions contain an attribute data-clk
          links_to_examine <- html_nodes(resp_parsed, "a[data-clk]")
          print(links_to_examine)
          links_to_examine <- html_attr(links_to_examine, "href")
          
          #Now the first step is to look for DOIs in the links themselves before we analyse the content of the webpage linked (which is more time-consuming)
          dois <- c()
          for (j in 1:length(links_to_examine)){
            if(grepl("osf.io",links_to_examine[j])){
              doi <- get_doi_from_osf(links_to_examine[j])
            }
            else{
              doi <- get_dois_from_string(links_to_examine[j])
            }
            if(!is.na(doi)){
              dois <- rbind(dois, doi)
            }
          }
          print(dois)
          
          #Now if we have found some DOIs, we can stop the search and use the most common DOI
          #First we check that we have found some DOIS
          if(is.null(dois)){
            doi <- NA
          }
          else if(nrow(dois)!=0){
            doi <- get_item_most_occurences(dois)
            print(paste("DOI",doi, sep=" = "))
          }
          
          #If this method failed, we can now look at the content of each link themselves
          if(is.na(doi)){
            print(paste0("In alt_link content for I = ",i))
            dois <- c()
            for (j in 1:length(links_to_examine)){
              doi <- get_doi_in_link(links_to_examine[j], return_all=TRUE)
              if(!is.null(doi) && is.na(doi)){
                dois <- rbind(dois, doi)
              }
            }
            print(dois)
            if(is.null(dois)){
              doi <- NA
            }
            else if(nrow(dois)!=0){
              doi <- get_item_most_occurences(dois)
              print(paste("DOI",doi, sep=" = "))
            }
          }
          publication_list$doi[i] <- doi
          
        }
      }
    }
  }
  return (publication_list)
}
