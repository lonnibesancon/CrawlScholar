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

###' Return the index of the element in a list that contains "version"
###' 
###' Use this convenience fonction to find the element that will link to all the other versions of a publication
###' that can be found at the bottom of a google scholar page.
###'
###' @param list a list of all html elements containing a URL at the bottom of a google scholar's page for a publication
###' 
###' @return the index of the element that contains "version"
###' @author Lonni Besançon
find_index_of_versions <- function(list){
  if(length(list) ==0){
    return (-1)
  }
  for (i in 1:length(list)){
    text <- html_text(list[i])
    if(grepl("version",text)){
      return (i)
    }
  }
  return (-1)
}



###' Composes a google scholar URL based on an ID
###' 
###'
###' @param scholar_id a valid google scholar ID of a scholar profile
###' @param start_index the index from which to start showing publications after they have been ordered
###' @param the number of publications to display on the page
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


###' Composes a google scholar URL to find the metrics of a specific venue
###' 
###' The venue passed as a parameter will be stripped of all whitespaces to make the query URL valid
###' 
###' @param venue a venue name
###' @importFrom stringr str_replace_all
###' @return the composed URL
###' @author Lonni Besançon
compose_venue_url <- function(venue){
  page <- "https://scholar.google.com/citations?hl=en&view_op=search_venues&vq="
  venue <- str_replace_all(venue," ","+")
  return(paste(page,venue, sep=""))
  
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
###' @param flush_cache if the cash should be flushed
###'
###' @return the response from GET
###' 
###' @import R.cache
###' @author Lonni Besançon
get_index_best_matching_string <- function(string,list,max_distance,case_sensitive=FALSE, flush_cache=FALSE){
  
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
  sleep_time <- x1 <- runif(1, 2.1, 3.6)
  Sys.sleep(sleep_time)
  resp <- httr::GET(url)
  if (httr::status_code(resp) == 200) {
    return(resp)
  }
  else{
    error_message <- paste("The url you provided is incorrect. Here it is for your reference:",url,sep="\n")
    stop(error_message)
  }
}

###' Convenience function to help at finding a match in the core ranking for conferences/journals
###' 
###' There is no garanty that the function will produce an output that will find a match
###' This is based on my own experience with core searches for conferences/journals
###' Might need some refinements. Send pull requests my way if you have better venue clean-up routines
###' 
###' @note Processing is different based on whether the venue is a journal or a conference, remember to set the value of is_journal accordingly
###' 
###' @param venue the venue name
###' @param is_journal set to FALSE for conference 
###'
###' @return a string that seems more likely to produce matches in the core ranking for conferences
###' @importFrom stringr str_c str_replace_all
###' @author Lonni Besançon
clean_venue_for_core<- function(venue, is_journal= TRUE){
  #If it is a journal we only do this, but this should also be done for conferences
  clean_venue <- str_replace_all(venue, "[^[:alnum:] ]", "") #Maybe use [^a-zA-Z0-9]
  if(!is_journal){
    #We need to remove words present in the following list
    to_replace <- c("Proceedings of the", "Proceedings", "Proceddings of", "Proc of the", "Proc of")
    patterns <- str_c(to_replace, collapse="|")
    clean_venue <- str_replace_all(venue, regex(patterns, ignore_case = TRUE), "")
    #Now we remove numbers
    clean_venue <- gsub('[[:digit:]]+', '', clean_venue)
  }
  return (clean_venue)
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
    list_impact_factors <- rbind(list_impact_factors, as.data.frame(tables[i]))
  }
  #We don't want the rank
  list_impact_factors <- subset(list_impact_factors,select =-1)
  #We want to remove the old headers from the reading of the HTML page
  list_impact_factors <- list_impact_factors[-1,]
  return (list_impact_factors)
}


###' Convenience function to find a doi in a string
###' 
###' DOIs car be useful for other packages/API calls and so having them can help produce more data on a specific publication
###' If the link does not contain the DOI, might need other methods to extract the DOI from within the page that is in the link itself
###' If multiple DOIs are found it returns the list of all DOIs found
###' 
###' @note Finding DOIs in a string is a very complex problem and one that cannot be solved with 100% accuracy. Current solution based on this https://www.findingyourway.io/blog/2019/03/13/2019-03-13_extracting-doi-from-text/
###' 
###' @param string the string to analyse
###' @param return_all does it return all of the results or just the one with most occurences, default = FALSE
###'
###' @return the DOI that has been found, if multiple ones are found, return the index the one found the most if 
###' @importFrom stringi stri_match_all_regex
###' @author Lonni Besançon
get_dois_from_string<- function(string, return_all=FALSE){
  doi_regex <- "10.\\d{4,9}/[-._;()/:a-z0-9A-Z]+"
  res <- stri_match_all_regex(string,doi_regex)
  if(length(res)==0 || is.na((res))){
    return (NA)
  }
  res <- res[[1]]
  #Now we might have multiple DOIs found in the string
  #We want to have a list of all of them and their occurence and pick the one that is the most frequent
  if(!return_all){
    return(get_item_most_occurences(res))  
  }
  return(res)
  
}

###' Convenience function to return the value with the most occurences in a dataframe
###' 
###' @param df a dataframe
###' @return the item that occured the most frequently
###' 
###' @author Lonni Besançon
get_item_most_occurences <- function(df){
  df <- as.data.frame(table(df))
  index_max_occurence <- -1
  max_occurence <- -1
  print("df")
  print(df)
  for(i in 1:nrow(df)){
    if(as.numeric(df$Freq[i]) > max_occurence){
      max_occurence <- df$Freq[i]
      index_max_occurence <- i
    }
  }
  return (as.character(df[index_max_occurence,1]))
}

###' Convenience function to find a doi in a webpage
###' 
###' DOIs car be useful for other packages/API calls and so having them can help produce more data on a specific publication
###' If the link does not contain the DOI, might need other methods to extract the DOI from within the page that is in the link itself
###' If multiple DOIs are found it returns the list of all DOIs found
###' 
###' 
###' @param link the link to analyse
###' @param escape_pdfs should pdfs be escaped, default, TRUE
###' @param return_all does the function return all of the results or just the one with most occurences
###' 
###'
###' @return the DOI that has been found, if multiple it only returns the one that has been found the most, NA if the page does not exist or if it can't be parsed
###' @importFrom httr GET
###' @importFrom rvest read_html html_text
###' @author Lonni Besançon
get_doi_in_link <- function(link, escape_pdf=TRUE, return_all= FALSE){
  if(is.na(link)){
    return(NA)
  }
  if(grepl(".pdf", link) && escape_pdf){
    return(NA)
  }
  resp <- httr::GET(link)
  if (httr::status_code(resp) != 200) {
    return(NA)
  }
  resp_parsed <- read_html(resp)
  resp_parsed <- html_text(resp_parsed)
  
  doi <- get_dois_from_string(resp_parsed, return_all = return_all)
  
  return (doi)
}

###' Convenience function to find a doi in a string (most likely a link)
###' 
###' DOIs car be useful for other packages/API calls and so having them can help produce more data on a specific publication
###' If the link does not contain the DOI, might need other methods to extract the DOI from within the page that is in the link itself
###' If multiple DOIs are found it returns the list of all DOIs found
###' 
###' @param strin the string to analyse
###'
###' @return the list of DOIs found, NA if none
###' @importFrom stringr str_split
###' @author Lonni Besançon
'''get_dois_from_string<- function(string){
  #First we need to find if there is a DOI in the string
  
  doi_part <-str_split(string,"10\\.")
  doi_part<-doi_part[[1]]
  
  print("doi_part")
  print(doi_part)
  dois <- c()
  
  #If doi_part has a length of 0 then there is no doi.
  
  #If doi_part has a length of 2, this is a simple case
  if(length(doi_part)==2){
    doi <- doi_part[2]
    doi<-paste0("10.",doi)
    doi <- extract_doi_from_string(doi)
    print(paste0("-----DOI FOUND == ",doi))
    return (doi)
  }
  else if(length(doi_part)>2){
    for(i in 2:length(doi_part)){
      doi <- doi_part[i]
      doi<-paste0("10.",doi)
      doi <- extract_doi_from_string(doi)
      print(paste0("-----DOI FOUND == ",doi))
      dois <- append(dois, doi)
    }
    return(dois)
  }
  return (NA)
}'''

###' Convenience function to extract an exact DOI from a string that contains the DOI and something else
###' 
###' 
###' @param strin the string to analyse, must contain a DOI
###'
###' @return the extracted DOI, NA if none is found
###' @importFrom stringr str_split
###' @author Lonni Besançon
'''
extract_doi_from_string <- function(string){
  #Now we need to make sure that there is nothing behind the doi
  #A doi will be in the shape of "prefix/suffix" so if there is anything after this that is of the like "/iaznfoerve" we can remove it
  
  #so first lets extract the prefix
  split <- str_split(string, "/")
  split <- split[[1]]
  #There was no DOI in this string
  if(length(split) < 2){
    return (NA)
  }
  else{
    prefix <- split[1]
    suffix <- split[2]
    
    #Now it is possible that the suffix contains more than the DOI.
    #In theory, it contains anything that was originally in the text until the next "/"
    #Now, all we need is to find the next non alphanumeric character except "." and "-" that can be found in DOIs
    
    r <- regex("[^a-zA-Z0-9.-]")
    split <- str_split(suffix, r)
    split <- split[[1]]
    print("split")
    print(split)
    suffix <- split[1]
    
    #Now we can put prefix and suffix together with "/" added
    doi <- paste0(prefix, "/", suffix)
    return (doi)
  }
}'''

###' Convenience function to extract an exact DOI from a string that contains the DOI and something else
###' 
###' From all the testing done it would seem that all DOIs from OSF are created by using adding the project/preprint identifier
###' To the same two prefixes. So all DOIs will be of the shape
###' 10.31219/osf.io/preprint_ID
###' 
###' @param strin the string to analyse, the link to the OSF preprint
###'
###' @return the created DOI
###' @importFrom stringr str_split
###' @author Lonni Besançon
get_doi_from_osf <- function(string){

  #If the link contains "osf.io" it can either be in one of these two forms
  #https://osf.io/preprints/3z7kx/
  #https://files.osf.io/v1/resources/63efj/providers/osfstorage/60508a5e80460f005c980f1e?format=pdf&action=download&direct&version=2
  #If it's the first one we can simply split by "/" and extract the ID
  
  prefix_osf <- "10.31219/osf.io/"
  doi <- NA
  split <- str_split(string, "/")
  split <- split[[1]]
  if(grepl("files.osf.io/v1/resources/",string)){
    doi <- paste0(prefix_osf,split[6])
  }
  else{
    doi <- paste0(prefix_osf,split[5])
  }
  return (doi)
}





###' Convenience function to help at finding a match in the google scholar metrics page
###' 
###' There is no garanty that the function will produce an output that will find a match
###' This is based on my own experience with google scholar searches for conferences/journals
###' Might need some refinements. Send pull requests my way if you have better venue-clean-up routines
###' 
###' @note Processing is different based on whether the venue is a journal or a conference, remember to set the value of is_journal accordingly
###' 
###' @param venue the venue name
###' @param is_journal set to FALSE for conference 
###'
###' @return a list of strings that seem more likely to produce matches in the scholar metrics queries
###' @importFrom stringr str_c str_replace_all str_match
###' @author Lonni Besançon
clean_venue_for_scholar<- function(venue, is_journal= TRUE){
  #If it is a journal we only do this, but this should also be done for conferences
  clean_venue <- str_replace_all(venue, "[^[:alnum:] ]", "") #Maybe use [^a-zA-Z0-9]
  if(!is_journal){
    #We need to remove words present in the following list
    clean_venue <- gsub("[[:digit:]]+th", "", clean_venue)
    
    #Find the name of the conference if it is in between "Proceedings of the ... conference" or something similar
    clean_venue <- str_match(tolower(clean_venue), tolower("Proceedings of the\\s*(.*?)\\s*conference"))
    print(clean_venue)
    clean_venue <- clean_venue[,2]
    #Now clean_venue contains the text between "Proceedings of the ... conference"
    #But it might also contain a number or a number and "st", "nd", "rd", or "th"
    #So we want to remove these
    clean_venue <- gsub("[[:digit:]]+", "", res)
    print(clean_venue)
    '
    to_replace <- c("Proceedings of the", "Proceedings", "Procedings of", "Proc of the", "Proc of")
    patterns <- str_c(to_replace, collapse="|")
    clean_venue <- str_replace_all(venue, regex(patterns, ignore_case = TRUE), "")
    #Now we remove numbers
    clean_venue <- gsub("[[:digit:]]+", "", clean_venue)'
  }
  return (clean_venue)
}
