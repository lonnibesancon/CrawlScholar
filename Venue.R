###' Convenience function to get the core ranking for a specific vpublication using its venue field
###' 
###' This returns the core ranking of the venue for a publication
###' This function automatically process the venue as input to maximize chances of a match
###' The core DB is actually divided into a conference portal and a journal portal. This functions searches on both portals.
###' Based on this match, we can return the type of venue that we have found
###' 
###' @note To find a match in the core DB, we need to clean the venue's name very often
###'       Characters such as "." used in abreviations of a journal's name are likely to prevent a match for a journal.
###'       For a conference, the year or the presence of "Proceedings of the" might also prevent a match.
###'       There is a convenience function in Utils.R called clean_venue_for_core(venue,is_journal) which could help find a match for a conference/journal
###'
###' @param publication a publication for which we want to know the core ranking
###' @importFrom stringr str_replace_all
###' @return a list containing the name found in the CORE database, the year of the ranking and the ranking
###' @author Lonni Besançon
'
get_core_ranking <- function(publication){
  
  if(is.na(publication$venue)){
    return (list(NA,NA,NA))
  }
  
  ranking <- get_core_ranking_venue(clean_venue)
  
  #If this is empty, then it could be a conference
  if(is.na(ranking[1])){
    
    #We need to remove whitespace (if any) to prepare the query
    venue <- str_replace_all(clean_venue, " ","+")
    ranking <- get_core_ranking_venue(clean_venue, is_journal = FALSE)
  }
  
  return(ranking)
  
}
'


###' Gets the core ranking for a specific venue
###' 
###' This returns the core ranking of a given venue as a parameter
###' 
###' For a conference, we need to remove potential years and and "proceedings of the"
###' The core DB is actually divided into a conference portal and a journal portal
###' We need to search on both
###' Based on this match, we can return the type of venue that we have found
###' The core DB only contains computing research venue. Other venues will not be found.
###' This function automatically process the venue as input to maximize chances of a match
###' If you have a venue string that you already know will return a match use get_core_ranking_venue()
###' Venue should not containg any whitespace (the function removes them automatically)
###'
###' @note To find a match in the core DB, we need to clean the venue's name very often
###'       Characters such as "." used in abreviations of a journal's name are likely to prevent a match for a journal.
###'       For a conference, the year or the presence of "Proceedings of the" might also prevent a match.
###'       The core DB only contains computing research venue. Other venues will not be found.
###'       There is a convenience function in Utils.R called clean_venue_for_core(venue,is_journal) which could help find a match for a conference/journal
###' 
###' @param venue a specific venue name
###' @param is_journal whether the venue is a journal or not (TRUE by default)
###' @param print_query use TRUE if you want to see the GET request that is sent
###' @param index_to_return the index of the result to return, default = 1, set to -1 to get all results
###'
###' @return a list containing the name found in the CORE database, the year of the ranking and the ranking
###' @importFrom xml2 read_html
###' @importFrom rvest html_table html_text
###' @importFrom stringr str_replace_all
###' @author Lonni Besançon
###' @examples {
###'   venue <- "IEEE Transactions on Visualization and Computer Graphics"
###'   venue <- str_replace_all(venue, " ","+")
###'   ranking <- get_core_ranking_venue(venue)
###'   venue <- "visualization"
###'   all_matches <- get_core_ranking_venue(venue, index_to_return = -1)
###' }
get_core_ranking_venue <- function(venue,is_journal=TRUE,print_query=FALSE,index_to_return=1){
  
  #First we remove all whitespaces if there are any
  venue <- str_replace_all(venue, " ","+")
  
  if(is_journal){
    portal <- "http://portal.core.edu.au/jnl-ranks/"
  }
  else{
    portal <- "http://portal.core.edu.au/conf-ranks/"
  }
  
  search_string <- paste0("?search=",venue,"&by=all")
  url <- paste0(portal,search_string)
  if(print_query){
    print(url) 
  }
  resp <- httr::GET(url)
  page_html <- read_html(resp)
  tables <- as.data.frame(html_table(page_html))
  if(!nrow(tables)==0 && index_to_return!=-1){
    return(c(tables[1,index_to_return],tables[3,index_to_return],tables[2,index_to_return]))
  }
  else if(index_to_return==-1){
    return(tables)
  }
  else{
    return(c(NA,NA,NA))
  }
}


###' Gets the journal impact factor for a specific venue
###'
###' @param venue a specific venue name, has to be a journal name
###' @param max_distance the maximum allowed distance between the venue and a potential match. Set by default at 10 as they might be differences in the use of articles like "The"
###' @param list_of_journals a list of journals and their impact factors. This function assumes that it contains at least a column "journal" and a column "IF". Null by default.
###' 
###' @note get_batch_journal_impact_factor() will give it the list_of_journals paramater
###' 
###' @return a list containing the name found in the list and the Impact Factor
###' @author Lonni Besançon
###' @examples {
###'   venue <- "IEEE Transactions on Visualization and Computer Graphics"
###'   venue <- str_replace_all(venue, " ","+")
###'   ranking <- get_core_ranking_venue(venue)
###' }
get_journal_impact_factor <- function(venue, max_distance=5, list_of_journals=NULL){
  matching_journal <- c(NA,NA)
  if(is.na(venue) || is.null(venue)){
    warning("The venue was empty, returning.")
    return(matching_journal);
  }
  if(length(list_of_journals)==0){
    list_of_journals <- get_list_of_impact_factors()
    colnames(list_of_journals) <- c("journal","IF")
  }
  
  for(i in 1:nrow(list_of_journals)){
    list_of_journals$journal[i] <- str_replace_all(list_of_journals$journal[i], "\n", " ")
    list_of_journals$journal[i] <- str_replace_all(list_of_journals$journal[i], "[^[:alnum:] ]", "") #Maybe use [^a-zA-Z0-9]
  }
  
  index_min <- get_index_best_matching_string(venue, list_of_journals$journal,max_distance=max_distance)
  if(index_min!=-1){
    matching_journal <- c(list_of_journals$journal[index_min],list_of_journals$IF[index_min])
  }
  return (matching_journal)
}


###' Convenience function to add the Impact factor of all publications in a list of venues
###'
###' @param venues_list the list of venues to consider
###' @param max_distance the maximum allowed distance between the venue and a potential match. Set by default at 10 as they might be differences in the use of articles like "The"
###' @param list_of_journals a list of journals and their impact factors. This function assumes that it contains at least a column "journal" and a column "IF". Null by default.
###'
###' @return a dataframe containing the name found in the list and the Impact Factor
###' @author Lonni Besançon
###' @examples {
###'   publication_list <- get_publication_list(scholar_id)
###'   results <- get_batch_journal_impact_factor(publication_list$venue)
###' }
get_batch_journal_impact_factor <- function(venues_list, max_distance=5, list_of_journals=NULL){
  if(is.null(list_of_journals)){
    list_of_journals <- get_list_of_impact_factors()
  }
  print("Fetching journal impact factors for the list of venues. This may take a while")
  results <- c()
  length(venues_list)
  pb = txtProgressBar(min = 0, max = length(venues_list), initial = 0, style= 3)
  for (i in 1:length(venues_list)){
    setTxtProgressBar(pb,i)
    if(is.na(venues_list[i])){
      next
    }
    res <- get_journal_impact_factor(new_pub_list$venue[i])
    results <- rbind(results,c(res[1],res[2]))
  }
  close(pb)
  return (results)
}


###' Gets the google scholar metrics for a specific venue
###' 
###' In case of multiple results it returns the "index_to_return" in the list
###'
###' @note The function uses cache memory to limit call to Google scholar and avoid being blocked by Google.
###'
###' @param venue a specific venue name, has to be a journal name
###' @param index_to_return the index of the result to return, default = 1, set to -1 to get all results
###' @param flush_cache should the cache be flushed, default = FALSE
###' 
###' 
###' @return a dataframe containing the name and scholar metrics found. If index_to_return == -1, or index_to_return greated than the number of results, returns all the results on the page. If no results, returns an empty dataframe. 
###' @author Lonni Besançon
###' @import R.cache
###' @examples {
###'   venue <- "IEEE Transactions on Visualization and Computer Graphics"
###'   best_match <- get_venue_scholar_metrics(venue)
###'   all_results <- get_venue_scholar_metrics(venue, index_to_return=-1)
###' }
get_venue_scholar_metrics <- function(venue,index_to_return=1,flush_cache=FALSE){
  
  # Define the cache path
  cache.dir <- file.path(tempdir(), "schrawlar")
  setCacheRootPath(cache.dir)
  
  # Clear the cache if requested
  if (flush_cache) saveCache(NULL, list(venue))
  
  # Check if already cached
  resp <- loadCache(key=list(venue))
  
  # If not, get the data and save it to cache
  if (is.null(resp)) {
    print("calling scholar query")
    url <- compose_venue_url(venue)
    resp <- get_scholar_page(url)
  }
  #We save that in the cache
  saveCache(resp, key=list(venue))
  
  
  html_page <- read_html(resp)
  table <- as.data.frame(html_table(html_page,index_to_return))
  
  if(nrow(table) == 0){
    warning(paste0("There was no match with this venue (",venue,") on google scholar you might want to modify the venue so it is found by google scholar"))
    return(table)
  }
  colnames(table)[1] <- "result.rank"
  if(index_to_return==-1){
    return (table)
  }
  if(index_to_return > nrow(table)){
    warning("index_to_return is greater than the number of results from Google Scholar, returning the whole table")
    return(table)
  }
  return (table[index_to_return,])
}

###' Convenience function to get the google scholar metrics for a list of venues
###' 
###' In case of multiple results it returns the "index_to_return" in the list
###'
###' @note The function will return the first result (if any is available) for all the queries to scholar
###'
###' @param venue_list a list of all venues to find in scholar
###' 
###' 
###' @return a dataframe containing the name and scholar metrics found for all venues
###' @author Lonni Besançon
###' @examples {
###'   venues <- c("CHI","Transactions Visualization")
###'   venues_data <- get_venue_scholar_metrics(venues)
###' }
get_batch_venue_scholar_metrics <- function(venue_list){
  results <- c()
  for(venue in venue_list){
    results <- rbind(results,get_venue_scholar_metrics(venue))
  }
  return (results)
}

