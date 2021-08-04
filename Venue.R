###' Gets the core ranking for a specific vpublication using its venue field
###' 
###' This returns the core ranking of the venue for a publication
###' This function automatically process the venue as input to maximize chances of a match
###' The core DB is actually divided into a conference portal and a journal portal. This functions searches on both portals.
###' Based on this match, we can return the type of venue that we have found
###' 
###' @note To find a match in the core DB, we need to clean the venue's name very often
###'       Characters such as "." used in abreviations of a journal's name are likely to prevent a match for a journal.
###'       For a conference, the year or the presence of "Proceedings of the" might also prevent a match.
###'       The core DB only contains computing research venue. Other venues will not be found.
###'
###' @param publication a publication for which we want to know the core ranking
###'
###' @return a list containing the name found in the CORE database, the year of the ranking and the ranking
###' @author Lonni Besançon
get_core_ranking <- function(publication){
  if(is.na(publication$venue)){
    return (list(NA,NA,NA))
  }
  
  #We look for a journal first
  #The first thing we need to do is clean the venue of all characters that would prevent a match in the core ranking
  clean_venue <- str_replace_all(publication$venue, "[^[:alnum:] ]", "") #Maybe use [^a-zA-Z0-9]
  clean_venue <- str_replace_all(clean_venue, " ","+")
  ranking <- get_core_ranking_venue(clean_venue)
  
  #If this is empty, then it could be a conference
  if(is.na(ranking[1])){
    #We need to words present in the following list
    to_replace <- c("Proceedings of the", "Proceedings", "Proceddings of", "Proc of the", "Proc of")
    patterns <- str_c(to_replace, collapse="|")
    clean_venue <- str_replace_all(publication$venue, regex(patterns, ignore_case = TRUE), "")
    #Now we remove numbers
    clean_venue <- gsub('[[:digit:]]+', '', clean_venue)
    #And finally all of the whitespaces
    clean_venue <- str_replace_all(clean_venue, " ","+")
    ranking <- get_core_ranking_venue(clean_venue, is_journal = FALSE)
  }
  
  return(ranking)
  
}

###' Gets the core ranking for a specific vpublication using its venue field
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
###' 
###' @param venue a specific venue name
###' @param is_journal whether the venue is a journal or not (TRUE by default)
###' @param print_query use TRUE if you want to see the GET request that is sent
###'
###' @return a list containing the name found in the CORE database, the year of the ranking and the ranking
###' @author Lonni Besançon
###' @examples {
###'   venue <- "IEEE Transactions on Visualization and Computer Graphics"
###'   venue <- str_replace_all(venue, " ","+")
###'   ranking <- get_core_ranking_venue(venue)
###' }
get_core_ranking_venue <- function(venue,is_journal=TRUE,print_query=FALSE){
  
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
  if(!nrow(tables)==0){
    return(c(tables[1],tables[3],tables[2]))
  }
  else{
    return(c(NA,NA,NA))
  }
}


###' Gets the journal impact factor for a specific venue
###'
###' @param venue a specific venue name, has to be a journal name
###'
###' @return a list containing the name found in the list and the Impact Factor
###' @author Lonni Besançon
###' @examples {
###'   venue <- "IEEE Transactions on Visualization and Computer Graphics"
###'   venue <- str_replace_all(venue, " ","+")
###'   ranking <- get_core_ranking_venue(venue)
###' }
get_journal_impact_factor <- function(venue){
  url <- "https://impactfactorforjournal.com/jcr-2021/"
  resp <- httr::GET(url)
  page_html <- read_html(resp)
}