###' Returns the co-author list of a specific author
###' 
###' The function uses the list of publications as a way to get co-authors
###' It will produce a better output if 'clean_publication_list()' has been called to produce it
###'
###' @param publication_list the list of publications 
###' @param scholar_id the id from the scholar, must be non null
###'
###' @return a table of all co-authors and how many publications they participated on (name and occurences)
###' @author Lonni Besançon
get_coauthors <- function(publication_list, scholar_id){
  scholar <- get_profile(scholar_id)
  scholar$name 
  
  co_authors <- c()
  for (elem in publication_list$author){
    list <- strsplit(elem,", ")
    list <- list[[1]]
    for (author in list){
      if(author!=scholar$name){
        co_authors <- append(co_authors,author)
      }
    }
  }
  unique_list <- table(co_authors)
  
  return(unique_list)
  
}


###' Gets profile information for a scholar
###' 
###' GGiven a specific valid google scholar ID, this function returns basic information about the scholar
###' Namely, the function returns, the scholar's name, affiliation, position and citation history (from the graph on the right of a google scholar page)
###'
###' @param publication_list the list of publications 
###' @param scholar_id the id from the scholar, must be non null
###'
###' @return a table of all co-authors and how many publications they participated on (name and occurences)
###' @importFrom xml2 read_html
###' @importFrom rvest html_table html_nodes html_text
###' @author Lonni Besançon
get_scholar_profile <- function(scholar_id) {
  url <- compose_scholar_url(scholar_id)
  page <- get_scholar_page(url) 
  html_page <- read_html(page)
  print(html_page)
  tables <- html_table(html_page)

  # Citation data is in tables[[1]]
  stats <- tables[[1]]
  rows <- nrow(stats)
  
  name <- html_text(html_nodes(html_page,"#gsc_prf_in"))
  infos <- html_text(html_nodes(html_page,".gsc_prf_il"))
  infos <- strsplit(infos[1],", ")
  infos <- infos[[1]]
  position <- infos[1]
  affiliation <- infos[2]
  
  scholar <- c()
  
  scholar$id <- id
  scholar$name <- name
  scholar$affiliation <- affiliation
  scholar$position <-position
  scholar$all_cites <- stats[[2]][1]
  scholar$all_h_index <- stats[[2]][2]
  scholar$all_i_index <- stats[[2]][3]
  scholar$since1 <- stats[[3]][1]
  scholar$since2 <- stats[[3]][2]
  scholar$since3 <- stats[[3]][3]
  
  year <- names(stats[3])
  year <- strsplit(year," ")[[1]][2]
  names(scholar)[names(scholar) == "since1"] <- paste("since_",year,"_cites", sep="")
  names(scholar)[names(scholar) == "since2"] <- paste("since_",year,"_h_index", sep="")
  names(scholar)[names(scholar) == "since3"] <- paste("since_",year,"_i_index", sep="")
  
  return(scholar)
}
