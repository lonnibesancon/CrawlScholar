##' Returns the co-author list of a specific author
##' 
##' The function uses the list of publications as a way to get co-authors
##' It will produce a better output if 'clean_publication_list()' has been called to produce it
##'
##' @param publication_list the list of publications 
##' @param author_id the id from the scholar, must be non null
##'
##' @return a table of all co-authors and how many publications they participated on (name and occurences)
get_coauthors <- function(publication_list, author_id){
  scholar <- get_profile(id)
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