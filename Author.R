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


get_profile <- function(id) {
  site <- "http://scholar.google.com/citations?user="
  url_template <- paste0(site, "/citations?hl=en&user=%s")
  url <- compose_url(id, url_template)
  
  ## Generate a list of all the tables identified by the scholar ID
  page <- get_scholar_resp(url) %>% read_html()
  tables <- page %>% html_table()
  
  ## The citation stats are in tables[[1]]$tables$stats
  ## but the number of rows seems to vary by OS
  stats <- tables[[1]]
  rows <- nrow(stats)
  
  ## The personal info is in
  name <- page %>% html_nodes(xpath="//*/div[@id='gsc_prf_in']") %>% html_text()
  bio_info <- page %>% html_nodes(xpath="//*/div[@class='gsc_prf_il']") %>% html_text()
  interests <- page %>% html_nodes(xpath="//*/div[@id='gsc_prf_int']") %>% html_children() %>% html_text()
  affiliation <- bio_info[1]
  
  ## Specialities (trim out HTML non-breaking space)
  specs <- iconv(bio_info[2], from="UTF8", to="ASCII")
  specs <- str_trim(tolower(str_split(specs, ",")[[1]]))
  
  
  return(list(id=id, name=name, affiliation=affiliation,
              total_cites=as.numeric(as.character(stats[rows-2,2])),
              h_index=as.numeric(as.character(stats[rows-1,2])),
              i10_index=as.numeric(as.character(stats[rows,2])),
              fields=specs,
              interests=interests,))
}
