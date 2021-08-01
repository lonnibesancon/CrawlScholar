################################################################
# Main.R
# Code for to fetch data for Academic Hiring Visualization
# CC-BY Lonni Besançon et al., 2021
################################################################

##'@importFrom xml2 read_html
##' @importFrom rvest html_table html_nodes html_text html_children

#Check that "likert" is installed
if(!require(scholar)){
  install.packages("scholar")
  library(scholar)
}

#Documentation for the scholar package is here: https://cran.r-project.org/web/packages/scholar/scholar.pdf


list <- read.csv("../data/list.csv")
list$id <- c()

length(list)


for(i in 1:nrow(list)){
  id <- get_scholar_id(last_name = list$family[i], first_name = list$first[i])
  list$id[i] <- id
}

# Define the id for the researcher
id <- list$id[1]

# Get his profile and print his name
l <- get_profile(id)
l$name 

# Get his citation history, i.e. citations to his work in a given year 
get_citation_history(id)

# Get his publications (a large data frame)
publication_list <-get_publications(id)

current_publication <- publication_list[1,]

clean_publication_data(current_publication)


library(httr)
library(xml2)
library(rvest)
library(RCurl)
library(XML)
resp <- get_scholar_resp("https://scholar.google.fr/citations?view_op=view_citation&hl=en&user=ulkW7fgAAAAJ&citation_for_view=ulkW7fgAAAAJ:u-x6o8ySG0sC")
resp <- get_scholar_resp("https://scholar.google.fr/citations?user=ulkW7fgAAAAJ&hl=en")

current_publication <- clean_publication_data(current_publication,id)


##' Gets accurate information about a specific publication
##'
##' Gets accurate information from the google scholar page of a publication
##' Completes and correct partial information gathered from a scholar's personnal page
##' Each page gives the following information 
##' title, link to PDF <-- we already have these so we ignore them
##' author list, publication date, venue, pages <-- we need to add/correct with these
##'
##' @param publication a publication as obtained from a scholar's publication list
##'
##' @return the updated publication with more accurate information
clean_publication_data <- function(publication, author_id){
  #function code starts here
  
  page <- "https://scholar.google.com/citations?view_op=view_citation&hl=en"
  citation_for_view_url <- paste("citation_for_view=",author_id,":",publication$pubid, sep="")
  author_id_url <- paste("user=",author_id, sep="")
  page <- paste(page,author_id_url,citation_for_view_url, sep="&")
  print(page)
  print("https://scholar.google.com/citations?view_op=view_citation&hl=en&user=ulkW7fgAAAAJ&citation_for_view=ulkW7fgAAAAJ:u-x6o8ySG0sC")
  pub_page <- get_scholar_resp(page)
  pub_page <- read_html(pub_page)
  
  #We can access all article information from the divs with the class "gsc_oci_value"
  #Need to remember to add "." to the class or returns null results
  values <- html_nodes(resp,".gsc_oci_value")
  publication$author <- html_text(values[1])
  publication$date <- html_text(values[2])
  publication$journal <- html_text(values[3])
  publication$number <- html_text(values[4])
  return(publication)
}



for(i in 1:nrow(list)){
  
}


predict_h_index(id)