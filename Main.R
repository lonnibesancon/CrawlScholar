################################################################
# Main.R
# Code for to fetch data for Academic Hiring Visualization
# CC-BY Lonni Besançon et al., 2021
################################################################


debug <- 1


#Check that "likert" is installed
if(!require(scholar)){
  install.packages("scholar")
  library(scholar)
}

library(httr)
library(xml2)
library(rvest)
library(RCurl)
library(XML)

#Documentation for the scholar package is here: https://cran.r-project.org/web/packages/scholar/scholar.pdf

co_authors <- get_coauthors(cleaned_publication_list,id)

list <- read.csv("./list.csv")
list$id <- c()

length(list)


for(i in 1:nrow(list)){
  print(i)
  id <- get_scholar_id(last_name = list$family[i], first_name = list$first[i])
  list$id[i] <- id
}

id <- get_scholar_id(last_name = "Leyrat", first_name = "Clemence")

# Define the id for the researcher
id <- "oTHPKOoAAAAJ"

# Get his profile and print his name
l <- get_profile(id)
l$name 

publication_list <-get_publications(id)
cleaned_publication_list <- clean_publication_list(publication_list,id)

file_name <- paste("Data-",l$name,".csv", sep = "")
write.csv(cleaned_publication_list,file_name, row.names = FALSE)

# Get his citation history, i.e. citations to his work in a given year 
get_citation_history(id)

# Get his publications (a large data frame)
publication_list <-get_publications(id)

current_publication <- publication_list[1,]

for (i in 1:nrow(publication_list)){
  print(publication_list[i,])
}

clean_publication_data(current_publication)

resp <- get_scholar_resp("https://scholar.google.fr/citations?view_op=view_citation&hl=en&user=ulkW7fgAAAAJ&citation_for_view=ulkW7fgAAAAJ:u-x6o8ySG0sC")
#resp <- get_scholar_resp("https://scholar.google.fr/citations?user=ulkW7fgAAAAJ&hl=en")

co_authors <- c()

list <- strsplit(cleaned_publication_list$author[3],", ")
list <- strsplit(cleaned_publication_list$author[4],", ")
list <- list[[1]]

for (elem in list){
  if(elem!=l$name){
    co_authors <- append(co_authors,elem)
  }
  
}

get_coauthors()

co_authors
unique_list <- table(co_authors)

unique_list <- unique_list[!l$name]


co_authors <- table(publication_list$author)

get_full_list_coauthors <- function(){
  
}


predict_h_index(id)
