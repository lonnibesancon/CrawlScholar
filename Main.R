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

#Documentation for the scholar package is here: https://cran.r-project.org/web/packages/scholar/scholar.pdf


list <- read.csv("./list.csv")
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

for (i in 1:nrow(publication_list)){
  print(publication_list[i,])
}

clean_publication_data(current_publication)


library(httr)
library(xml2)
library(rvest)
library(RCurl)
library(XML)
resp <- get_scholar_resp("https://scholar.google.fr/citations?view_op=view_citation&hl=en&user=ulkW7fgAAAAJ&citation_for_view=ulkW7fgAAAAJ:u-x6o8ySG0sC")
resp <- get_scholar_resp("https://scholar.google.fr/citations?user=ulkW7fgAAAAJ&hl=en")


current_publication <- publication_list[1,]

publication_list <- clean_publication_list(publication_list,id)

clean_publication_data(publication_list[1,],id)

current_publication <- clean_publication_data(current_publication, id)






predict_h_index(id)
