################################################################
# Main.R
# Code to fetch data for Academic Hiring Visualization
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
library(R.cache)

#Documentation for the scholar package is here: https://cran.r-project.org/web/packages/scholar/scholar.pdf


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
id <- "ulkW7fgAAAAJ"
id <- "PMZ3h7sAAAAJ" #JDF to check for max number of papers

# Get his profile and print his name
scholar <- get_scholar_profile(id)
scholar_name <- scholar$name 

publication_list <- get_publications(id)
cleaned_publication_list <- clean_publication_list(publication_list,id)

file_name <- paste("Data-",scholar_name,".csv",sep="")
write.csv(cleaned_publication_list,file_name)

url <- "https://scholar.google.fr/citations?view_op=view_citation&hl=en&user=ulkW7fgAAAAJ&citation_for_view=ulkW7fgAAAAJ:u-x6o8ySG0sC"
result <- get_scholar_page(url)
cache_info(result)

current_publication <- cleaned_publication_list[18,]
authors <- strsplit(current_publication$author,", ")
authors <- authors[[1]]
authors
for (i in  1:length(authors)){
  authors[i] <- get_ascii_string(authors[i])
}
authors
scholar_name <- iconv(scholar_name, to='ASCII//TRANSLIT')
position <- match(scholar_name,authors)

#We add author position for the scholar that we are interested in
#There might special characters automatically left out in the publication metadata
#So we look for ASCII equivalences
for (i in  1:length(authors)){
  print("coucou")
  authors[i] <- iconv(authors[i], to='ASCII//TRANSLIT')
}
scholar_name <- iconv(scholar_name, to='ASCII//TRANSLIT')
position <- match(scholar_name,authors)
position

#Finally we get the citation history of the publication
citation_history <- fetch_publication_citation_history(resp_parsed)

publication$citation_history <- citation_history


s1 <- "Besançon" #as read from an input file I cannot modify
s2 <- "Paris"
s3 <- "Linköping"
s4 <- "Besançon" #Manual input


s1b <- get_ascii_string(s1)
s1b

latin=iconv(s1,'utf8','latin1')

s1 <- iconv(s1, to='ASCII//TRANSLIT', sub='')
s2 <- iconv(s2, to='ASCII//TRANSLIT')
s3 <- iconv(s3, to='ASCII//TRANSLIT')
s4 <- iconv(s4, to='ASCII//TRANSLIT')


s1 <- "Besançon" #as read from an input file I cannot modify
s2 <- "Paris"
s3 <- "Linköping"
s4 <- "Besançon" #Manual input

s1 <- iconv(s1, to='ASCII//TRANSLIT')
s2 <- iconv(s2, to='ASCII//TRANSLIT')
s3 <- iconv(s3, to='ASCII//TRANSLIT')
s4 <- iconv(s4, to='ASCII//TRANSLIT')

utf_16<-iconv(s1,'utf8','utf-16')


position <- match(scholar_name,authors)
if (is.na(position)){
  scholar_name <- iconv(scholar_name, to='ASCII//TRANSLIT')
  position <- match(scholar_name,authors)
}
publication$position <- position

url <- "https://dl.acm.org/doi/abs/10.1145/2807442.2807488"
respt <- httr::GET(url)
page_html <- read_html(respt)

#<div _ngcontent-qdl-c29="" class="u-pb-1 stats-document-abstract-doi"><strong _ngcontent-qdl-c29="">DOI: </strong><!----><a _ngcontent-qdl-c29="" append-to-href="?src=document" target="_blank" href="https://doi.org/10.1109/VAST.2010.5652896">10.1109/VAST.2010.5652896</a><!----></div>

  
doi <- html_nodes(page_html,"document-main-left-trail-content")
doi 
page_text <- as.character(page_html)

write(page_text,"test2.csv")


