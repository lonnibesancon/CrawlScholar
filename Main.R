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

cleaned_publication_list <- get_publication_list(id)

new_pub_list <- curate_publication_list(cleaned_publication_list)

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


pub <- cleaned_publication_list[18,]


s1 <- "Besançon" #as read from an input file I cannot modify
s2 <- "Paris"
s3 <- "Linköping"
s4 <- "Besançon" #Manual input


s1b <- get_ascii_string(pub$author)
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





#Venue tests

publication <- new_pub_list[2,]
journal_portal <- "http://portal.core.edu.au/jnl-ranks/"
conference_portal <- "http://portal.core.edu.au/conf-ranks/"

publication <- new_pub_list[18,]

res <- get_journal_impact_factor(publication$venue)

test <- get_journal_impact_factor(publication$venue)
new_pub_list <- curate_publication_list(cleaned_publication_list)

res <- get_batch_journal_impact_factor(new_pub_list$venue)
for (i in 1:nrow(new_pub_list)){
  res <- get_journal_impact_factor(new_pub_list$venue[i])
  new_pub_list$impact_factor[i] <- res[2]
  new_pub_list$journal_found[i] <- res[1]
}

###### Test getting scholar metrics for a venue

venue <- "CHI"
result <- get_venue_scholar_metrics(venue, index_to_return = 62553,flush_cache = FALSE)

venues <- c("CHI","Transactions Visualization","Medical Research Methodology")
results <- get_batch_venue_scholar_metrics(venues) 

clean_venue_names <- c()
for (venue in new_pub_list$venue){
  print(venue)
  clean_venue_names <- append(clean_venue_names,clean_venue_for_scholar(venue,is_journal = FALSE))
}

candidates <- c()
venue <-new_pub_list$venue[42]
#test values
venue <- "Proceedings of the 29th Conference on l'Interaction Homme-Machine"
venue <- "Proceedings of 29th Conference on l'Interaction Homme-Machine"
venue <- "Proceedings 2nd Conference on l'Interaction Homme-Machine"
venue <- "Proc of the 29th Conference on l'Interaction Homme-Machine"
venue <- "Proc of 29th Conference on l'Interaction Homme-Machine"
venue <- "Proc 2nd Conference on l'Interaction Homme-Machine"
venue <-gsub("[[:digit:]]", replacement="", venue)
#Find the name of the conference if it is in between "Proceedings of the ... conference" or something similar
candidate <- str_match(tolower(venue), tolower("Proceedings of the\\s*(.*?)\\s*conference"))
print(candidate)
candidate <- candidate[,2]
#We check that the text between  "Proceedings of the ... conference" is not "1st" or "4th"
if(!any(candidate == c("st","nd","rd","th"))){
  candidates <- append(candidate)
}
candidate <- str_split(tolower(venue),tolower("conference"))
candidate <- candidate[[1]][2]
if(!is.null(candidate) || is.na(candidate)){
  candidates <- append(candidate)
}

get_venue_scholar_metrics(clean_venue_names[9])


#core test

url <- "http://portal.core.edu.au/conf-ranks/?search=VIS&by=all&source=CORE2021&sort=atitle&page=1"
resp <- httr::GET(url)
page_html <- read_html(resp)
tables <- as.data.frame(html_table(page_html))

tables[1,1]


#Get publication links and DOI:
id <- "ulkW7fgAAAAJ"
initial_list <- get_initial_publication_list(id)
initial_list[initial_list==""] <- NA

clean_pub_list <- get_publication_list(id)

link <- clean_pub_list$link[8]
doi <- get_doi_from_link(link)

doi_part <-str_split(link,"10\\.")
doi_part<-doi_part[[1]]

for(i in 1:nrow(clean_pub_list)){
  link <- clean_pub_list$link[i]
  print(link)
  doi <- get_doi_from_string(link)
  clean_pub_list$doi[i] <- doi
  print(doi)
  print("------------")
}

pubs_without_dois <- c()

for(i in 1:nrow(clean_pub_list)){
  if(is.na(clean_pub_list$doi[i])){
    print(paste("I  = ", i))
    link <- clean_pub_list$link[i]
    if(is.na(link)){
      next
    }
    if(grepl("osf.io",clean_pub_list$link[i])){
      clean_pub_list$doi[i] <- get_doi_from_osf(clean_pub_list$link[i])
    }
    else{
      
      resp <- httr::GET(link)
      if (httr::status_code(resp) != 200) {
        print(url)
        next
      }
      
      html_page <- read_html(resp)
      text_page <- html_text(html_page)
      doi_occurence <- get_dois_from_string(text_page)
      unique_list_of_dois <- table(doi_occurence)
      if(length(unique_list_of_dois)>1){
        #We need to disambiguate which ones occurs the most
        #TODO
      }
      else{
        clean_pub_list$doi[i] <- doi_occurence[1]
      }
    }
  }
}




link <- "https://scholar.google.fr/citations?view_op=view_citation&hl=en&user=ulkW7fgAAAAJ&cstart=20&pagesize=80&citation_for_view=ulkW7fgAAAAJ:ye4kPcJQO24C"
resp_parsed <- httr::GET(link)
resp_parsed <- read_html(resp_parsed)
articles <- html_nodes(resp_parsed,".gsc_oms_link")
#versions now contains the three links at the bottom of the scholar page
#We need to find, if any, the link to the "All N versions"

index_of_version <- find_index_of_versions(articles)

if(index_of_version!=-1){
  print(html_attr(articles[index_of_version],"href"))
}
else{
  return (NA)
}






if

publication$versions <- versions


link <- "https://osf.io/preprints/3z7kx/"
html_page <- read_html(link)
text_page <- html_text(html_page)
write(text_page,"test2.csv")

resp <- httr::POST(link)
html_page <- read_html(resp)
text_page <- html_text(html_page)
write(text_page,"test2.csv")

doi_occurence <- get_dois_from_string(text_page)


unique_list_of_dois

resp <- get_scholar_page("https://scholar.google.fr/citations?view_op=view_citation&hl=en&user=ulkW7fgAAAAJ&sortby=pubdate&citation_for_view=ulkW7fgAAAAJ:LjlpjdlvIbIC")
html_page <- read_html(resp)
link <- html_nodes(html_page,".gsc_oci_title_link")
if(length(link)==0){
  print("0")
}
link <- html_attr(link,"href")

test <- initial_list[30,]




#### Getting dois

#IEEE DOI location:
#   ""doi"":""10.1109/TVCG.2016.2599217""
#ACM DOI location
#   in the link on publication page
#BMC DOI location
#   in the link on publication page, arfter "article/"
#Science (AAS)
#   ""DOI"":""10.1126/science.abd9338""