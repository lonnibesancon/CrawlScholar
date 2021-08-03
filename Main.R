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
scholar <- get_profile(id)
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

s1 <- iconv(s1, to='ASCII//TRANSLIT', sub='/')
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

s1
s2
s3
s4


position <- match(scholar_name,authors)
if (is.na(position)){
  scholar_name <- iconv(scholar_name, to='ASCII//TRANSLIT')
  position <- match(scholar_name,authors)
}
publication$position <- position







# Get profile infor

url <- "https://scholar.google.com/citations?user=ulkW7fgAAAAJ&hl=en"
page <- get_scholar_page(url) 
html_page <- read_html(page)
tables <- html_table(html_page)

# Citation data is in tables[[1]]
stats <- tables[[1]]
rows <- nrow(stats)

year <- names(stats[3])
year <- strsplit(year," ")[[1]][2]

stats[[1]][1]

name <- html_text(html_nodes(html_page,"#gsc_prf_in"))
infos <- html_text(html_nodes(html_page,".gsc_prf_il"))
infos <- strsplit(infos[1],",")
infos <- infos[[1]]
position <- infos[1]
affiliation <- infos[2]

scholar <-

scholar$id <- id
scholar$name <- name
scholar$affiliation <- affiliation
scholar$position <-position


#id <- "ulkW7fgAAAAJ"
id <- "PMZ3h7sAAAAJ"
s <- get_scholar_profile(id)
publication <- get_publications(s$id)

publication_list<-get_publications(id, flush_cache = TRUE)

url <- compose_scholar_url(id)
url <- "https://scholar.google.com/citations?hl=en&user=PMZ3h7sAAAAJ"
url <- "https://scholar.google.com/citations?user=PMZ3h7sAAAAJ&cstart=0&pagesize=100"
page_html <- read_html(get_scholar_page(url))
publications <- html_nodes(page_html,".gsc_a_at")


#To recover the publication ID we extract it from the link in the page
links <- html_attr(publications,"href")

#The publication_id is located in the URL after "citation_for_view"
for(i in 1:length(links)){
  publication_ids[i] <- strsplit(links[i],"for_view=")[[1]][2]
  #The complete publication_id also contains the scholar_id
  #It is needed to construct a valid URL, but we still want to separate these two
  #They are separated by ":"
  publication_ids[i] <- strsplit(publication_ids[i],":")[[1]][2]
}

publication_ids <- strsplit(publication_ids,":")[[1]][2]

#Total citation information is located in a class="gsc_a_ac gs_ibl"
citations <- html_text(html_nodes(page_html,".gsc_a_ac.gs_ibl"))
#Year of publication is located in  a class="gsc_a_h gsc_a_hc gs_ibl"
years <- html_text(html_nodes(page_html,".gsc_a_h.gsc_a_hc.gs_ibl"))
#Title information is located in a class="gsc_a_at"
titles <- html_text(html_nodes(page_html,".gsc_a_at"))

publication_list <- data.frame(year=years,title=titles,citation=citations,publication_id=publication_ids)

button <- html_nodes(page_html,"#gsc_bpf_more")
as.character(button)

#If the "show more" button is disabled, we don't have any more publications to parse
is_button_disabled <- grepl("disabled", as.character(button), fixed = TRUE)
if(!is_button_disabled){
  
}


