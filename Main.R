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
#resp <- get_scholar_resp("https://scholar.google.fr/citations?user=ulkW7fgAAAAJ&hl=en")


current_publication <- publication_list[1,]


publication_list <-get_publications(id)
publication_list <- publication_list[c(5,7),]
publication_list <- clean_publication_list(publication_list,id)

clean_publication_data(publication_list[1,],id)

current_publication <- clean_publication_data(current_publication, id)

current_publication <- publication_list[48,]

publication_list <-get_publications(id)
publication_list <- curate_publication_list(publication_list)
publication_list <- clean_publication_list(publication_list,id)


resp <- get_scholar_resp("https://scholar.google.fr/citations?view_op=view_citation&hl=en&user=ulkW7fgAAAAJ&sortby=pubdate&citation_for_view=ulkW7fgAAAAJ:eMMeJKvmdy0C")
if (is.null(resp)){
  errorMessage <- paste("The scholar page for this publication is empty\n The function tried to fetch the following page:\n'",pub_page,"'",sep="")
  stop(errorMessage)
} 
resp_parsed <- read_html(resp)

citation_history <- fetch_publication_citation_history(resp_parsed)

current_publication<-publication_list[1,]
years <- html_nodes(resp_parsed,".gsc_oci_g_t")
cites_per_year <- html_nodes(resp_parsed,".gsc_oci_g_al")
years <- html_text(years)
cites_per_year <- html_text(cites_per_year)
citation_history <- years
citation_history <- vector(mode="list", length=length(years))
names(citation_history) <- years
citation_history<- ""
for (i in 1:length(years)){
  year_info <- paste(years[i],":",cites_per_year[i], sep="")
  citation_history <- paste(citation_history,year_info, sep=";")
}

#We remove the first ";" at the begining of the string
sub('.', '', citation_history)



current_publication$citation_history <- citation_history

#citation_history <- do.call(rbind, Map(data.frame, years=years, cites=cites_per_year))

current_publication$citation_history <- citation_history

if(!is.null(current_publication$cites)){
  years <- html_nodes(resp_parsed,".gsc_oci_g_t")
  years <- html_text(years)
  length(years)
}



#We can access all article information from the divs with the class "gsc_oci_value"
#Need to remember to add "." to the class or returns null results
values <- html_nodes(resp_parsed,".gsc_oci_value")
fields <- html_nodes(resp_parsed,".gsc_oci_field")
fields <- html_text(fields)

index <- find_venue_index(fields)


fields_list <- c("Authors","Publication date","Journal","Volume","Issue")

fields <- html_text(fields)

index <- find_field_index("Book",fields)

publication$author <- html_text(values[1])
publication$date <- html_text(values[2])
publication$journal <- html_text(values[3])
publication$number <- html_text(values[4])


predict_h_index(id)
