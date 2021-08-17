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
library(stringdist)
library(stringi)
library(stringr)

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
ids <- c("ulkW7fgAAAAJ","rEf29d0AAAAJ","megudlUAAAAJ","lwSLxVgAAAAJ") #Lonni,Arnaud,Evanthia,Ignacio
id <- "PMZ3h7sAAAAJ" #JDF to check for max number of papers

# Get his profile and print his name
scholar <- get_scholar_profile(ids[4])
scholar_name <- scholar$name 

cleaned_publication_list <- get_publication_list(ids[2])
cleaned_publication_list <- get_dois_for_publications(cleaned_publication_list)


#Venue name cleaning for HCI/VIS people
for(i in 1:nrow(cleaned_publication_list)){
  venue <- cleaned_publication_list$venue[i]
  if(grepl("Vis. Comput.",venue) || grepl("TVCG",venue) || grepl("Visualization and Comp",venue) || grepl("IEEE VIS",venue)){
    venue <- "IEEE transactions on visualization and computer graphics"
  }
  if(grepl("Conference on Human Factors in Computing Systems",venue) || grepl("CHI",venue)){
    venue <- "Computer Human Interaction (CHI)"
  }
  if(grepl("IHM",venue) || grepl("Interaction Homme",venue)){
    venue <- "Conference on l'Interaction Homme-Machine (IHM)"
  }
  if(grepl("EuroVis Posters",venue)){
    venue <- "Eurographics/IEEE VGTC Conference on Visualization: Posters"
  }
  if(grepl("International Symposium on Mixed and Augmented Reality",venue) || grepl("ISMAR",venue)){
    venue <- "International Symposium on Mixed and Augmented Reality"
  }
  cleaned_publication_list$venue[i] <- venue
}

#Get Journals IFs
for(i in 1:nrow(cleaned_publication_list)){
  print(paste0("I = ", i))
  venue <- cleaned_publication_list$venue[i]
  IF <- get_journal_impact_factor(venue)[2]
  cleaned_publication_list$IF[i] <- IF
  print(IF)
}

#GetScholarMetrics
for(i in 1:nrow(cleaned_publication_list)){
  print(paste0("I = ",i))
  venue <- cleaned_publication_list$venue[i]
  result <- get_venue_scholar_metrics(venue)
  if(length(result)!=0){
    cleaned_publication_list$h5.index[i] <-  result$h5.index
    cleaned_publication_list$h5.median[i] <-  result$h5.median
  }
  else{
    cleaned_publication_list$h5.index[i] <-  NA
    cleaned_publication_list$h5.median[i] <-  NA
  }
}

#For the CORE ranking we need to change the venue again
for(i in 1:nrow(cleaned_publication_list)){
  print(paste0("I = ",i))
  venue <- cleaned_publication_list$venue[i]
  if(!is.na(venue) && ("Computer Human Interaction (CHI)" == venue)){
    venue <- "International Conference on Human Factors in Computing Systems"
  }
  cleaned_publication_list$venue[i] <- venue
}

#Get CORE Ranking
for(i in 1:nrow(cleaned_publication_list)){
  print(paste0("I = ",i))
  venue <- cleaned_publication_list$venue[i]
  result <- get_core_ranking_venue(venue,is_journal = TRUE)
  if(is.na(result)){
    result <- get_core_ranking_venue(venue, is_journal = FALSE)
    if(is.na(result)){
      cleaned_publication_list$core_ranking[i] <- NA
    }
    else{
      cleaned_publication_list$core_ranking[i] <- result$Rank
    }
  }
  else{
    cleaned_publication_list$core_ranking[i] <- result$Rank
  }
}

write.csv(cleaned_publication_list,"Ignacio.csv")
