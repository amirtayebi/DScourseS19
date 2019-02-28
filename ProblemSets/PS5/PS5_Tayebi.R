#Question 3

library('rvest')

webpage <- read_html("http://search.beaconforfreedom.org/search/censored_publications/result.html?author=&cauthor=&title=&country=7327&language=&censored_year=&censortype=&published_year=&censorreason=&Search=Search")

rank_data_html <- html_nodes(webpage,'tr+ tr td:nth-child(1)')

rank_data <- html_text(rank_data_html)
 
rank_data<-as.numeric(rank_data)
 
title_data_html <- html_nodes(webpage,'.censo_list font')

title_data <- html_text(title_data_html)
 
author_data_html <- html_nodes(webpage,'.censo_list+ td font')

author_data <- html_text(author_data_html)

country_data_html <- html_nodes(webpage,'.censo_list~ td:nth-child(4) font')
 
rcountry_data <- html_text(country_data_html)
 
year_data_html <- html_nodes(webpage,'tr+ tr td:nth-child(5) font')
 
year_data <- html_text(year_data_html)
 
type_data_html <- html_nodes(webpage,'tr+ tr td:nth-child(6) font')
 
type_data <- html_text(type_data_html)

 censorship_df<-data.frame(Rank = rank_data, Title = title_data, Author = author_data, Country = rcountry_data, Type = type_data, Year = year_data)

 write.table(censorship_df, file="Amir.csv",sep=",",row.names=F)

#Question4

# install the package from my computer
install.packages(C:\\pvsR_0.3.tar.gz, repos = NULL, type="source")# install the package from my computer

library(pvsR)

#set the API key
pvs.key <- "c29553e173b3cf2762fc1607ea847f6c" 

#get  biographical data for Obama
obama <- CandidateBio.getAddlBio(9490) 


#get  biographical data for Obama and Romney
or <- CandidateBio.getAddlBio(list(9490,21942)) 
X_Dataframe = as.data.frame(unlist(or))

#get more detailed biographical for Obama and Romney
bio <- CandidateBio.getDetailedBio(list(9490,21942))
X_Dataframe1 = as.data.frame(unlist(bio))
head(bio$profession)
head(bio$orgMembership)

# get a list of candidates for two specific elections
candidates <- Candidates.getByElection(list(2582,2646))
