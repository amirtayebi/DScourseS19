 library('rvest')
 library(tidyverse)
 genius_urls <- paste0(("http://search.beaconforfreedom.org/search/censored_publications/result.html?author=&cauthor=&title=&country=7327&language=&censored_year=&censortype=&published_year=&censorreason=&sort=t&page=, i"))

final <- tibble()

 for (i in 1:40) {

genius_page <- read_html(genius_urls[i])

 rank_data_html <- html_nodes(genius_page,'tr+ tr td:nth-child(1)')

 rank_data <- html_text(rank_data_html)
 
 rank_data<-as.numeric(rank_data)
 
 title_data_html <- html_nodes(genius_page,'.censo_list font')

 title_data <- html_text(title_data_html)
 
 author_data_html <- html_nodes(genius_page,'.censo_list+ td font')
 author_data <- html_text(author_data_html)
 
 country_data_html <- html_nodes(genius_page,'.censo_list~ td:nth-child(4) font')
 
 rcountry_data <- html_text(country_data_html)
 
 year_data_html <- html_nodes(genius_page,'tr+ tr td:nth-child(5) font')
 
 year_data <- html_text(year_data_html)
 
 type_data_html <- html_nodes(genius_page,'tr+ tr td:nth-child(6) font')
 
 type_data <- html_text(type_data_html)
final <- rbind(final, tibble(Rank = rank_data, Title = title_data, Author = author_data, Country = rcountry_data, Type = type_data, Year = year_data ))
 
 }

 censorship_df<-data.frame(Rank = rank_data, Title = title_data, Author = author_data, Country = rcountry_data, Type = type_data, Year = year_data)

 write.table(censorship_df, file="Amir.csv",sep=",",row.names=F)