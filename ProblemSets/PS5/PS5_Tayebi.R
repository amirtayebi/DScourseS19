#Question 3

> library('rvest')

> webpage <- read_html("http://search.beaconforfreedom.org/search/censored_publications/result.html?author=&cauthor=&title=&country=7327&language=&censored_year=&censortype=&published_year=&censorreason=&Search=Search")

> rank_data_html <- html_nodes(webpage,'tr+ tr td:nth-child(1)')

> rank_data <- html_text(rank_data_html)
 
> rank_data<-as.numeric(rank_data)
 
> title_data_html <- html_nodes(webpage,'.censo_list font')

> title_data <- html_text(title_data_html)
 
> author_data_html <- html_nodes(webpage,'.censo_list+ td font')
> author_data <- html_text(author_data_html)
 
> country_data_html <- html_nodes(webpage,'.censo_list~ td:nth-child(4) font')
 
> rcountry_data <- html_text(country_data_html)
 
> year_data_html <- html_nodes(webpage,'tr+ tr td:nth-child(5) font')
 
> year_data <- html_text(year_data_html)
 
> type_data_html <- html_nodes(webpage,'tr+ tr td:nth-child(6) font')
 
> type_data <- html_text(type_data_html)

> censorship_df<-data.frame(Rank = rank_data, Title = title_data, Author = author_data, Country = rcountry_data, Type = type_data, Year = year_data)

> write.table(censorship_df, file="Amir.csv",sep=",",row.names=F)

#Question4
library(Quandl)
Amir'sData1 = Quandl.datatable("ZACKS/FC", ticker="AAPL")
Amir'sData2 = Quandl.datatable("ZACKS/FC", ticker="AAPL", paginate=T)
Amir'sData3 = Quandl.datatable("ZACKS/FC", ticker=c("AAPL", "MSFT"), qopts.columns=c("ticker", "per_end_date"))
Amir'sData4 = Quandl.datatable("ZACKS/FC", ticker=c("AAPL", "MSFT"), per_end_date.gt="2015-01-01",qopts.columns=c("m_ticker", "per_end_date", "tot_revnu"))
