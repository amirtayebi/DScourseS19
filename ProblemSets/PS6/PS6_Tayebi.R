rm(list=ls())

#Loading required packages
library(pvsR) 
library(effects)
library(vcd)


#setting th API code
pvs.key <- "c29553e173b3cf2762fc1607ea847f6c" 

# getting the list of all candidates for all the states
officials <- Officials.getStatewide(list("FL","OK","AL","AK","AZ","AR","CA","CT","DE","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","OH","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WI","WV","WY"))

# getting bills from US Congress in year 2012 for all the states
bills <- Votes.getBillsByYearState(year=2012, stateId=list("FL","OK","AL","AK","AZ","AR","CA","CT","DE","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","OH","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WI","WV","WY"))


# looking for the term  "Education" in the title column
bills <- bills[grep("Education", bills$title),] 


# getting details of the bill
bill <- Votes.getBill(bills$billId, separate="actions") 

# getting the actionId related to the passage of the bill
aId <- bill[["actions"]]$actionId[bill[["actions"]]$stage=="Passage"]
votes <- Votes.getBillActionVotes(actionId=aId)  

# removing absentees:
votes <- votes[votes$action=="Nay" | votes$action=="Yea",] 

# getting all the biographical data on all legislators acciciated with the bills
bio <- CandidateBio.getBio(candidateId=votes$candidateId) 


# merging the biographical data with the votes
leg_vote <- merge(votes, bio, by="candidateId") 

# intrudicing some binary variablesiables additional variables for statistical analysis
leg_vote$yes <- 0 
leg_vote$yes[leg_vote$action=="Yea"] <- 1 

leg_vote$Male <- 0 
leg_vote$Male[grep("Male", leg_vote$candidate.gender)]<-1 
leg_vote$Male[grep("Female", leg_vote$candidate.gender)]<-1 
leg_vote$Male <- as.factor(leg_vote$Male) 

leg_vote$Democratic <- 0 
leg_vote$Democratic[leg_vote$officeParties=="Democratic"] <- 1
leg_vote$Democratic <- as.factor(leg_vote$Democratic) 




# plotting the relationship between the gender of a legislator and their voting behavior
tab <- xtabs(~ Male + yes, data= leg_vote) 
p.tab <- prop.table(tab, 1) 
no <- unname(tab[,1]) 
yes <- unname(tab[,2]) 
tab2 <- t(data.frame("Yes"=yes, "No"=no, row.names=c("Female", "Male"))) 
tab2 <- round(tab2[,c("Male","Female")],2)
tab2 <- as.table(tab2)
names(dimnames(tab2)) <- c( "Vote", "Gender")
tab2 <- t(tab2)

par(mai=c(0.8,1,0.2,0.1))

mosaic(tab2, split_vertical=TRUE, shade=TRUE, gp=shading_max, gp_args=list(h=c(260,44.4), c=c(100,105), l=c(90,74) ), labeling=TRUE, pop=FALSE ) 
labeling_cells(text=tab2)(tab2) 


# plotting the relationship between the accociated party of a legislator and their voting behavior
tab <- xtabs(~ Democratic + yes, data= leg_vote) 
p.tab <- prop.table(tab, 1) 
no <- unname(tab[,1]) 
yes <- unname(tab[,2]) 
tab2 <- t(data.frame("Yes"=yes, "No"=no, row.names=c("Other", "Democratic"))) 
tab2 <- round(tab2[,c("Democratic","Other")],2)
tab2 <- as.table(tab2)
names(dimnames(tab2)) <- c( "Vote", "Party")
tab2 <- t(tab2)

par(mai=c(0.8,1,0.2,0.1))

mosaic(tab2, split_vertical=TRUE, shade=TRUE, gp=shading_max, gp_args=list(h=c(260,44.4), c=c(100,105), l=c(90,74) ), labeling=TRUE, pop=FALSE ) 
labeling_cells(text=tab2)(tab2) 



#Part B
rm(list=ls())

library(ggplot2)
library(maps)
load("C:/Users/Amir/Desktop/S3_3.1_gender_counties.RData")

#Mapping the map of the US
all_states <- map_data("state")

#merging the two datasets
Total <- merge(all_states, counties, by="region")
Total <- Total[Total$region!="district of columbia",]

#plotting the map
p <- ggplot()
p <- p + geom_polygon(data=Total, aes(x=long.x, y=lat.x, group = group.x , fill=Total$femaleshare.pvs),colour="white"
      ) + scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar")

P1 <- p + theme_bw()  + labs(fill = "Share Of Female Politicians" 
                             ,title = "Share Of Female Politicians", x="", y="")
P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())


