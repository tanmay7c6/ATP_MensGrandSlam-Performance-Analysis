#K means Clustering 

library(data.table)
library(dplyr)
library(sqldf)
library(MVA)


data<-read.csv("C:/Users/Siri/Downloads/dataset_final.csv", stringsAsFactors=FALSE)
View(data)

#Australian Tournament 
AustralianOpen<-subset(data,data$Tournament=="Australian Open")
View(AustralianOpen)

AustralianOpen_Finalists<-subset(AustralianOpen,AustralianOpen$Round=='The Final')
View(AustralianOpen_Finalists)
AustralianOpen_Finalists<-subset(AustralianOpen_Finalists,select = c("PlayerName","Year"))
AustralianOpen_Finalists_allstats<-merge(x=AustralianOpen,y=AustralianOpen_Finalists, by=c("PlayerName","Year"))
View(AustralianOpen_Finalists_allstats)
setDT(AustralianOpen_Finalists_allstats)

#Win percentage calculation for finalists

total_matches=AustralianOpen_Finalists_allstats%>%
  group_by(PlayerName,Year)%>%
  summarize(total_matchs=n())
View(total_matches)


total_matches_Won=AustralianOpen_Finalists_allstats%>%
  filter(Winner=='TRUE')%>%
  group_by(PlayerName,Year)%>%
  summarise(total_matches_won=n()) 
View(total_matches_Won)

setDT(total_matches)
setDT(total_matches_Won)
total_matches$winpercentage=(winpercentage=(total_matches_Won$total_matches_won)/(total_matches$total_matchs))


#merging the win percentage in finalists data 
AustralianOpen_Finalists_allstats=merge(x=total_matches,y=AustralianOpen_Finalists_allstats, by=c("PlayerName","Year"))
View(AustralianOpen_Finalists_allstats)

#Factor Analysis  Analysis

head(AustralianOpen_Finalists_allstats)

str(AustralianOpen_Finalists_allstats)

summary(AustralianOpen_Finalists_allstats)

AustralianOpen_Finalists_allstats_Numeric<-subset(AustralianOpen_Finalists_allstats,select = c("Age","Rank","avgOdds","SP_Percent","RP_Percent","BP_Win_Percentage","Aces","firstServeReturnsWon","SecondServeReturnsWon","FirstServesIn","DoubleFaults","FirstServePercentage"))#Factor Analysis
View(AustralianOpen_Finalists_allstats_Numeric)

#K-Means Clustering

AustralianOpen_Finalists_allstats_Numeric_scale<-scale(AustralianOpen_Finalists_allstats_Numeric)
# K-means, k=2, 3, 4, 5, 6
# Centers (k's) are numbers thus, 10 random sets are chosen

(kmeans2<-kmeans(AustralianOpen_Finalists_allstats_Numeric,2,nstart = 10))

# Computing the percentage of variation accounted for Two clusters
perc.var.2 <- round(100*(1 - kmeans2$betweenss/kmeans2$totss),1)
names(perc.var.2) <- "Perc. 2 clus"
perc.var.2


# Computing the percentage of variation accounted for three clusters
(kmeans3<-kmeans(AustralianOpen_Finalists_allstats_Numeric,3,nstart = 10))
perc.var.3 <- round(100*(1 - kmeans3$betweenss/kmeans3$totss),1)
names(perc.var.3) <- "Perc. 3 clus"
perc.var.3

# Computing the percentage of variation accounted for three clusters
(kmeans4<-kmeans(AustralianOpen_Finalists_allstats_Numeric,4,nstart = 10))
perc.var.4 <- round(100*(1 - kmeans4$betweenss/kmeans4$totss),1)
names(perc.var.4) <- "Perc. 4 clus"
perc.var.4

## Computing the percentage of variation accounted for three clusters
(kmeans5<-kmeans(AustralianOpen_Finalists_allstats_Numeric,5,nstart = 10))
perc.var.5 <- round(100*(1 - kmeans5$betweenss/kmeans5$totss),1)
names(perc.var.5) <- "Perc. 5 clus"
perc.var.5

## Computing the percentage of variation accounted for three clusters
(kmeans6<-kmeans(AustralianOpen_Finalists_allstats_Numeric,6,nstart = 10))
perc.var.6<- round(100*(1 - kmeans6$betweenss/kmeans6$totss),1)
names(perc.var.6) <- "Perc. 6 clus"
perc.var.6

## Computing the percentage of variation accounted for three clusters
(kmeans9<-kmeans(AustralianOpen_Finalists_allstats_Numeric,9,nstart = 10))
perc.var.9<- round(100*(1 - kmeans9$betweenss/kmeans9$totss),1)
names(perc.var.9) <- "Perc. 9 clus"
perc.var.9

AustralianOpen_Finalists_allstats_Numeric_Scale<-scale(AustralianOpen_Finalists_allstats_Numeric)


k.max<-18
wss<-sapply(1:k.max,function(k){kmeans(AustralianOpen_Finalists_allstats_Numeric_Scale,k,nstart=50)$tot.withinss})

plot(1:k.max,wss, type='b',pch =19,frame = FALSE, xlab ="Number Of clusters k",ylab ="Total within cluster  sum of squares")
abline(v=3, lty=2)

plot(AustralianOpen_Finalists_allstats_Numeric$SP_Percent,AustralianOpen_Finalists_allstats_Numeric$BP_Win_Percentage,col=(kmeans9$cluster+1),main="K-means Clustering Results with K=9",pch=20,cex=2)

plot(AustralianOpen_Finalists_allstats_Numeric$avgOdds,AustralianOpen_Finalists_allstats_Numeric$Age,col=(kmeans9$cluster+1),main="K-means Clustering Results with K=9",pch=20,cex=2)

plot(AustralianOpen_Finalists_allstats_Numeric$Age,AustralianOpen_Finalists_allstats_Numeric$Rank,col=(kmeans9$cluster+1),main="K-means Clustering Results with K=9",pch=20,cex=2)
