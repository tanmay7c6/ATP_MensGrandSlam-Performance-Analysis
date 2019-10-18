#Exploratory Data Analysis 

# We have considered the Australian Open Tournament for EDA


#Data Wrangling 
#Final Data set has been created by merging all the raw data set into one.
#Few new attributes have been added like age which is crucial in determining the likelihood of reaching the finals.

install.packages("data.table")
library(data.table)
library(dplyr)
library(sqldf)
library(MVA)

data<-dataset_final
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

#Exploratory data analysis


#How the match time per set has increased over the years in the Australian tournament
setDT(AustralianOpen_Finalists_allstats)
View(AustralianOpen_Finalists_allstats)
summary(AustralianOpen_Finalists_allstats)
AustralianOpen_Finalists_allstats[is.na(AvgMinsPerSet),NROW(AvgMinsPerSet)]
is.na(AustralianOpen_Finalists_allstats)
AustralianOpen_Finalists_allstats[,'avgset_overyears':= mean(AvgMinsPerSet),by=Year]
ab<-unique(AustralianOpen_Finalists_allstats$avgset_overyears)
is.na(AustralianOpen_Finalists_allstats$avgset_overyears)
library(ggplot2)  
ggplot(AustralianOpen_Finalists_allstats,aes(x=Year,y=avgset_overyears))+geom_line()

#Box Plot for univariate analysis of various attributes

boxplot(AustralianOpen_Finalists_allstats$Age, main="Age Box plot",yaxt="n", xlab="Age", horizontal=TRUE)
boxplot(AustralianOpen_Finalists_allstats$Aces, main="Aces Box plot",yaxt="n", xlab="Aces", horizontal=TRUE)
boxplot(AustralianOpen_Finalists_allstats$Points, main="Points Box plot",yaxt="n", xlab="Points", horizontal=TRUE)
boxplot(AustralianOpen_Finalists_allstats$avgOdds, main="Odds Box plot",yaxt="n", xlab="Average Odds", horizontal=TRUE)
boxplot(AustralianOpen_Finalists_allstats$SP_Percent, main="Service Points Box plot",yaxt="n", xlab="Service Points", horizontal=TRUE)
boxplot(AustralianOpen_Finalists_allstats$RP_Percent, main="Return Points Percent Box plot",yaxt="n", xlab="Return Points", horizontal=TRUE)
boxplot(AustralianOpen_Finalists_allstats$BP_Win_Percentage, main="Break Points Win Percent Box plot",yaxt="n", xlab="Break Points Percent", horizontal=TRUE)
boxplot(AustralianOpen_Finalists_allstats$firstServeReturnsWon, main="First Serve Returns Won Box plot",yaxt="n", xlab="First Serve Returns Won", horizontal=TRUE)
boxplot(AustralianOpen_Finalists_allstats$SecondServeReturnsWon, main="Second Serve Returns Won Box plot",yaxt="n", xlab="Second Serce Returns Won", horizontal=TRUE)
boxplot(AustralianOpen_Finalists_allstats$FirstServePercentage, main="First Serve Percentage Box plot",yaxt="n", xlab="First Serve Percentage", horizontal=TRUE)
boxplot(AustralianOpen_Finalists_allstats$TotalMatchMins, main="Duration Box plot",yaxt="n", xlab="Duration", horizontal=TRUE)

#scatterplot 3D
#install.packages("scatterplot3d")

library(scatterplot3d)

Sd3 <- scatterplot3d(AustralianOpen_Finalists_allstats$TotalMatchMins,AustralianOpen_Finalists_allstats$Aces,AustralianOpen_Finalists_allstats$DoubleFaults,xlab="Match Duration", ylab="Aces", angle=45,zlab="DoubleFaults", lty.hide=2,type="h",y.margin.add=0.1,font.axis=2,font.lab=2)


#Diagonal plot 
library(SciViews)
pairs(AustralianOpen_Finalists_allstats_Numeric, diag.panel = panel.boxplot, labels=c("Age","Rank","Odds","Service Points","Return Points","BP Win Percentage","Aces","FS Returns Won","SS Returns Won","FS In","Double Faults","FS Percentage"),pch=c(1,16), font.labels=2)


#Bivariate Analysis

mlab="Age"
plab="Aces"
Match_Aces_Age=data.frame(AustralianOpen_Finalists_allstats$Aces, AustralianOpen_Finalists_allstats$Age)
bvbox(Match_Aces_Age, mtitle = "", xlab = mlab, ylab = plab)


mlab="Rank"
plab="Points"
Match_Rank_Points=data.frame(AustralianOpen_Finalists_allstats$Rank, AustralianOpen_Finalists_allstats$Points)
bvbox(Match_Rank_Points, mtitle = "", xlab = mlab, ylab = plab)

plot(AustralianOpen_Finalists_allstats$Points, AustralianOpen_Finalists_allstats$TotalMatchMins,xlab="ATP points",ylab="Match Duration")
plot(AustralianOpen_Finalists_allstats$SP_Percent, AustralianOpen_Finalists_allstats$RP_Percent,xlab="Service Points", ylab="Return POints")



#Chiplot used to figure out the dependence in the multivariate analysis

mlab = "Aces"
plab = "Match Duration"
with(AustralianOpen_Finalists_allstats, plot(Aces, TotalMatchMins, xlab = mlab , ylab = plab, cex.lab = 0.9))
with(AustralianOpen_Finalists_allstats, chiplot(Aces, TotalMatchMins))

mlab = "Average Odds"
plab = "Points"
with(AustralianOpen_Finalists_allstats, plot(avgOdds, Points, xlab = mlab , ylab = plab, cex.lab = 0.9))
with(AustralianOpen_Finalists_allstats, chiplot(avgOdds, Points))


mlab = "Service Points"
plab = "Return Points"
with(AustralianOpen_Finalists_allstats, plot(SP_Percent, RP_Percent, xlab = mlab , ylab = plab, cex.lab = 0.9))
with(AustralianOpen_Finalists_allstats, chiplot(SP_Percent, RP_Percent))


mlab = "First Serve Returns Won"
plab = "Second Serve Returns Won"
with(AustralianOpen_Finalists_allstats, plot(firstServeReturnsWon, SecondServeReturnsWon, xlab = mlab , ylab = plab, cex.lab = 0.9))
with(AustralianOpen_Finalists_allstats, chiplot(firstServeReturnsWon, SecondServeReturnsWon))


mlab = "First Serve In"
plab = "Double Faults"
with(AustralianOpen_Finalists_allstats, plot(FirstServesIn, DoubleFaults, xlab = mlab , ylab = plab, cex.lab = 0.9))
with(AustralianOpen_Finalists_allstats, chiplot(FirstServesIn, DoubleFaults))

mlab = "First Serve In"
plab = "Match Duration"
with(AustralianOpen_Finalists_allstats, plot(FirstServesIn, TotalMatchMins, xlab = mlab , ylab = plab, cex.lab = 0.9))
with(AustralianOpen_Finalists_allstats, chiplot(FirstServesIn, TotalMatchMins))



library(ggplot2)

ggplot(AustralianOpen_Finalists_allstats,aes(x=AustralianOpen_Finalists_allstats$Age,fill=AustralianOpen_Finalists_allstats$Round)) + geom_bar() +
  labs(y= "Count", x="Age", title = "Age")

ggplot(AustralianOpen_Finalists_allstats,aes(x=AustralianOpen_Finalists_allstats$TotalMatchMins,fill=AustralianOpen_Finalists_allstats$Round)) + geom_bar() +
  labs(y= "Count", x="Match Duration", title = "Match Duration")

ggplot(AustralianOpen_Finalists_allstats,aes(x=AustralianOpen_Finalists_allstats$Aces,fill=AustralianOpen_Finalists_allstats$Round)) + geom_bar() +
  labs(y= "Count", x="Aces", title = "Aces")


#correlation analysis
AustralianOpen_Finalists_allstats_Numeric<-subset(AustralianOpen_Finalists_allstats,select = c("Age","Rank","avgOdds","SP_Percent","RP_Percent","BP_Win_Percentage","Aces","firstServeReturnsWon","SecondServeReturnsWon","FirstServesIn","DoubleFaults","FirstServePercentage"))
pairs(AustralianOpen_Finalists_allstats_Numeric, labels=c("Age","Rank","Odds","Service Points","Return Points","BP Win Percentage","Aces","FS Returns Won","SS Returns Won","FS In","Double Faults","FS Percentage"),pch=c(1,16),font.labels=2)


