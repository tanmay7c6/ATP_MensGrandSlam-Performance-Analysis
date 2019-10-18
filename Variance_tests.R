#Variance Test analysis

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


View(AustralianOpen_Finalists_allstats_Numeric)

str(AustralianOpen_Finalists_allstats_Numeric)

qqnorm(AustralianOpen_Finalists_allstats_Numeric[,"Age"], main = "Age")
qqline(AustralianOpen_Finalists_allstats_Numeric[,"Age"])


qqnorm(AustralianOpen_Finalists_allstats_Numeric[,"avgOdds"], main = "avgOdds")
qqline(AustralianOpen_Finalists_allstats_Numeric[,"avgOdds"])


qqnorm(AustralianOpen_Finalists_allstats_Numeric[,"SP_Percent"], main = "SP_Percent")
qqline(AustralianOpen_Finalists_allstats_Numeric[,"SP_Percent"])

qqnorm(AustralianOpen_Finalists_allstats_Numeric[,"RP_Percent"], main = "RP_Percent")
qqline(AustralianOpen_Finalists_allstats_Numeric[,"RP_Percent"])

qqnorm(AustralianOpen_Finalists_allstats_Numeric[,"BP_Win_Percentage"], main = "BP_Win_Percentage")
qqline(AustralianOpen_Finalists_allstats_Numeric[,"BP_Win_Percentage"])

qqnorm(AustralianOpen_Finalists_allstats_Numeric[,"Aces"], main = "Aces")
qqline(AustralianOpen_Finalists_allstats_Numeric[,"Aces"])

qqnorm(AustralianOpen_Finalists_allstats_Numeric[,"firstServeReturnsWon"], main = "firstServeReturnsWon")
qqline(AustralianOpen_Finalists_allstats_Numeric[,"firstServeReturnsWon"])

qqnorm(AustralianOpen_Finalists_allstats_Numeric[,"SecondServeReturnsWon"], main = "SecondServeReturnsWon")
qqline(AustralianOpen_Finalists_allstats_Numeric[,"SecondServeReturnsWon"])

qqnorm(AustralianOpen_Finalists_allstats_Numeric[,"FirstServesIn"], main = "FirstServesIn")
qqline(AustralianOpen_Finalists_allstats_Numeric[,"FirstServesIn"])

qqnorm(AustralianOpen_Finalists_allstats_Numeric[,"DoubleFaults"], main = "DoubleFaults")
qqline(AustralianOpen_Finalists_allstats_Numeric[,"DoubleFaults"])

qqnorm(AustralianOpen_Finalists_allstats_Numeric[,"FirstServePercentage"], main = "FirstServePercentage")
qqline(AustralianOpen_Finalists_allstats_Numeric[,"FirstServePercentage"])

#individually they had outliers 
#all of them together or how they interact with each other 
#they look they are normally multivariate
plot(qchisq((1:nrow(AustralianOpen_Finalists_allstats_Numeric) - 1/2) / nrow(AustralianOpen_Finalists_allstats_Numeric), df = 3), sort(d),
     xlab = expression(paste(chi[3]^2, " Quantile")),
     ylab = "Ordered distances")
abline(a = 0, b = 1)


#t Tests
t.test(AustralianOpen_Finalists_allstats$Age[AustralianOpen_Finalists_allstats$Winner=="TRUE"],AustralianOpen_Finalists_allstats$Age[AustralianOpen_Finalists_allstats$Winner=='FALSE'],var.equal=TRUE)
#Not significant

t.test(AustralianOpen_Finalists_allstats$Rank[AustralianOpen_Finalists_allstats$Winner=='FALSE'],AustralianOpen_Finalists_allstats$Rank[AustralianOpen_Finalists_allstats$Winner=="TRUE"],var.equal=TRUE)
#Not Significant

t.test(AustralianOpen_Finalists_allstats$avgOdds[AustralianOpen_Finalists_allstats$Winner=='TRUE'],AustralianOpen_Finalists_allstats$avgOdds[AustralianOpen_Finalists_allstats$Winner=='FALSE'],var.equal=TRUE)
#Significant

t.test(AustralianOpen_Finalists_allstats$SP_Percent[AustralianOpen_Finalists_allstats$Winner=='TRUE'],AustralianOpen_Finalists_allstats$SP_Percent[AustralianOpen_Finalists_allstats$Winner=='FALSE'],var.equal=TRUE)
#significant

t.test(AustralianOpen_Finalists_allstats$RP_Percent [AustralianOpen_Finalists_allstats$Winner=='TRUE'],AustralianOpen_Finalists_allstats$RP_Percent [AustralianOpen_Finalists_allstats$Winner=='FALSE'],var.equal=TRUE)
#signficant 

t.test(AustralianOpen_Finalists_allstats$BP_Win_Percentage[AustralianOpen_Finalists_allstats$Winner=='TRUE'],AustralianOpen_Finalists_allstats$BP_Win_Percentage[AustralianOpen_Finalists_allstats$Winner=='FALSE'],var.equal=TRUE)
# NOT significant

t.test(AustralianOpen_Finalists_allstats$Aces[AustralianOpen_Finalists_allstats$Winner=='TRUE'],AustralianOpen_Finalists_allstats$Aces[AustralianOpen_Finalists_allstats$Winner=='FALSE'],var.equal=TRUE)
# Not significant 

t.test(AustralianOpen_Finalists_allstats$firstServeReturnsWon[AustralianOpen_Finalists_allstats$Winner=='TRUE'],AustralianOpen_Finalists_allstats$firstServeReturnsWon[AustralianOpen_Finalists_allstats$Winner=='FALSE'],var.equal=TRUE)
# Not significant

t.test(AustralianOpen_Finalists_allstats$SecondServeReturnsWon[AustralianOpen_Finalists_allstats$Winner=='TRUE'],AustralianOpen_Finalists_allstats$SecondServeReturnsWon[AustralianOpen_Finalists_allstats$Winner=='FALSE'],var.equal=TRUE)
# signficant 

t.test(AustralianOpen_Finalists_allstats$FirstServesIn[AustralianOpen_Finalists_allstats$Winner=='TRUE'],AustralianOpen_Finalists_allstats$FirstServesIn[AustralianOpen_Finalists_allstats$Winner=='FALSE'],var.equal=TRUE)
#Significant 

t.test(AustralianOpen_Finalists_allstats$DoubleFaults[AustralianOpen_Finalists_allstats$Winner=='TRUE'],AustralianOpen_Finalists_allstats$DoubleFaults[AustralianOpen_Finalists_allstats$Winner=='FALSE'],var.equal=TRUE)
#significant 

t.test(AustralianOpen_Finalists_allstats$FirstServePercentage[AustralianOpen_Finalists_allstats$Winner=='FALSE'],AustralianOpen_Finalists_allstats$FirstServePercentage[AustralianOpen_Finalists_allstats$Winner=="TRUE"],var.equal=TRUE)
#Not Significant 



