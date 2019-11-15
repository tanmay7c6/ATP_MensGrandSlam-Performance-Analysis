library(readr)
library(dplyr)
library(MVA)
library(stringr)

dataset_final <- read_csv("dataset_final.csv")



AustralianOpen<-subset(dataset_final,dataset_final$Tournament=="Australian Open")

AustralianOpen$Tournament<-NULL
AustralianOpen$MatchID<-NULL


total_matches=AustralianOpen%>%
  group_by(PlayerName,Year)%>%
  summarize(total_matchs=n())


total_matches_Won=AustralianOpen%>%
  filter(Winner=='TRUE')%>%
  group_by(PlayerName,Year)%>%
  summarise(total_matches_won=n())


total_matches_Lost=AustralianOpen%>%
  filter(Winner=='FALSE')%>%
  group_by(PlayerName,Year)%>%
  summarise(total_matches_Lost=n())

# Exporting to CSV to find CumSum

write.csv(total_matches,"total_matches.csv", row.names = FALSE)
write.csv(total_matches_Won,"total_matches_Won.csv", row.names = FALSE)
write.csv(total_matches_Lost,"total_matches_Lost", row.names = FALSE)

rm(total_matches)
rm(total_matches_Lost)
rm(total_matches_Won)

#Importing the new files

total_matches <- read_csv("C:/Users/TG/Desktop/Multivariate Analysis/total_matches.csv")
total_matches_Won <- read_csv("C:/Users/TG/Desktop/Multivariate Analysis/total_matches_Won.csv")
total_matches_Lost <- read_csv("C:/Users/TG/Desktop/Multivariate Analysis/total_matches_Lost.csv")



total_Loss<-merge(x=total_matches,y=total_matches_Lost,by=c("PlayerName","Year"),all.x = TRUE)
 
total_Loss_Won<-merge(x=total_Loss,y=total_matches_Won,by=c("PlayerName","Year"),all.x = TRUE)

total_Loss_Won$total_matchs<-NULL
total_Loss_Won$total_matches_Lost<-NULL
total_Loss_Won$total_matches_won<-NULL
total_Loss_Won$total_matchs<-NULL

total_Loss_Won$Cumulative_lost<-ifelse(is.na(total_Loss_Won$Cumulative_lost),total_Loss_Won$Cumulative_Total-total_Loss_Won$Cumulative_Won,total_Loss_Won$Cumulative_lost)

total_Loss_Won$Cumulative_Won<-ifelse(is.na(total_Loss_Won$Cumulative_Won),total_Loss_Won$Cumulative_Total-total_Loss_Won$Cumulative_lost,total_Loss_Won$Cumulative_Won)


total_Loss_Won$winPercentage=(winPercentage=(total_Loss_Won$Cumulative_Won)/total_Loss_Won$Cumulative_Total)

AustralianOpen_allPlayersFinal<-merge(x=AustralianOpen,y=total_Loss_Won,by=c("PlayerName","Year"))


AustralianOpen_Finalists<-subset(AustralianOpen,AustralianOpen$Round=='The Final')
AustralianOpen_Finalists<-subset(AustralianOpen_Finalists,select = c("PlayerName","Year"))
AustralianOpen_Finalists$Finalists<-1

AustralianOpen_allPlayersFinal<-merge(x=AustralianOpen_allPlayersFinal,y=AustralianOpen_Finalists, by=c("PlayerName","Year"),all.x = TRUE)

AustralianOpen_allPlayersFinal[is.na(AustralianOpen_allPlayersFinal)]<-0


AustralianOpen_FinalistsOnly<-merge(x=AustralianOpen_allPlayersFinal,y=AustralianOpen_Finalists, by=c("PlayerName","Year"))

AustralianOpen_FinalistsOnly

write.csv(AustralianOpen_allPlayersFinal,"AustralianOpen_allPlayersFinal.csv",row.names = FALSE)

write.csv(AustralianOpen_FinalistsOnly,"AustralianOpen_FinalistsOnly.csv",row.names = FALSE)


train<-train_data
test<-test_data

head(train,5)

train_numeric<-subset(train,select = c("Age","Rank","avgOdds","SP_Percent","BP_Win_Percentage","Aces","firstServeReturnsWon","SecondServeReturnsWon","FirstServesIn","DoubleFaults","FirstServePercentage","winPercentage","Finalists"))
#View(train_numeric)

test_numeric<-subset(test,select = c("Age","Rank","avgOdds","SP_Percent","BP_Win_Percentage","Aces","firstServeReturnsWon","SecondServeReturnsWon","FirstServesIn","DoubleFaults","FirstServePercentage","winPercentage","Finalists"))

#Logistic Regression
logistic_regres <- glm( Finalists ~. ,data=train_numeric, family="binomial")
summary(logistic_regres)


#Mcfadden R2 Value
ll.null <- logistic_regres$null.deviance/-2
ll.proposed <- logistic_regres$deviance/-2
ll.null
ll.proposed
(ll.null - ll.proposed) / ll.null


#probablity_pred
predicted.data<-data.frame(probability.of.final=logistic_regres$fitted.values,Finalists=train_numeric$Finalists)
predicted.data <- predicted.data[order(predicted.data$probability.of.final, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)



library("ggplot2")
ggplot(data=predicted.data, aes(x=rank, y=probability.of.final)) +
geom_point(aes(color=Finalists), alpha=1, shape=4, stroke=2) +
xlab("Index") +
ylab("Predicted probability of reaching finals")

#install.packages("regclass")
library(regclass)
confusion_matrix(logistic_regres)

#install.packages("caret")
library(caret)

pdata <- predict(logistic_regres,newdata=test_numeric,type="response")
pdata
train_numeric$Finalists=as.factor(train_numeric$Finalists)
test_numeric$Finalists=as.factor(test_numeric$Finalists)
str(pdata)
str(train_numeric$Finalists)
#View(train_numeric)

pdataF<- as.factor(ifelse(test=as.numeric(pdata>0.54)==0,yes=0,no=1))

#install.packages("e1071")
library(e1071)
confusionMatrix(pdataF,test_numeric$Finalists)


#install.packages("pROC")
library(pROC)
roc(train_numeric$Finalists,logistic_regres$fitted.values,plot=TRUE)

roc(train_numeric$Finalists,logistic_regres$fitted.values,plot=TRUE, legacy.axes=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4,print.auc= TRUE)

roc.info <- roc(train_numeric$Finalists,logistic_regres$fitted.values, legacy.axes=TRUE)
str(roc.info)
roc.df <- data.frame(tpp=roc.info$sensitivities*100, fpp=(1 - roc.info$specificities)*100,thresholds=roc.info$thresholds)
roc.df
head(roc.df) 
tail(roc.df)






