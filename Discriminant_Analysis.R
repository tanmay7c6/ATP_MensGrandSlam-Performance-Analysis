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


#Discriminant Analysis



library(readr)
library(dplyr)
library(MVA)
library(stringr)
library(MASS)

train<- train_data
test<- test_data
str(train)

train_numeric<-subset(train,select = c("Age","Rank","avgOdds","SP_Percent","BP_Win_Percentage","Aces","firstServeReturnsWon","SecondServeReturnsWon","FirstServesIn","DoubleFaults","FirstServePercentage","winPercentage","Finalists"))
#View(train_numeric)
test_numeric<-subset(test,select = c("Age","Rank","avgOdds","SP_Percent","BP_Win_Percentage","Aces","firstServeReturnsWon","SecondServeReturnsWon","FirstServesIn","DoubleFaults","FirstServePercentage","winPercentage","Finalists"))
head(train_numeric,5)
head(test_numeric,5)


train.lda <- lda( Finalists~., data = train_numeric)
summary(train.lda)
print(train.lda)
plot(train.lda)
train.lda$counts
train.lda$means
train.lda$scaling
train.lda$prior
train.lda$lev
train.lda$svd

lda.predict <- predict(train.lda, newdata = test_numeric)
lda.predict$class
#View(lda.predict)
lda.predict$x



# Get the posteriors as a dataframe.
lda.predict.posteriors <- as.data.frame(lda.predict$posterior)
install.packages("ROCR")
#create ROC/AUC curve
library(ROCR)
pred <- prediction(lda.predict.posteriors[,2], test_numeric$Finalists)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values
plot(roc.perf)
abline(a=0, b= 1)

text(x = .25, y = .65 ,paste("AUC = ", round(auc.train[[1]],3), sep = ""))





          
          