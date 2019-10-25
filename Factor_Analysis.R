#Factor Analysis

library(data.table)
library(dplyr)
library(sqldf)
library(MVA)

#data=read.csv("C://Users//Siri//Documents//dataset_final.csv")
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

#Factor Analysis  Analysis

head(AustralianOpen_Finalists_allstats)

str(AustralianOpen_Finalists_allstats)

summary(AustralianOpen_Finalists_allstats)

AustralianOpen_Finalists_allstats_Numeric<-subset(AustralianOpen_Finalists_allstats,select = c("Age","Rank","avgOdds","SP_Percent","RP_Percent","BP_Win_Percentage","Aces","firstServeReturnsWon","SecondServeReturnsWon","FirstServesIn","DoubleFaults","FirstServePercentage"))#Factor Analysis

#compute correlation matrix
cor_tennis<-cor(AustralianOpen_Finalists_allstats_Numeric)
plot(cor_tennis)


AustralianOpen_Finalists_allstats_pca <- prcomp(AustralianOpen_Finalists_allstats_Numeric, scale=TRUE)
plot(AustralianOpen_Finalists_allstats_pca)
summary(AustralianOpen_Finalists_allstats_pca)

eigen_AO_Finalists <-AustralianOpen_Finalists_allstats_pca$sdev^2
eigen_AO_Finalists

names(eigen_AO_Finalists) <- paste("PC",1:12,sep="")
eigen_AO_Finalists

sumlambdas<-sum(eigen_AO_Finalists)
sumlambdas

propvar<-eigen_AO_Finalists/sumlambdas
propvar

cumvar_AO_Finalists<-cumsum(propvar)
cumvar_AO_Finalists

matlambdas<-rbind(eigen_AO_Finalists,propvar,cumvar_AO_Finalists)
rownames(matlambdas)<-c("Eigenvalues","Prop.variance","Cum.propvariance")
matlambdas


eigvec.AO_Finalists <- AustralianOpen_Finalists_allstats_pca$rotation
print(AustralianOpen_Finalists_allstats_pca)

pcafactors.AO_Finalists <- eigvec.AO_Finalists[,1:5]


# Multiplying each column of the eigenvector's matrix by the square-root of the corresponding eigenvalue in order to get the factor loadings
unrot.fact.AO_Finalist <- sweep(pcafactors.AO_Finalists,MARGIN=2,AustralianOpen_Finalists_allstats_pca$sdev[1:5],`*`)
unrot.fact.AO_Finalist

# Performing the varimax rotation. The default in the varimax function is norm=TRUE thus, Kaiser normalization is carried out
rot.fact.AO_Finalist<-varimax(unrot.fact.AO_Finalist)
View(unrot.fact.AO_Finalist)

fact.load.AO_Finalist <- rot.fact.AO_Finalist$loadings[1:12,1:5]
fact.load.AO_Finalist

scale.AO_Finalist <- scale(AustralianOpen_Finalists_allstats_Numeric)
scale.AO_Finalist
as.matrix(scale.AO_Finalist)%*%fact.load.AO_Finalist%*%solve(t(fact.load.AO_Finalist)%*%fact.load.AO_Finalist)

install.packages("psych")
library(psych)
fit.pc <- principal(AustralianOpen_Finalists_allstats_Numeric, nfactors=5, rotate="varimax")
fit.pc
round(fit.pc$values, 3)
fit.pc$loadings 
for (i in c(1,3,2,4,5)) { print(fit.pc$loadings[[1,i]])}
fit.pc$communality
fit.pc$scores
fa.parallel(AustralianOpen_Finalists_allstats_Numeric) #factor recommendation
fa.plot(fit.pc) #see correlations within factors
fa.diagram(fit.pc)#Visualize the realtionship
vss(AustralianOpen_Finalists_allstats_Numeric) # See Factor recommendations for a simple structure
