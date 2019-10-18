#PCA Analysis

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

#PCA Analysis

head(AustralianOpen_Finalists_allstats)

str(AustralianOpen_Finalists_allstats)

summary(AustralianOpen_Finalists_allstats)

cor(AustralianOpen_Finalists_allstats_Numeric)

plot(cor(AustralianOpen_Finalists_allstats_Numeric))

#Using prcomp to compute the pricipal components (eigenvalues and eigenvectors), With Scale=TRUE, variable means are set to zero, and variance set to one
AustralianOpen_Finalists_allstats_pca<-prcomp(AustralianOpen_Finalists_allstats_Numeric,scale=TRUE)
plot(AustralianOpen_Finalists_allstats_pca)

summary(AustralianOpen_Finalists_allstats_pca)
View(AustralianOpen_Finalists_allstats_pca)


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
rownames(matlambdas)
round(matlambdas,5)

summary(AustralianOpen_Finalists_allstats_pca)
AustralianOpen_Finalists_allstats_pca$rotation
print(AustralianOpen_Finalists_allstats_pca)

#Sample scores stores in AustralianOpen_Finalists_allstats_pca$x
head(AustralianOpen_Finalists_allstats_pca$x)

#Identifying scores by their conversion status
AO_type_finalists_pca<-cbind(data.frame(AustralianOpen_Finalists_allstats$Winner),AustralianOpen_Finalists_allstats_pca$x)

head(AO_type_finalists_pca)

#Means od scors for all PC's classified by Winners of Finals

tabmeansPC<-aggregate(AO_type_finalists_pca[,2:13],by=list(Winner=AO_type_finalists_pca$AustralianOpen_Finalists_allstats.Winner),mean)
tabmeansPC

tabmeansPC<-tabmeansPC[rev(order(tabmeansPC$Winner)),]
tabmeansPC

tabfmeans<-t(tabmeansPC[,-1])
tabfmeans

colnames(tabfmeans)<-t(as.vector(tabmeansPC[1]))
tabfmeans

#Standard Deviations of scores for all the PC's Classified by Winner Yes/NO

tabsdsPC<-aggregate(AO_type_finalists_pca[,2:13],by=list(Winner=AustralianOpen_Finalists_allstats$Winner),sd)
tabsds<-t(tabsdsPC[,-1])
colnames(tabsds)<-t(as.vector(tabsdsPC[1]))
tabsds


#t test on all the principal components
t.test(PC1~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca)

t.test(PC2~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca)

t.test(PC3~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca)

t.test(PC4~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca)

t.test(PC5~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca)

t.test(PC6~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca)

t.test(PC7~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca)

t.test(PC8~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca)

t.test(PC9~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca)

t.test(PC10~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca)

t.test(PC11~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca)

t.test(PC12~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca)

#F Ratio Test

var.test(PC1~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca)
var.test(PC2~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca)
var.test(PC3~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca)
var.test(PC4~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca)
var.test(PC5~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca)
var.test(PC6~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca)
var.test(PC7~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca)
var.test(PC8~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca)
var.test(PC9~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca)
var.test(PC10~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca)
var.test(PC11~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca)
var.test(PC12~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca)

#Levene's tests(one-sided)

library(car)
(LTPC_1<-leveneTest(PC1~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca))
(p_PC1_1sided<-LTPC_1[[3]][1]/2)

(LTPC_1<-leveneTest(PC2~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca))
(p_PC2_1sided<-LTPC_1[[3]][1]/2)

(LTPC_1<-leveneTest(PC3~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca))
(p_PC3_1sided<-LTPC_1[[3]][1]/2)

(LTPC_1<-leveneTest(PC4~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca))
(p_PC4_1sided<-LTPC_1[[3]][1]/2)

(LTPC_1<-leveneTest(PC5~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca))
(p_PC5_1sided<-LTPC_1[[3]][1]/2)

(LTPC_1<-leveneTest(PC6~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca))
(p_PC6_1sided<-LTPC_1[[3]][1]/2)

(LTPC_1<-leveneTest(PC7~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca))
(p_PC7_1sided<-LTPC_1[[3]][1]/2)

(LTPC_1<-leveneTest(PC8~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca))
(p_PC8_1sided<-LTPC_1[[3]][1]/2)

(LTPC_1<-leveneTest(PC9~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca))
(p_PC9_1sided<-LTPC_1[[3]][1]/2)

(LTPC_1<-leveneTest(PC10~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca))
(p_PC10_1sided<-LTPC_1[[3]][1]/2)

(LTPC_1<-leveneTest(PC11~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca))
(p_PC11_1sided<-LTPC_1[[3]][1]/2)

(LTPC_1<-leveneTest(PC12~AustralianOpen_Finalists_allstats$Winner,data=AO_type_finalists_pca))
(p_PC12_1sided<-LTPC_1[[3]][1]/2)


#Plotting scores for first and second component
plot(AO_type_finalists_pca$PC1,pch=ifelse(AO_type_finalists_pca$AustralianOpen_Finalists_allstats.Winner=="TRUE",1,16),xlab = "PC1",ylab = "PC2",main="Australian Open Finals Winner Response for PC1 and PC2")

plot(eigen_AO_Finalists, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")

plot(log(eigen_AO_Finalists), xlab = "Component number",ylab = "log(Component variance)", type="l",main = "Log(eigenvalue) diagram")

print(summary(AustralianOpen_Finalists_allstats_pca))
View(AustralianOpen_Finalists_allstats_pca)
diag(cov(AustralianOpen_Finalists_allstats_pca$x))
xlim <- range(AustralianOpen_Finalists_allstats_pca$x[,1])
head(AustralianOpen_Finalists_allstats_pca$x[,1])
head(AustralianOpen_Finalists_allstats_pca$x)
plot(AustralianOpen_Finalists_allstats_pca$x,xlim=xlim,ylim=xlim)
AustralianOpen_Finalists_allstats_pca$rotation[,1]
AustralianOpen_Finalists_allstats_pca$rotation[,2]
AustralianOpen_Finalists_allstats_pca$rotation[,3]
AustralianOpen_Finalists_allstats_pca$rotation


#get the original value of the data based on PCA

center <- AustralianOpen_Finalists_allstats_pca$center
scale <- AustralianOpen_Finalists_allstats_pca$scale
new_AO_Finalist <- as.matrix(AustralianOpen_Finalists_allstats_Numeric)
head(new_AO_Finalist)
drop(scale(new_AO_Finalist,center=center, scale=scale)%*%AustralianOpen_Finalists_allstats_pca$rotation[,1])
drop(new_AO_Finalist%*%AustralianOpen_Finalists_allstats_pca$rotation[,1])
predict(AustralianOpen_Finalists_allstats_pca)[,1]

