library(dplyr)
library(MASS)
library(tidyverse)
library(ggplot2)
library(gplots)
library(GGally)
set.seed(1996)
library(ggpubr)
library(factoextra)
library(GGally)
library(plotly)
library(devtools)
#install_github("ggobi/ggally")
#install.packages("GGally")
packages = c('GGally', 'plotly', 'parcoords', 'tidyverse')

for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
}
#https://www.kaggle.com/imakash3011/customer-personality-analysis
setwd("/Users/macbookpro/Documents/665_BD")
PerSet <- read.csv("marketing_campaign (1).csv", sep = "\t")
View(PerSet)
PerSet$Spending <-  PerSet$MntWines+
                    PerSet$MntFruits+
                    PerSet$MntSweetProducts+
                    PerSet$MntGoldProds+
                    PerSet$MntMeatProducts+
                    PerSet$MntFishProducts

View(PerSet)
###K Means
#Variables excluded for the analysis:
PerSetKM = subset(PerSet, select = -c(ID,
                                      Year_Birth,
                                      Education,
                                      Marital_Status,
                                      Income,
                                      Kidhome,
                                      Teenhome,
                                      Dt_Customer,
                                      Recency,
                                      NumDealsPurchases,
                                      NumWebPurchases,
                                      NumCatalogPurchases,
                                      NumStorePurchases,
                                      NumWebVisitsMonth,
                                      AcceptedCmp3,
                                      AcceptedCmp4,
                                      AcceptedCmp5,
                                      AcceptedCmp1,
                                      AcceptedCmp2,
                                      Complain,
                                      Z_CostContact,
                                      Z_Revenue,
                                      Response,
                                      Spending))

View(PerSetKM)

#Outliers
ggparcoord(PerSetKM,
           columns = 1:6, groupColumn = 1,
           scale = "uniminmax", 
           title = "Parallel Coord. Plot of outliers")

#Observations removed
max(PerSetKM$MntMeatProducts)
PerSetKM2 <- PerSetKM[-c(22, 165),]
## Perform K-Means with 2 clusters and check the results 
## the set.seed() could use any number since the original starting point is randomly selected
set.seed(7)
km1 = kmeans(PerSetKM2, 3, nstart=100)
km1

#"Elbow method"
install.packages("factoextra")
library(factoextra)
fviz_nbclust(PerSetKM2, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

#Second analysis with the adjusted number of clusters.
set.seed(7)
kmNC = kmeans(PerSetKM2, 2, nstart=100)
kmNC

# Just check the plot to find the optimal K
wss <- (nrow(PerSetKM2)-1)*sum(apply(PerSetKM2,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(PerSetKM2,
                                     centers=i)$Education)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

#Plot Cluster 1
fviz_cluster(km1, data = PerSetKM2[, -2],
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

#Plot Clusters 2
plot(PerSetKM2, col =(km1$cluster +1) , main="K-Means result with 3 clusters", pch=20, cex=2)

#plot Clusters 3
ggparcoord(PerSetKM,
           columns = 1:6, groupColumn = 1,
           scale = "uniminmax", 
           title = "Parallel Coord. Plot of outliers")

#############################################################################################
#Logistic Regression

#install.packages("InformationValue")
library(InformationValue)
library(corrplot)
library(caret)
library(pROC)
#install.packages("ROCR")
library(ROCR)

PerSet <- data %>%
  drop_na()

#New variables
x <- as.Date(mydata1$Dt_Customer,format= "%d-%m-%Y")
mydata1$Yr_Customer <- as.integer(format(Sys.Date(), format = "%Y")) - as.integer(format(x, format="%Y"))
mydata <- select(mydata1, -c("Complain","Z_CostContact","Z_Revenue","Dt_Customer"))
str(mydata)
summary(mydata)

#correlation
table(mydata$Response)
M<-cor(select(mydata,-c("ID","Education","Marital_Status")))
corrplot(M, type = "upper", method="color")

#model 1
loganalysis <- glm(Response~., data=mydata, family = "binomial")
summary(loganalysis)
logAnalysis1 <- glm(Response~NumWebVisitsMonth+NumDealsPurchases+NumWebPurchases+NumStorePurchases+Teenhome+Kidhome+NumCatalogPurchases+
                      AcceptedCmp1+AcceptedCmp2+AcceptedCmp3+AcceptedCmp4+AcceptedCmp5+MntWines+MntFishProducts+MntSweetProducts+MntGoldProds+
                      MntMeatProducts+MntFishProducts+Income+Yr_Customer+Recency, data=mydata, family="binomial")
summary(logAnalysis1)

#model 2 - removed insignificant variables
logAnalysis3 <- glm(Response~NumWebVisitsMonth+NumDealsPurchases+NumWebPurchases+NumStorePurchases+Teenhome+
                      AcceptedCmp1+AcceptedCmp2+AcceptedCmp3+AcceptedCmp4+AcceptedCmp5+MntMeatProducts+
                      Yr_Customer+Recency, data=mydata, family="binomial")
summary(logAnalysis3)

#Predicting the model
predicted <- predict(logAnalysis3, newdata = mydata, type = "response")
summary(predicted)
head(predicted)

#ROC-AUC
plotROC(mydata$Response, predicted)
ROCRpred = prediction(predicted, mydata$Response)
ROCRperf = performance(ROCRpred, "tpr", "fpr", measures = "auc")
ROCRperf
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

#Confusion matrix with threshold 0.5
p_response <- ifelse(predicted >0.5, 1,0)
table(p_response, mydata$Response)
confusionMatrix(factor(p_response),factor(mydata$Response), positive = "1")

#Confusion matrix with threshold 0.3
p_response <- ifelse(predicted >0.3, 1,0)
table(p_response, mydata$Response)
confusionMatrix(factor(p_response),factor(mydata$Response), positive = "1")

#Making Prediction
newdata<-data.frame(NumWebVisitsMonth=10,Teenhome=1,NumDealsPurchases=6,NumWebPurchases=4,NumStorePurchases=3,
                    AcceptedCmp1=1,AcceptedCmp2=1,AcceptedCmp3=1,AcceptedCmp4=1,AcceptedCmp5=1,Recency=5,
                    Yr_Customer=7,MntMeatProducts=500)
predict(logAnalysis3,newdata = newdata,type="response")

newdata<-data.frame(NumWebVisitsMonth=10,Teenhome=1,NumDealsPurchases=6,NumWebPurchases=4,NumStorePurchases=3,
                    AcceptedCmp1=0,AcceptedCmp2=0,AcceptedCmp3=0,AcceptedCmp4=1,AcceptedCmp5=1,Recency=5,
                    Yr_Customer=7,MntMeatProducts=500)
predict(logAnalysis3,newdata = newdata,type="response")

newdata1<-data.frame(NumWebVisitsMonth=10,Teenhome=1,NumDealsPurchases=6,NumWebPurchases=4,NumStorePurchases=10,
                     AcceptedCmp1=1,AcceptedCmp2=1,AcceptedCmp3=1,AcceptedCmp4=1,AcceptedCmp5=1,Recency=5,
                     Yr_Customer=1,MntMeatProducts=500)
predict(logAnalysis3,newdata = newdata1,type="response")
?predict()


