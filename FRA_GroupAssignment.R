library(SnowballC)
library(tm)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(topicmodels)
library(data.table)
library(stringi)
library(syuzhet)
library(qdap)
library(dplyr)
library(plyr)
library(grid)
library(gridExtra)
library(NbClust)
library(cluster)
library(fpc)
library(Rtsne)
library(caTools)
library(rattle)
library(ROCR)
library(psych)
library(DataExplorer)
library(rplot)
library(rpart.plot)
library(CHAID)
library(randomForest)
library(tidyverse)
library(caret)
library(corpus)
library(InformationValue)
library(GGally)
library(xlsx)

#Data Loading
getwd()
company <- read.xlsx("raw-data.xlsx", sheetName = "raw data")
company<-company[-1]
names(company)
attach(company)
summary(company)
sum(is.na(company))

#Removing the column which was completely empty
company[,c("Deposits..accepted.by.commercial.banks.")] <- list(NULL)
summary(WIP.turnover)

#deleting the cols which has high number of missing values
#PE.on.BSE
#Investments
#Other.income
#Deferred.tax.liability
#Income.from.fincial.services
company[,c("PE.on.BSE")] <- list(NULL)
company[,c("Investments")] <- list(NULL)
company[,c("Other.income")] <- list(NULL)
company[,c("Deferred.tax.liability")] <- list(NULL)
company[,c("Income.from.fincial.services")] <- list(NULL)
names(company)
str(company)

#46 variables now
#Missing value treatment

# perform knn imputation.
library(DMwR)
company.knn<- knnImputation(company[,!names(company) %in% "medv"])
summary(company.knn)

#Perform using the library mice
library(mice)
miceMod <- mice(company[, !names(company) %in% "medv"], method="rf") # perform mice imputation, based on random forests.
miceOutput <- complete(miceMod) # generate the completed data.
anyNA(miceOutput)
company.mice = miceOutput

library(xlsx)
#write.xlsx(company.mice, "company_mice.xlsx")
summary(company.mice)
sum(is.na(company.mice))
is.na(company.mice)
company.mice.2<- knnImputation(company.mice[,!names(company.mice) %in% "medv"])
summary(company.mice.2)
sum(is.na(company.mice.2))

#Addition of new variables
#Profitability ratio
company.mice.2$Profitablity = company.mice.2$Profit.after.tax/company.mice.2$Sales
company.mice.2$Profitability.assets.ratio = company.mice.2$PBT/company.mice.2$Total.liabilities
#Leverage ratio
company.mice.2$TotalEquity = company.mice.2$Total.liabilities/company.mice.2$Debt.to.equity.ratio..times.
company.mice.2$EquityMultipier = company.mice.2$Total.assets/company.mice.2$TotalEquity
company.mice.2$Borrowing.ratio = company.mice.2$Borrowings/company.mice.2$Total.liabilities
#Liquidity ratio
company.mice.2$LiquidityRatio = company.mice.2$Net.working.capital/company.mice.2$Total.assets
company.mice.2$Turnover.ratio = company.mice.2$Sales/company.mice.2$Total.liabilities
#Company size
company.mice.2$CompanySize = company.mice.2$Net.worth/company.mice.2$Total.assets
View(company.mice.2)

#Dependent Variable is Default
names(company.mice.2)

# Checking the correlation and MultiCollinearity
mydatacor=cor(company.mice.2[,-c(1)])
View(mydatacor)
write.xlsx(mydatacor, "correlation.xlsx")
corrplot::corrplot(mydatacor,
                   method="color",type="upper",
                   addCoef.col="black",
                   tl.col="black",
                   tl.cex=0.9,
                   diag=FALSE,
                   number.cex=0.7
)

company.mice.2[,c("TotalEquity")] <- list(NULL)
company.mice.2[,c("EquityMultipier")] <- list(NULL)
company.mice.2[,c("Total.assets")] <- list(NULL)
names(company.mice.2)
#EquityMultipier is exactly same as Debt.to.equity.ratio..times. - hence removing it
#Total.assets is exactly same as Total.liabilities - hence removing it
company.mice.2$Default = as.factor(company.mice.2$Default)
TrainModel.1=glm(Default~ .,
                 data=company.mice.2,family = binomial,control = list(maxit = 1000))
vif(TrainModel.1)
#Identifying the important variables by blorr method
library(blorr)
blr_step_aic_both(TrainModel.1, details = FALSE)
library(xlsx)
#write.xlsx(company.mice.2, "company.mice.2.xlsx")
#Identifying the important variables by RF method
company.mice.2.rf=company.mice.2
#Getting the Important Variables from Random Forest after a variable Plot
company_rf_model <- randomForest(Default~., company.mice.2.rf,
                                 ntree = 501, # number of trees to be built
                                 mtry = 5, # number of variables sampled at each split
                                 nodesize = 10, # minimum number of records at terminal node
                                 importance = TRUE) # should importance of predictors be assessed
plot(company_rf_model, main="")
summary(company_rf_model)
detach("package:rattle", unload = TRUE)
importance(company_rf_model)
varImpPlot(company_rf_model)

#For VIF
#OUTLIER TREATMENT
library(outliers)
outlier_capping <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}
company.mice.2.ot = company.mice.2
company.mice.2.ot$Net.worth=outlier_capping(company.mice.2.ot$Net.worth)
company.mice.2.ot$Total.income=outlier_capping(company.mice.2.ot$Total.income)
company.mice.2.ot$Change.in.stock=outlier_capping(company.mice.2.ot$Change.in.stock)
company.mice.2.ot$Total.expenses=outlier_capping(company.mice.2.ot$Total.expenses)
company.mice.2.ot$Profit.after.tax=outlier_capping(company.mice.2.ot$Profit.after.tax)
company.mice.2.ot$PBDITA=outlier_capping(company.mice.2.ot$PBDITA)
company.mice.2.ot$PBT=outlier_capping(company.mice.2.ot$PBT)
company.mice.2.ot$Cash.profit=outlier_capping(company.mice.2.ot$Cash.profit)
company.mice.2.ot$PBDITA.as...of.total.income=outlier_capping(company.mice.2.ot$PBDITA.as...of.total.income)
company.mice.2.ot$PBT.as...of.total.income=outlier_capping(company.mice.2.ot$PBT.as...of.total.income)
company.mice.2.ot$PAT.as...of.total.income=outlier_capping(company.mice.2.ot$PAT.as...of.total.income)
company.mice.2.ot$Cash.profit.as...of.total.income=outlier_capping(company.mice.2.ot$Cash.profit.as...of.total.income)
company.mice.2.ot$PAT.as...of.net.worth=outlier_capping(company.mice.2.ot$PAT.as...of.net.worth)
company.mice.2.ot$Sales=outlier_capping(company.mice.2.ot$Sales)
company.mice.2.ot$Total.capital=outlier_capping(company.mice.2.ot$Total.capital)
company.mice.2.ot$Reserves.and.funds=outlier_capping(company.mice.2.ot$Reserves.and.funds)
company.mice.2.ot$Borrowings=outlier_capping(company.mice.2.ot$Borrowings)
company.mice.2.ot$Current.liabilities...provisions=outlier_capping(company.mice.2.ot$Current.liabilities...provisions)
company.mice.2.ot$Shareholders.funds=outlier_capping(company.mice.2.ot$Shareholders.funds)
company.mice.2.ot$Cumulative.retained.profits=outlier_capping(company.mice.2.ot$Cumulative.retained.profits)
company.mice.2.ot$Capital.employed=outlier_capping(company.mice.2.ot$Capital.employed)
company.mice.2.ot$TOL.TNW=outlier_capping(company.mice.2.ot$TOL.TNW)
company.mice.2.ot$Total.term.liabilities...tangible.net.worth=outlier_capping(company.mice.2.ot$Total.term.liabilities...tangible.net.worth)
company.mice.2.ot$Contingent.liabilities...Net.worth....=outlier_capping(company.mice.2.ot$Contingent.liabilities...Net.worth....)
company.mice.2.ot$Contingent.liabilities=outlier_capping(company.mice.2.ot$Contingent.liabilities)
company.mice.2.ot$Net.fixed.assets=outlier_capping(company.mice.2.ot$Net.fixed.assets)
company.mice.2.ot$Current.assets=outlier_capping(company.mice.2.ot$Current.assets)
company.mice.2.ot$Net.working.capital=outlier_capping(company.mice.2.ot$Net.working.capital)
company.mice.2.ot$Quick.ratio..times.=outlier_capping(company.mice.2.ot$Quick.ratio..times.)
company.mice.2.ot$Current.ratio..times.=outlier_capping(company.mice.2.ot$Current.ratio..times.)
company.mice.2.ot$Debt.to.equity.ratio..times.=outlier_capping(company.mice.2.ot$Debt.to.equity.ratio..times.)
company.mice.2.ot$Cash.to.current.liabilities..times.=outlier_capping(company.mice.2.ot$Cash.to.current.liabilities..times.)
company.mice.2.ot$Cash.to.average.cost.of.sales.per.day=outlier_capping(company.mice.2.ot$Cash.to.average.cost.of.sales.per.day)
company.mice.2.ot$Creditors.turnover=outlier_capping(company.mice.2.ot$Creditors.turnover)
company.mice.2.ot$Debtors.turnover=outlier_capping(company.mice.2.ot$Debtors.turnover)
company.mice.2.ot$Finished.goods.turnover=outlier_capping(company.mice.2.ot$Finished.goods.turnover)
company.mice.2.ot$WIP.turnover=outlier_capping(company.mice.2.ot$WIP.turnover)
company.mice.2.ot$Raw.material.turnover=outlier_capping(company.mice.2.ot$Raw.material.turnover)
company.mice.2.ot$Shares.outstanding=outlier_capping(company.mice.2.ot$Shares.outstanding)
company.mice.2.ot$Equity.face.value=outlier_capping(company.mice.2.ot$Equity.face.value)
company.mice.2.ot$EPS=outlier_capping(company.mice.2.ot$EPS)
company.mice.2.ot$Adjusted.EPS=outlier_capping(company.mice.2.ot$Adjusted.EPS)
company.mice.2.ot$Total.liabilities=outlier_capping(company.mice.2.ot$Total.liabilities)
company.mice.2.ot$Profitablity=outlier_capping(company.mice.2.ot$Profitablity)
company.mice.2.ot$LiquidityRatio=outlier_capping(company.mice.2.ot$LiquidityRatio)
company.mice.2.ot$CompanySize=outlier_capping(company.mice.2.ot$CompanySize)
#company.mice.2.ot$Default=outlier_capping(company.mice.2.ot$Default)
company.mice.2.ot$Profitability.assets.ratio=outlier_capping(company.mice.2.ot$Profitability.assets.ratio)
company.mice.2.ot$Borrowing.ratio=outlier_capping(company.mice.2.ot$Borrowing.ratio)
company.mice.2.ot$Turnover.ratio=outlier_capping(company.mice.2.ot$Turnover.ratio)
## RUNNING THE LOGISTIC MODEL
TrainModel.mice.1=glm(Default~ Net.worth+Cash.profit+PBDITA.as...of.total.income+
                        PBT.as...of.total.income+PAT.as...of.total.income+
                        Cash.profit.as...of.total.income+PAT.as...of.net.worth+
                        Sales+TOL.TNW+Total.term.liabilities...tangible.net.worth+
                        Contingent.liabilities...Net.worth....+Quick.ratio..times.+
                        Current.ratio..times.+Debt.to.equity.ratio..times.+
                        Cash.to.current.liabilities..times.+
                        Cash.to.average.cost.of.sales.per.day+Creditors.turnover+
                        Debtors.turnover+Finished.goods.turnover+WIP.turnover+
                        Raw.material.turnover+EPS+Total.liabilities+Profitablity+
                        LiquidityRatio+CompanySize+Profitability.assets.ratio+
                        Borrowing.ratio+Turnover.ratio,
                      data=company.mice.2.ot,family = binomial,control = list(maxit = 1000))
summary(TrainModel.mice.1)
vif(TrainModel.mice.1)
plot(as.factor(TrainModel.mice.1$y),TrainModel.mice.1$fitted.values)
TrainModel.mice.1.prediction = ifelse(TrainModel.mice.1$fitted.values>0.05,1,0)
table(TrainModel.mice.1$y,TrainModel.mice.1.prediction)
blr_step_aic_both(TrainModel.mice.1, details = FALSE)
vif(TrainModel.mice.1)
#Identifying the important variables by blorr method
library(blorr)

#RE RUNNING THE LOGISTICS MODEL AFTER BLORR VARIABLES TREATMENT
#THIS IS THE FINAL MODEL
TrainModel.mice.2=glm(Default~ PAT.as...of.net.worth+TOL.TNW+
                        Cash.profit.as...of.total.income+Turnover.ratio+
                        CompanySize+Cash.profit+Cash.to.average.cost.of.sales.per.day+
                        Profitability.assets.ratio+Debt.to.equity.ratio..times.+
                        EPS+WIP.turnover+Debtors.turnover+
                        Cash.to.current.liabilities..times.+Current.ratio..times.+
                        Profitablity+Raw.material.turnover,
                      data=company.mice.2.ot,family = binomial,control = list(maxit = 1000))
summary(TrainModel.mice.2)
vif(TrainModel.mice.2)
plot(as.factor(TrainModel.mice.2$y),TrainModel.mice.2$fitted.values)
TrainModel.mice.2.prediction = ifelse(TrainModel.mice.2$fitted.values>0.05,1,0)
table(TrainModel.mice.2$y,TrainModel.mice.2.prediction)

## VALIDATION DATA RUN
validation.data.knn$LR.predScore<- predict(TrainModel.mice.2,
                                           validation.data.knn,
                                           type = "response")
validation.data.knn$LR.predClass =
  ifelse(validation.data.knn$LR.predScore>0.05,1,0)
validation.data.knn$LR.predClass=as.factor(validation.data.knn$LR.predClass)
table(validation.data.knn$Default,
      validation.data.knn$LR.predClass)

#UNIVARIATE ANALYSIS
#REMOVING THE UNWANTED VARIABLES
company.mice.2.EDA = company.mice.2.ot
names(company.mice.2.EDA)
company.mice.2.EDA[,c("Net.worth")] <- list(NULL)
company.mice.2.EDA[,c("Total.income")] <- list(NULL)
company.mice.2.EDA[,c("Change.in.stock")] <- list(NULL)
company.mice.2.EDA[,c("Total.expenses")] <- list(NULL)
company.mice.2.EDA[,c("Profit.after.tax")] <- list(NULL)
company.mice.2.EDA[,c("PBDITA")] <- list(NULL)
company.mice.2.EDA[,c("PBT")] <- list(NULL)
company.mice.2.EDA[,c("PBDITA.as...of.total.income")] <- list(NULL)
company.mice.2.EDA[,c("PBT.as...of.total.income")] <- list(NULL)
company.mice.2.EDA[,c("PAT.as...of.total.income")] <- list(NULL)
company.mice.2.EDA[,c("Sales")] <- list(NULL)
company.mice.2.EDA[,c("Total.capital")] <- list(NULL)
company.mice.2.EDA[,c("Reserves.and.funds")] <- list(NULL)
company.mice.2.EDA[,c("Borrowings")] <- list(NULL)
company.mice.2.EDA[,c("Current.liabilities...provisions")] <- list(NULL)
company.mice.2.EDA[,c("Shareholders.funds")] <- list(NULL)
company.mice.2.EDA[,c("Cumulative.retained.profits")] <- list(NULL)
company.mice.2.EDA[,c("Capital.employed")] <- list(NULL)
company.mice.2.EDA[,c("Total.term.liabilities...tangible.net.worth")] <- list(NULL)
company.mice.2.EDA[,c("Contingent.liabilities...Net.worth....")] <- list(NULL)
company.mice.2.EDA[,c("Contingent.liabilities")] <- list(NULL)
company.mice.2.EDA[,c("Net.fixed.assets")] <- list(NULL)
company.mice.2.EDA[,c("Current.assets")] <- list(NULL)
company.mice.2.EDA[,c("Net.working.capital")] <- list(NULL)
company.mice.2.EDA[,c("Quick.ratio..times.")] <- list(NULL)
company.mice.2.EDA[,c("Creditors.turnover")] <- list(NULL)
company.mice.2.EDA[,c("Finished.goods.turnover")] <- list(NULL)
company.mice.2.EDA[,c("Shares.outstanding")] <- list(NULL)
company.mice.2.EDA[,c("Equity.face.value")] <- list(NULL)
company.mice.2.EDA[,c("Adjusted.EPS")] <- list(NULL)
company.mice.2.EDA[,c("Total.liabilities")] <- list(NULL)
company.mice.2.EDA[,c("LiquidityRatio")] <- list(NULL)
company.mice.2.EDA[,c("Borrowing.ratio")] <- list(NULL)
summary(company.mice.2.EDA)

#Box plot and Histogram for the continuos variables
###########1
boxplot(company.mice.2.EDA$Cash.profit,horizontal = TRUE,
        col="red",main="Box plot of Cash.profit")
hist(company.mice.2.EDA$Cash.profit,col="blue",
     main="Histogram of Cash.profit",xlab="Cash.profit")
boxplot(company.mice.2.EDA$PAT.as...of.net.worth,horizontal = TRUE,
        col="red",main="Box plot of PAT.as...of.net.worth")
hist(company.mice.2.EDA$PAT.as...of.net.worth,col="blue",
     main="Histogram of PAT.as...of.net.worth",xlab="PAT.as...of.net.worth")
boxplot(company.mice.2.EDA$TOL.TNW,horizontal = TRUE,
        col="red",main="Box plot of TOL.TNW")
hist(company.mice.2.EDA$TOL.TNW,col="blue",
     main="Histogram of TOL.TNW",xlab="TOL.TNW")
boxplot(company.mice.2.EDA$Cash.profit.as...of.total.income,horizontal = TRUE,
        col="red",main="Box plot of Cash.profit.as...of.total.income")
hist(company.mice.2.EDA$Cash.profit.as...of.total.income,col="blue",
     main="Histogram of Cash.profit.as...of.total.income",xlab="Cash.profit.as...of.total.income")
##################2
boxplot(company.mice.2.EDA$CompanySize,horizontal = TRUE,
        col="red",main="Box plot of CompanySize")
hist(company.mice.2.EDA$CompanySize,col="blue",
     main="Histogram of CompanySize",xlab="CompanySize")
boxplot(company.mice.2.EDA$Turnover.ratio,horizontal = TRUE,
        col="red",main="Box plot of Turnover.ratio")
hist(company.mice.2.EDA$Turnover.ratio,col="blue",
     main="Histogram of Turnover.ratio",xlab="Turnover.ratio")
boxplot(company.mice.2.EDA$Cash.to.average.cost.of.sales.per.day,horizontal = TRUE,
        col="red",main="Box plot of Cash.to.average.cost.of.sales.per.day")
hist(company.mice.2.EDA$Cash.to.average.cost.of.sales.per.day,col="blue",
     main="Histogram of Cash.to.average.cost.of.sales.per.day",xlab="Cash.to.average.cost.of.sales.per.day")
boxplot(company.mice.2.EDA$Profitability.assets.ratio,horizontal = TRUE,
        col="red",main="Box plot of Profitability.assets.ratio")
hist(company.mice.2.EDA$Profitability.assets.ratio,col="blue",
     main="Histogram of Profitability.assets.ratio",xlab="Profitability.assets.ratio")

############3
boxplot(company.mice.2.EDA$Debt.to.equity.ratio..times.,horizontal = TRUE,
        col="red",main="Box plot of Debt.to.equity.ratio..times.")
hist(company.mice.2.EDA$Debt.to.equity.ratio..times.,col="blue",
     main="Histogram of Debt.to.equity.ratio..times.",xlab="Debt.to.equity.ratio..times.")
boxplot(company.mice.2.EDA$EPS,horizontal = TRUE,
        col="red",main="Box plot of EPS")
hist(company.mice.2.EDA$EPS,col="blue",
     main="Histogram of EPS",xlab="EPS")
boxplot(company.mice.2.EDA$WIP.turnover,horizontal = TRUE,
        col="red",main="Box plot of WIP.turnover")
hist(company.mice.2.EDA$WIP.turnover,col="blue",
     main="Histogram of WIP.turnover",xlab="WIP.turnover")
boxplot(company.mice.2.EDA$Debtors.turnover,horizontal = TRUE,
        col="red",main="Box plot of Debtors.turnover")
hist(company.mice.2.EDA$Debtors.turnover,col="blue",
     main="Histogram of Debtors.turnover",xlab="Debtors.turnover")

#######################4
boxplot(company.mice.2.EDA$Cash.to.current.liabilities..times.,horizontal = TRUE,
        col="red",main="Box plot of Cash.to.current.liabilities..times.")
hist(company.mice.2.EDA$Cash.to.current.liabilities..times.,col="blue",
     main="Histogram of Cash.to.current.liabilities..times.",xlab="Cash.to.current.liabilities..times.")
boxplot(company.mice.2.EDA$Current.ratio..times.,horizontal = TRUE,
        col="red",main="Box plot of Current.ratio..times.")
hist(company.mice.2.EDA$Current.ratio..times.,col="blue",
     main="Histogram of Current.ratio..times.",xlab="Current.ratio..times.")
boxplot(company.mice.2.EDA$Profitablity,horizontal = TRUE,
        col="red",main="Box plot of Profitablity")
hist(company.mice.2.EDA$Profitablity,col="blue",
     main="Histogram of Profitablity",xlab="Profitablity")
boxplot(company.mice.2.EDA$Raw.material.turnover,horizontal = TRUE,
        col="red",main="Box plot of Raw.material.turnover")
hist(company.mice.2.EDA$Raw.material.turnover,col="blue",
     main="Histogram of Raw.material.turnover",xlab="Raw.material.turnover")
summary(company.mice.2.EDA$Cash.profit)

#Bill Variables
nd2.melt <- melt(company.mice.2.EDA, id = c("Default"))
nd2.melt

# box plots
zz <- ggplot(nd2.melt, aes(x=Default, y=value))
zz+geom_boxplot(aes(color = Default), alpha=0.7 ) +
  facet_wrap(~variable,scales = "free_x", nrow = 3)+
  coord_flip()
names(company.mice.2.EDA)
mydata2cor=cor(company.mice.2.EDA[,-c(1)])
mydata2cor
corrplot::corrplot(mydata2cor,
                   method="color",type="upper",
                   addCoef.col="black",
                   tl.col="black",
                   tl.cex=0.9,
                   diag=FALSE,
                   number.cex=0.7
)


#THIS IS THE FINAL MODEL
TrainModel.mice.2=glm(Default~ PAT.as...of.net.worth+TOL.TNW+
                        Cash.profit.as...of.total.income+Turnover.ratio+
                        CompanySize+Cash.profit+Cash.to.average.cost.of.sales.per.day+
                        Profitability.assets.ratio+Debt.to.equity.ratio..times.+
                        EPS+WIP.turnover+Debtors.turnover+
                        Cash.to.current.liabilities..times.+Current.ratio..times.+
                        Profitablity+Raw.material.turnover,
                      data=company.mice.2.ot,family = binomial,control = list(maxit = 1000))
summary(TrainModel.mice.2)
vif(TrainModel.mice.2)
plot(as.factor(TrainModel.mice.2$y),TrainModel.mice.2$fitted.values)
company.mice.2.ot$LR.predScore<- predict(TrainModel.mice.2,
                                         company.mice.2.ot, type = "response")
company.mice.2.ot$LR.predClass = ifelse(company.mice.2.ot$LR.predScore>0.5,1,0)
pred_ratio <- prediction(company.mice.2.ot$LR.predScore, company.mice.2.ot$Default)
perf_ratio <- performance(pred_ratio, "tpr", "fpr")
plot(perf_ratio,main = "ROC curve")
auc(company.mice.2.ot$Default,company.mice.2.ot$LR.predScore)

###KS
max(perf_ratio@y.values[[1]]-perf_ratio@x.values[[1]])
library(ineq)

########### GINI
ineq(company.mice.2.ot$LR.predScore,"gini")
#TrainModel.mice.2.prediction = ifelse(TrainModel.mice.2$fitted.values>0.05,1,0)
#View(TrainModel.mice.2.prediction)
#table(TrainModel.mice.2$y,TrainModel.mice.2.prediction)

## VALIDATION DATA RUN
validation.data.knn <- read.xlsx("validation_data-2.xlsx", sheetName = "valdata")
validation.data.knn$LR.predScore<- predict(TrainModel.mice.2,
                                           validation.data.knn,
                                           type = "response")
validation.data.knn$LR.predClass =
  ifelse(validation.data.knn$LR.predScore>0.05,1,0)
validation.data.knn$LR.predClass=as.factor(validation.data.knn$LR.predClass)
table(validation.data.knn$Default,
      validation.data.knn$LR.predClass)
validation.data.knn$LR.predScore<- predict(TrainModel.mice.2,
                                           validation.data.knn, type = "response")
validation.data.knn$LR.predClass = ifelse(validation.data.knn$LR.predScore>0.5,1,0)
pred_ratio <- prediction(validation.data.knn$LR.predScore, validation.data.knn$Default)
perf_ratio <- performance(pred_ratio, "tpr", "fpr")
plot(perf_ratio,main = "ROC curve")
auc(validation.data.knn$Default,validation.data.knn$LR.predScore)
#Concordance Ratio
Concordance(actuals = validation.data.knn$Default,
            predictedScores = validation.data.knn$LR.predScore)
###KS
max(perf_ratio@y.values[[1]]-perf_ratio@x.values[[1]])
library(ineq)

########### GINI
ineq(validation.data.knn$LR.predScore,"gini")
## Analyzing Coefficients
summary(TrainModel.mice.2)
exp(coef(TrainModel.mice.2))


## Decile Prediction
table(validation.data.knn$Default,
      validation.data.knn$LR.predClass)
library(xlsx)
write.xlsx(validation.data.knn$Default, "validation.default.xlsx")
write.xlsx(validation.data.knn$LR.predScore, "validation.prediction.xlsx")
write.xlsx(company.mice.2.ot$Default, "company.mice.2.ot.default.xlsx")
write.xlsx(company.mice.2.ot$LR.predScore, "company.mice.2.ot.prediction.xlsx")