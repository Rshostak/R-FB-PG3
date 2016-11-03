# Set working directory and import datafiles
#install.packages('RGtk2')
#install.packages('rattle')
#install.packages('rpart.plot')
#install.packages('RColorBrewer')

#install.packages("dplyr")
#install.packages("magrittr")

#install.packages("lattice")
#install.packages("ggplot2")
#install.packages("caret")

#install.packages('randomForest')
#install.packages('party')

#install.packages('Hmisc') #for describe

#for nice tree decision
library(rpart)
library(RGtk2)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#library(lattice)
#library(ggplot2)
#library(caret)

require(magrittr)
require(dplyr)        #for createDataPartition
require(caret)        #for slice

library(randomForest)
require(party)

library(Hmisc)

setwd("E:/temp/kaggle/My_R/FB_BF")
train_ini <- read.csv("E:/temp/kaggle/My_R/FB_BF/ZUPp_M_h10.csv",header=TRUE, sep=";", row.names=NULL)
train_ini$sl <- 0 
test <- read.csv("E:/temp/kaggle/My_R/FB_BF/ZUP_M_h16.csv",header=TRUE, sep=";", row.names=NULL)
test$sl <- 1 
train <- rbind(train_ini, test)
# train <- train_ini

# there is an incorrect prediction where is only target product
train=train[!(train$isTarget == 1 & train$isNotOnlyTarget ==0),] #excuding the clients with the only target product

anColName = 'isTarget'
idColName = 'id'

# #for Server
# train$isLicense <- NULL #Home, CHANGE IT!
# train$isServer <- NULL #Target, CHANGE IT!
# train$name <- NULL #client's name
# # context Server
# train$isBS <- NULL #context
# train$isRep <- NULL #context

#for ZUP
train$isBuh <- NULL #Home, CHANGE IT!
train$isZUP <- NULL #Target, CHANGE IT!

# # context ZUP
train=train[train$cEmpl >= 2,] #2 for ZUP all
train$isEmpl20 <- 0
train$isEmpl20[train$cEmpl >= 22] <- 1

# train$cEmpl[train$cEmpl == 0] = NA
# cEmpl <- rpart(cEmpl ~ branch + sS + sG, data=train[!is.na(train$cEmpl),], method="anova")
# train$cEmpl[is.na(train$cEmpl)] <- predict(cEmpl, train[is.na(train$cEmpl),])

#for everything
train$name <- NULL #client's name
train$cCust <- NULL #client's quantity


#restrict the dataset with Head or|and something else 
# train=train[train$sGS >10000,]
train=train[train$isHead == 1,]
train$isHead <- NULL #client's name
# train=train[train$sTotal !=0,]
# train=train[train$isBuh !=0,]
# train=train[train$sGS >30000,]

#find the clients with Head and Target together 
train=train[train$isSameOrNever == 1,]

# доля купивших 
nrow(train[train$isTarget == 1,])/nrow(train)

# доля купивших вместе с Head среди купивших
nrow(train[train$isSameDay,]) / nrow(train[train$isTarget == 1,])

train$branch0 <- as.numeric(train$branch)
train$branch0 <- paste0(as.character(train$branch0))
train$branch0 <- factor(train$branch0)

train$branchN <- paste(train$branch0,train$branch, sep = '=')
branches <- data.frame(table(train$branchN))
train$branch <- NULL 
train$branchN <- NULL 

#mask the prediction colomns
train$isNotOnlyTarget <- NULL #=Target
train$cDaysBetween <- NULL #Days between Head & Target
train$isSameDay <- NULL #Days between Head & Target
train$isLater <- NULL #Days between Head & Target
train$isLaterOrNever <- NULL #Days between Head & Target
train$isSameOrNever <- NULL #Days between Head & Target
train$isEarlier <- NULL #Days between Head & Target

#split back the test and train sets
test <- train[train$sl == 1,]
train <- train[train$sl == 0,]
test$sl <- NULL
train$sl <- NULL

#Split in-train and in-test
set.seed(222)
split <- createDataPartition(train[,anColName], p = 1/2, list = FALSE)
train0 <- slice(train, split)
train1 <- slice(train, -split)

########################################################

# Prediction

# randomForest - a random Forest
all_var_PA = sort(names(train[colSums(is.na(train)) == 0]))
all_var_NA = sort(names(train[colSums(is.na(train)) != 0]))
all_var_PA <- all_var_PA[!is.element(all_var_PA,c(anColName,idColName,'sTarget'))]

all_var_PA

parseText_eval = paste(anColName, paste0(all_var_PA, collapse = " + "), sep = " ~ ")
parseText_eval = paste0('as.factor(', anColName,') ~ ', paste0(all_var_PA, collapse = " + "))

fit <- randomForest(eval(parse(text=parseText_eval)), data=train1[!is.na(train1[,anColName]),], importance=TRUE, ntree=200)
# fit <- randomForest(eval(parse(text=parseText_eval)), data=train1[!is.na(train1[,anColName]),], importance=TRUE, ntree=200, na.action=na.omit)
# fit <- randomForest(as.factor(isTarget) ~ cEmpl + branch0  + isUT, data=train1, importance=TRUE, ntree=200)

# # ZUP light
# fit <- randomForest(as.factor(isTarget) ~ sTotal + sS + sGS + sG + cEmpl + branch0 + isService, data=train1, importance=TRUE, ntree=200)

# # ZUP all
# fit <- randomForest(as.factor(isTarget) ~ sG + sS + cEmpl + branch0, data=train1, importance=TRUE, ntree=200)

varImpPlot(fit)
Prediction <- predict(fit, train0)

submit <- data.frame(id = train0[,idColName], PredictA = train0[,anColName], PredictB = Prediction, Coin = (train0[,anColName] == Prediction))
prop.table(table(submit$Coin))
prop.table(table(submit$Coin[submit$PredictA == 1]))

nrow((submit[submit$PredictA == 1,])) #Target
nrow((submit[submit$PredictA != 1 & submit$PredictB == 1,]))

# # cforest - a random Forest+
# fit <- cforest(as.factor(isZUP) ~ isBUCorp + office + branch + cEmpl + cBus + sT  + sS + sTotal + cSMen + dFirstSale + cWorkMonth + cSaleMonth + isAnyBuh, data=train1, controls=cforest_unbiased(ntree=50, mtry=3))
# Prediction <- predict(fit, train0, OOB=TRUE, type = "response")
# submit <- data.frame(id = train0$id, PredictA = train0$isZUP, PredictB = Prediction, Coin = (train0$isZUP == Prediction))
# prop.table(table(submit$Coin))
# prop.table(table(submit$Coin[submit$PredictA == 1]))
# 


############################ 
############################ functions
############################ 
## submitting <- function(fit, dataset)
# Prediction <- predict(fit, train0, type = "class")
# submit <- data.frame(id = train0[,idColName], sTotal = train0[,'sTotal'],  PredictA = train0[,anColName], PredictB = Prediction, Coin = (train0[,anColName] == Prediction))
# prop.table(table(submit$Coin))
# prop.table(table(submit$Coin[submit$PredictA == 1]))
# # bought
# nrow((submit[submit$PredictA == 1,])) #Target
# # predicted
# nrow((submit[submit$PredictB == 1,])) #Target
# # predicted and bought
# nrow((submit[submit$PredictA == 1 & submit$PredictB == 1,]))
# # predicted but not  bought
# nrow((submit[submit$PredictA != 1 & submit$PredictB == 1,]))
# 
# # sum median predicted and bought
# median((submit[submit$PredictA == 1 & submit$PredictB == 1,'sTotal']))
# # sum median predicted but not bought
# # sum((submit[submit$PredictA != 1 & submit$PredictB == 1,'sTotal'])) / nrow((submit[submit$PredictA != 1 & submit$PredictB == 1,]))
# median((submit[submit$PredictA != 1 & submit$PredictB == 1,'sTotal']))
# 
submitting <- function(fit, dataset){
  Prediction <- predict(fit, dataset, type = "class")
  submit <- data.frame(id = dataset[,idColName], sTotal = dataset[,'sTotal'], sTarget = dataset[,'sTarget'],  PredictA = dataset[,anColName], PredictB = Prediction, Coin = (dataset[,anColName] == Prediction))
  # submit <- data.frame(id = dataset[,idColName], sTotal = dataset[,'sTotal'], sTarget = dataset[,'sTarget'],  PredictA = dataset[,anColName], PredictB = dataset[,'isEmpl20'], Coin = (dataset[,anColName] == dataset[,'isEmpl20']))
  
  
  print(prop.table(table(submit$Coin)))
  print(prop.table(table(submit$Coin[submit$PredictA == 1])))
  # all
  print(paste('all=',nrow(submit))) #all
    # bought
  print(paste('bought=',nrow((submit[submit$PredictA == 1,])))) #Target
  # predicted
  print(paste('predicted=',nrow((submit[submit$PredictB == 1,])))) #Target
  # predicted and bought
  print(paste('predicted and bought=',nrow((submit[submit$PredictA == 1 & submit$PredictB == 1,]))))
  # predicted but not  bought
  print(paste('predicted but not  bought=',nrow((submit[submit$PredictA != 1 & submit$PredictB == 1,]))))

  submitBought = submit[submit$PredictA == 1 & submit$PredictB == 1,]
  submitNotBought = submit[submit$PredictA != 1 & submit$PredictB == 1,]
  
  # sum median target
  sumTarget = median((submit[submit$PredictA == 1 & submit$PredictB == 1,'sTarget']))
  sumTargetR = median(submitBought[,'sTarget'])
  
  print(paste('sum Target=',sumTargetR,sumTarget,sep=' '))
  
  # sum median predicted and bought
  sumPredictedBought = median((submit[submit$PredictA == 1 & submit$PredictB == 1,'sTotal']))
  sumPredictedBoughtR = median(submitBought[,'sTotal'])
  
  print(paste('sum median predicted and bought=',sumPredictedBoughtR,sumPredictedBought,sep=' '))
  # sum median predicted but not bought
  # sum((submit[submit$PredictA != 1 & submit$PredictB == 1,'sTotal'])) / nrow((submit[submit$PredictA != 1 & submit$PredictB == 1,]))
  sumPredictedNotBought = median((submit[submit$PredictA != 1 & submit$PredictB == 1,'sTotal']))
  sumPredictedNotBoughtR = median(submitNotBought[,'sTotal'])
  
  print(paste('sum median predicted but not bought=',sumPredictedNotBought))
  print(paste('sum median predicted but not bought=',sumPredictedNotBoughtR,sumPredictedNotBought,sep=' '))
  
  
  print(paste('difference=',sumPredictedBought - sumPredictedNotBought))
  return(submit)
}

# rpart - a decision tree

rpartcontrol = rpart.control(maxdepth = 5) 
parseText_eval = paste(anColName, paste0(all_var_PA, collapse = " + "), sep = " ~ ") 
# fit <- rpart(eval(parse(text=parseText_eval)), data=train1, method="class")
fit <- rpart(eval(parse(text=parseText_eval)), data=train1, method="class", control = rpartcontrol)

fit <- rpart(isTarget ~ sGHeadDate  + cEmpl + branch0 , data=train0, method="class", control = rpartcontrol)
fancyRpartPlot(fit)
submit <- submitting(fit, train1)

# ZUP all
fit <- rpart(isTarget ~ branch0 + sGHeadDate + isEmpl20, data=train0, method="class", control = rpartcontrol)
fancyRpartPlot(fit)
submit <- submitting(fit, test)
