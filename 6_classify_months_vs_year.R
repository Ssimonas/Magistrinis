setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(DrugClust)
library(randomForest)
library(mlr)
library(tuneRanger)
library(caret)
library(tidyr)
library(PresenceAbsence)
library(ROC)
library(DMwR)
library(data.table)
library(glmnetUtils)
library(glmnet)
library(doParallel)


using_model <- "rngRF-all-year"
#using_model <- "rngRF-all-year"

# 0.001336 disbalance
fullData <- read.csv(paste0("data\\4_ready_data\\train_only_jan.csv"), header=T, stringsAsFactors = FALSE, sep=',')
# 10 healthy companies for every bankrupt
fullData <- read.csv(paste0("data\\4_ready_data\\train_all_year.csv"), header=T, stringsAsFactors = FALSE, sep=',')


fullData <- fullData[!names(fullData) %in% c("DATE","ID")]

idxY <- which(colnames(fullData) %in% "STATUS")
col_target_ind <- which(colnames(fullData) %in% "STATUS")

fullData[,col_target_ind] <- factor(as.character(fullData[,col_target_ind]),labels=c("False","True")) # This works

length(fullData$STATUS[fullData$STATUS == "True"])/length(fullData$STATUS[fullData$STATUS == "False"])


## For monthly data
fullData_healthy <- fullData[ sample( which( fullData$STATUS == "False" ) , (nrow(fullData[fullData$STATUS=="True",])*10) ), ]
nrow(fullData_healthy)
fullData <- rbind(fullData_healthy,fullData[fullData$STATUS=="True",])
length(fullData$STATUS[fullData$STATUS == "True"])/length(fullData$STATUS[fullData$STATUS == "False"])

set.seed(101)
rand <- sample(nrow(fullData))
fullData <- fullData[rand,]

sapply(fullData, function(x){sum(is.na(x))})

nrow(fullData)
nrow(fullData[fullData$STATUS == "True",])
nrow(fullData[fullData$STATUS == "True",])/nrow(fullData)

no_cores <- detectCores()
cl <- makeCluster(no_cores)
registerDoParallel(cl)

# classical random forest settings
#mtry <- floor(sqrt(ncol(fullData)-1))
det.mtry <- 50
#ntree <- 100
ntree <- 200
# ptree <- 50 #only RF
det.min.node.size <- 100

### random forest settings for tuneRanger
#tuneParams <- c("mtry", "min.node.size")
#itersWarm <- 8
#itersTune <- 15
#tuneRangerMeasure <- list(mlr::logloss)
#tuneRangerMeasureTxt <- "logloss" # auc ok if no class imbalance ### ???
kCV <- 5

myFolds <- createFolds(fullData[,"STATUS"],kCV)

factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)], as.numeric))

gc()
myResults <- NULL

colY <-"STATUS"

TIME <- Sys.time()
i <- 1
for (i in 1:kCV) {
  
  tstInd <- myFolds[[i]]
  trnIdx <- as.logical(rep(1,1,nrow(fullData)))
  trnIdx[tstInd] <- FALSE
  trnInd <- which(trnIdx)
  target <- as.logical(fullData[tstInd,idxY])

  Y <- as.factor(fullData[trnInd,idxY])
  
  # classProps <- as.numeric(prop.table(table(Y)))
  # sampfrac <- 2*min(classProps)*0.632
  # W <- rep(NA, length(Y))
  # W[Y=="False"] <- classProps[2]
  # W[Y=="True"] <- classProps[1]
  
  trainData <- fullData[trnInd,]
  testData <- fullData[tstInd,-idxY]
  
  
  ######################################################################################################
  ######### RANGER RANDOM FOREST - SMOTE########
  cat(sprintf("\nCV fold %d out of %d / Ranger Random Forest (%s)\n", i, kCV,using_model))
  
  task <- makeClassifTask(data = trainData, target = "STATUS", positive = "True")
  #estimateTimeTuneRanger(task, iters = itersWarm + itersTune, num.threads = no_cores, num.trees = ntree)
  cat('\n')
  
  #idx <- which.min(res$results[,tuneRangerMeasureTxt])
  #opt.mtry <- res$results$mtry[idx]
  #opt.min.node.size <- res$results$min.node.size[idx]
  
  rf_model <- ranger(data = trainData, classification = T,
                     dependent.variable.name = colY, num.trees = ntree,
                     mtry = det.mtry, min.node.size = det.min.node.size,
                     write.forest = T, probability = T, replace = F,
                     num.threads = no_cores, verbose = T, importance = "impurity")
  print(rf_model)

  model <- rep(using_model,length(target))
  soft <- predict(rf_model,data=testData)$predictions
  score <- soft[,2]
  myResults <- rbind(myResults,data.frame(tstInd,model,score,target))

  saveRDS(rf_model, paste0(using_model,"_model_",format(Sys.time(),'_%Y%m%d_%H%M%S'),"_CV_",i,".rds"))
  
  DF<-as.data.frame(rf_model$variable.importance)
  setDT(DF, keep.rownames = "newname")
  colnames(DF) <- c("var", "importance")
  write.csv(DF, file=paste0(using_model,"_var_importance_",i,"_.csv"), row.names = TRUE)
  rm(rf_model)    
  
}

Sys.time()-TIME

# some_results <- read.csv(paste0("Run_04__new_data_3RF\\myResults_RF-SMOTE_RF_RNG-RF_NEURALNET_02.csv"), header=T, stringsAsFactors = FALSE, sep=',')
# myResults <- some_results

################# EVALUATION ###################
myResults_back <- myResults

write.csv(myResults, file=paste0(using_model,"_myResults_train.csv"), row.names = FALSE)
#write.csv(DF, file="myResults_RF_01_importance.csv", row.names = FALSE)

myResults <- read.csv(file="results\\rngRF-all-year_myResults_train2.csv")

myModels <- levels(myResults[,"model"])
myModels <- unique(myResults[,"model"])
myScores <- spread(myResults, model, score)

TIME <- Sys.time()
# confusion matrix @ EER threshold
myF <- NULL
numScores <- myScores
numScores$target <- as.numeric(numScores$target)
opt.thr <- optimal.thresholds(DATA = numScores, model.names = myModels, opt.methods ="MaxKappa")
cat('\n')
print(data.frame(Threshold=t(opt.thr[-1])))
cat('\n')
for (model in myModels) {
  #opt.cut.result <- optimal.cutpoints(X = model, status = "target", tag.healthy = 0, methods = "SpEqualSe", data = myScores, trace = T)
  #threshold <- opt.cut.result$SpEqualSe$Global$optimal.cutoff$cutoff
  threshold <- 0.35
  confusionMatrix <- caret::confusionMatrix(as.factor(myScores[,model]>=threshold),as.factor(as.logical(myScores$target)),positive="TRUE",mode="everything")
  cat(paste0(model,'\n'))
  print(confusionMatrix)
  myF <- c(myF,as.numeric(confusionMatrix$byClass['F1']))
}

# ROC curves
myModelNames <- NULL
i <- 1
performance <- roc.plot(myResults[myResults[,"model"]==myModels[i],],i,traditional=T)
myModelNames[i] <- sprintf('%s AUC=%5.3f',myModels[i],1-performance['pAUC'])
for (i in 2:length(myModels)) {
  performance <- roc.plot(myResults[myResults[,"model"]==myModels[i],],i,traditional=T)
  myModelNames[i] <- sprintf('%s AUC=%5.3f',myModels[i],1-performance['pAUC'])
}
legend(0.4,0.55,myModelNames,lty=rep(1,1,length(myModels)),col=1:length(myModels),cex=0.8)

# DET curves
myModelNames <- NULL
det.plot(NULL,1,xmax=60,ymax=60)
for (i in 1:length(myModels)) {
  performance <- det.plot(myResults[myResults[,"model"]==myModels[i],],nr=i+1)
  myModelNames[i] <- sprintf('%s EER=%5.2f%%',myModels[i],performance['eer'])
}
legend(log(0.042),log(0.28),myModelNames,lty=rep(1,1,length(myModels)),col=2:(length(myModels)+1),cex=0.8)

#install.packages("precrec")
library(precrec)

# Precision-Recall curves
myScores <- spread(myResults, model, score)
myLegend <- paste0(myModels, " F1=", format(myF,digits=3))

msmdat <- mmdata(myScores[,-c(1,2)], myScores[,2], posclass = T, modnames = myLegend)
plot(autoplot(evalmod(msmdat), "PRC", type="b"))






###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###################################### TEST SAMPLE (BEST MODEL) ###########################################################


test_data_2018 <- read.csv(paste0("data\\4_ready_data\\test_jan.csv"), header=T, stringsAsFactors = FALSE, sep=',')
test_data_2018 <- test_data_2018[!names(test_data_2018) %in% c("DATE","ID")]

nrow(test_data_2018)
nrow(test_data_2018[test_data_2018$STATUS==1,])
nrow(test_data_2018[test_data_2018$STATUS==0,])
nrow(test_data_2018[test_data_2018$STATUS==1,])/nrow(test_data_2018)

best_model <- readRDS("rngRF-month-JAN_model__20200507_183712_CV_5.rds")

#myResults_test <- NULL
#best_model <- rf_model

idxY <- which(colnames(test_data_2018) %in% "STATUS")
col_target_ind <- which(colnames(test_data_2018) %in% "STATUS")

model <- rep("rngRF-month_model_JAN",nrow(test_data_2018))
#model <- rep("rngRF-all-year_model_JAN",nrow(test_data_2018))
target <- as.logical(test_data_2018[,idxY])


soft <- predict(best_model,data=test_data_2018[,-idxY])$predictions #rngRF
score <- soft[,2]


#score <- predict(best_model,test_data_2018[,-idxY],type="prob")[,2] # RF

#myResults_test <- NULL
myResults_test <- rbind(myResults_test,data.frame(c(1:nrow(test_data_2018)),model,score,target))

myModels_test <- unique(myResults_test$model)
myScores_test <- spread(myResults_test, model, score)

# confusion matrix @ EER threshold
myF_test <- NULL
numScores_test <- myScores_test
numScores_test$target <- as.numeric(numScores_test$target)

cat('\n')
for (model in myModels_test) {
  threshold_test <- 0.7
  confusionMatrix_test <- caret::confusionMatrix(as.factor(myScores_test[,model]>=threshold_test),as.factor(as.logical(myScores_test$target)),positive="TRUE",mode="everything")
  cat(paste0(model,'\n'))
  print(confusionMatrix_test)
  myF_test <- c(myF_test,as.numeric(confusionMatrix_test$byClass['F1']))
}

# ROC curves
myModelNames_test <- NULL
i <- 1
performance <- roc.plot(myResults_test[myResults_test[,"model"]==myModels_test[i],],i,traditional=T)
myModelNames_test[i] <- sprintf('%s AUC=%5.3f',myModels_test[i],1-performance['pAUC'])
for (i in 2:length(myModels_test)) {
  performance <- roc.plot(myResults_test[myResults_test[,"model"]==myModels_test[i],],i,traditional=T)
  myModelNames_test[i] <- sprintf('%s AUC=%5.3f',myModels_test[i],1-performance['pAUC'])
}
legend(0.4,0.55,myModelNames_test,lty=rep(1,1,length(myModels_test)),col=1:length(myModels_test),cex=0.8)

# DET curves
myModelNames_test <- NULL
det.plot(NULL,1,xmax=60,ymax=60)
for (i in 1:length(myModels_test)) {
  performance <- det.plot(myResults_test[myResults_test[,"model"]==myModels_test[i],],nr=i+1)
  myModelNames_test[i] <- sprintf('%s EER=%5.2f%%',myModels_test[i],performance['eer'])
}
legend(log(0.042),log(0.28),myModelNames_test,lty=rep(1,1,length(myModels_test)),col=2:(length(myModels_test)+1),cex=0.8)

#install.packages("precrec")
library(precrec)

# Precision-Recall curves
myScores_test <- spread(myResults_test, model, score)
myLegend_test <- paste0(myModels_test, " F1=", format(myF_test,digits=3))

msmdat <- mmdata(myScores_test[,-c(1,2)], myScores_test[,2], posclass = T, modnames = myLegend_test)
plot(autoplot(evalmod(msmdat), "PRC", type="b"))


write.csv(myResults, file=paste0(using_model,"_myResults_test.csv"), row.names = FALSE)




















###### DOODLING AROUND ######
# res <- cor(fullData[,1:180])
# res <- round(res,2)
# corrplot(res)
# 
# str(fullData)
# 
# partData <- fullData[,1:227]
# x <- model.matrix(STATUS~.,data=partData)
# x <- x[,-1]
# glmnet1 <- cv.glmnet(x=x,y=fullData$STATUS,type.measure = 'mse',nfolds=5,alpha=0.5, family="binomial")
# 
# glmnet_lasso_cv <- cv.glmnet(x,fullData$STATUS, alpha=1, family="binomial")
# glmnet_lasso <- glmnet(x,fullData$STATUS, alpha=1, family="binomial")
# plot(glmnet_lasso_cv)
# plot(glmnet_lasso, xvar = "lambda")
# abline(v = log(glmnet_lasso_cv$lambda.min), col = "red", lty = "dashed")
# abline(v = log(glmnet_lasso_cv$lambda.1se), col = "red", lty = "dashed")
# 
# abline(v = log(glmnet_lasso$lambda), col = "red", lty = "dashed")
# 
# plot(coef(glmnet_lasso_cv, s = "lambda.1se"))
# 
# coef(glmnet_lasso_cv, s = "lambda.1se") %>%
#   tidy() %>%
#   filter(row != "(Intercept)") %>%
#   ggplot(aes(value, reorder(row, value), color = value > 0)) +
#   geom_point(show.legend = FALSE) +
#   ggtitle("Influential variables") +
#   xlab("Coefficient") +
#   ylab(NULL)
# 
# ggplot(aes(value, reorder(row, value), color = value > 0))
# options("scipen"=100, "digits"=4)
# c
# plot(glmnet1)
# c<-coef(glmnet1,s='lambda.min',exact=TRUE)
# 
# inds<-which(c!=0)
# variabless<-row.names(c)[inds]
# variabless<-variables[variables %ni% '(Intercept)']

#############################