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
library(neuralnet)
library(nnet)
library(corrplot)


using_model <- "rngRF-month-JAN-big-imb"

# 0.001336 disbalance
fullData <- read.csv(paste0("data\\4_ready_data\\train_only_jan.csv"), header=T, stringsAsFactors = FALSE, sep=',')
# 10 healthy companies for every bankrupt
fullData <- read.csv(paste0("data\\4_ready_data\\train_all_year.csv"), header=T, stringsAsFactors = FALSE, sep=',')

nrow(fullData[fullData$STATUS == 1,])
fullData <- fullData[!names(fullData) %in% c("DATE","ID")]
idxY <- which(colnames(fullData) %in% "STATUS")
col_target_ind <- which(colnames(fullData) %in% "STATUS")

fullData[,col_target_ind] <- factor(as.character(fullData[,col_target_ind]),labels=c("False","True")) # This works

length(fullData$STATUS[fullData$STATUS == "True"])/length(fullData$STATUS[fullData$STATUS == "False"])

#length(fullData$STATUS[fullData$STATUS == "True"])*3 + length(fullData$STATUS[fullData$STATUS == "False"])/(2*length(fullData$STATUS[fullData$STATUS == "False"]))
balancedFullData <- SMOTE(STATUS ~ ., fullData, perc.under = 1340) #Disbalance ~ 0.04


length(fullData$STATUS[fullData$STATUS == "True"])
length(balancedFullData$STATUS[balancedFullData$STATUS == "True"])
length(balancedFullData$STATUS[balancedFullData$STATUS == "False"])
#length(balancedFullData$STATUS[balancedFullData$STATUS == "True"])/length(balancedFullData$STATUS[balancedFullData$STATUS == "False"])
length(balancedFullData$STATUS[balancedFullData$STATUS == "True"])/nrow(balancedFullData)
row.names(balancedFullData) <- 1:nrow(balancedFullData)
fullData <- balancedFullData
fullData <- fullData[complete.cases(fullData),]


#which(fullData$STATUS == 1)
#fullData <- fullData[1:10000,]
#nrow(fullData)

#test_data_2018 <- test_data_2018[!names(test_data_2018) %in% c("DATE","ID")]
#fullData <- fullData %>% select(!matches("n_PVM_MEN|VADOV_POK|FA_vald_dalis|vidut_parduot_pvm"))


set.seed(101)
rand <- sample(nrow(fullData))
fullData <- fullData[rand,]

nrow(fullData[fullData$STATUS=="True",])/nrow(fullData)


#fullData_healthy <- sample(fullData[fullData$STATUS=="False",],nrow(fullData[fullData$STATUS=="True",])*10)

#fullData_healthy <- fullData[ sample( which( fullData$STATUS == "False" ) , (nrow(fullData[fullData$STATUS=="True",])*10) ) , ]

#nrow(fullData_healthy)

#fullData <- rbind(fullData_healthy,fullData[fullData$STATUS=="True",])


set.seed(101)
rand <- sample(nrow(fullData))
fullData <- fullData[rand,]

sapply(fullData, function(x){sum(is.na(x))})

#fullData <- fullData[ !duplicated(fullData[, c("ID")], fromLast=T),]

nrow(fullData)
nrow(fullData[fullData$STATUS == "True",])
nrow(fullData[fullData$STATUS == "True",])/nrow(fullData)

#test_data <- read.csv(paste0("data\\4_ready_data\\test.csv"), header=T, stringsAsFactors = FALSE, sep=',')
## Part of data set
#rawData <- fullData
#fullData <- rawData

no_cores <- detectCores()
cl <- makeCluster(no_cores)
registerDoParallel(cl)

# classical random forest settings
#mtry <- floor(sqrt(ncol(fullData)-1))
mtry <- 50
#ntree <- 100
ntree <- 200
# ptree <- 50 #only RF
min.node.size <- 100

### random forest settings for tuneRanger
#tuneParams <- c("mtry", "min.node.size")
#itersWarm <- 8
#itersTune <- 15
tuneRangerMeasure <- list(mlr::logloss)
tuneRangerMeasureTxt <- "logloss" # auc ok if no class imbalance
kCV <- 3 #5?

myFolds <- createFolds(fullData[,"STATUS"],kCV)

factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)], as.numeric))

gc()
myResults <- NULL

colY <-"STATUS"

TIME <- Sys.time()

for (i in 1:kCV) {
  
  tstInd <- myFolds[[i]]
  trnIdx <- as.logical(rep(1,1,nrow(fullData)))
  trnIdx[tstInd] <- FALSE
  trnInd <- which(trnIdx)
  target <- as.logical(fullData[tstInd,idxY])

  Y <- as.factor(fullData[trnInd,idxY])
  
  classProps <- as.numeric(prop.table(table(Y)))
  sampfrac <- 2*min(classProps)*0.632
  W <- rep(NA, length(Y))
  W[Y=="False"] <- classProps[2]
  W[Y=="True"] <- classProps[1]
  
  trainData <- fullData[trnInd,]
  testData <- fullData[tstInd,-idxY]
  
  
  ######################################################################################################
  ##### RANDOM FOREST - SMOTE #####
  # cat(sprintf("\nCV fold %d out of %d / Random Forest + SMOTE\n", i, kCV))
  # 
  # #balancedFullData <- SMOTE(STATUS ~ ., trainData, perc.over = 100, k = 2000, perc.under = 5000) #Disbalance ~ 0.04
  # balancedFullData <- SMOTE(STATUS ~ ., trainData, perc.over = 100) #Disbalance ~ 0.04
  # #balancedFullData <- SMOTE(STATUS ~ ., trainData, perc.over = 100, k = 10000, perc.under = 100) #Disbalance ~ 0.04
  # 
  # length(balancedFullData$STATUS[balancedFullData$STATUS == "True"])/length(balancedFullData$STATUS[balancedFullData$STATUS == "False"])
  # nrow(balancedFullData)
  # row.names(balancedFullData) <- 1:nrow(balancedFullData)
  # 
  # trainData <- balancedFullData
  # trainData <- trainData[complete.cases(trainData),]
  # nrow(trainData)
  # Y <- as.factor(trainData[,idxY])
  # trainData <- trainData[,-idxY]
  # 
  # model_classwt <- prop.table(table(Y))
  # # rf_model <- randomForest(myData[trnInd,-idxY], Y, ntree = ntree, mtry = mtry, classwt = model_classwt, cutoff = model_classwt, strata = Y, replace = FALSE, importance=FALSE, do.trace = ptree)
  # rf_model <- tuneRF(trainData, Y, mtryStart = mtry, ntreeTry = ntree,
  #                    stepFactor = 2, improve = 0.01, plot=FALSE, doBest=T,
  #                    strata = Y, replace = FALSE, importance = TRUE, do.trace = ptree)
  # print(rf_model)
  # model <- rep("RF-SMOTE-perc-over100",length(target))
  # score <- predict(rf_model,testData,type="prob")[,2]
  # myResults <- rbind(myResults,data.frame(tstInd,model,score,target))
  # 
  # saveRDS(rf_model, paste0("RF-SMOTE-perc-over100_",format(Sys.time(),'_%Y%m%d_%H%M%S'),"_CV_",i,".rds"))
  # 
  # varImpPlot(rf_model, n.var = 200, scale=TRUE, type = 2)
  # var_importance <- rf_model$importance
  # write.csv(var_importance, file=paste0("myResults_RF-SMOTE-perc-over100_importance_",i,"_.csv"), row.names = TRUE)
  
  
  
  ######################################################################################################
  ######### RANGER RANDOM FOREST - SMOTE########
  cat(sprintf("\nCV fold %d out of %d / Ranger Random Forest - SMOTE\n", i, kCV))
  
  ## January
  #balancedFullData <- SMOTE(STATUS ~ ., trainData, perc.over = 500, perc.under = 600) #Disbalance ~ 0.04
  
  ## All year
  balancedFullData <- SMOTE(STATUS ~ ., trainData, perc.over = 100, perc.under = 200) #Disbalance ~ 0.04
  
  #balancedFullData <- SMOTE(STATUS ~ ., trainData, perc.over = 100, k = 10000, perc.under = 100) #Disbalance ~ 0.04
  # length(trainData$STATUS[trainData$STATUS == "True"])
  # length(balancedFullData$STATUS[balancedFullData$STATUS == "True"])
  # length(balancedFullData$STATUS[balancedFullData$STATUS == "True"])/length(balancedFullData$STATUS[balancedFullData$STATUS == "False"])
  # nrow(balancedFullData)
  # row.names(balancedFullData) <- 1:nrow(balancedFullData)
  # 
  # trainData <- balancedFullData
  # trainData <- trainData[complete.cases(trainData),]
  # nrow(trainData)
  # Y <- as.factor(trainData[,idxY])
  
  task <- makeClassifTask(data = trainData, target = "STATUS", positive = "True")
  #estimateTimeTuneRanger(task, iters = itersWarm + itersTune, num.threads = no_cores, num.trees = ntree)
  cat('\n')
  res <- tuneRanger(task, measure = tuneRangerMeasure, 
                    iters = itersTune, 
                    iters.warmup = itersWarm, 
                    num.trees = ntree, 
                    parameters = list(replace = F, verbose = T), 
                    tune.parameters = tuneParams, 
                    num.threads = no_cores, build.final.model = F)
  
  
  idx <- which.min(res$results[,tuneRangerMeasureTxt])
  opt.mtry <- res$results$mtry[idx]
  opt.min.node.size <- res$results$min.node.size[idx]
  rf_model <- ranger(data = trainData, classification = T,
                     dependent.variable.name = colY, num.trees = ntree,
                     mtry = opt.mtry, min.node.size = opt.min.node.size,
                     write.forest = T, probability = T, replace = F,
                     num.threads = no_cores, verbose = T, importance = "impurity")
  print(rf_model)
  ##model <- rep("rngRF-all-year",length(target))
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
  
  ######################################################################################
  ######### RANDOM FOREST ########
  # Y <- as.factor(fullData[trnInd,idxY])
  # cat(sprintf("\nCV fold %d out of %d / Random Forest\n", i, kCV))
  # model_classwt <- prop.table(table(Y))
  # # rf_model <- randomForest(myData[trnInd,-idxY], Y, ntree = ntree, mtry = mtry, classwt = model_classwt, cutoff = model_classwt, strata = Y, replace = FALSE, importance=FALSE, do.trace = ptree)
  # rf_model <- tuneRF(fullData[trnInd,-idxY], Y, mtryStart = mtry, ntreeTry = ntree,
  #                    stepFactor = 2, improve = 0.01, plot=FALSE, doBest=T,
  #                    strata = Y, replace = FALSE, importance = TRUE, do.trace = ptree)
  # print(rf_model)
  # model <- rep("RF",length(target))
  # score <- predict(rf_model,fullData[tstInd,-idxY],type="prob")[,2]
  # myResults <- rbind(myResults,data.frame(tstInd,model,score,target))
  # 
  # saveRDS(rf_model, paste0("RF_model_",format(Sys.time(),'_%Y%m%d_%H%M%S'),"_CV_",i,".rds"))
  # 
  # varImpPlot(rf_model, n.var = 160, scale=TRUE)
  # var_importance <- rf_model$importance
  # write.csv(var_importance, file=paste0("myResults_RF_importance_",i,"_.csv"), row.names = TRUE)
  ###############################################################
  # TIME <- Sys.time()

  ######################################################################################
  ######### RANGER RANDOM FOREST - MAJORITY DOWNSAMPLING ########
  # cat(sprintf("\nCV fold %d out of %d / Ranger Random Forest - majority downsampling\n", i, kCV))
  # task <- makeClassifTask(data = fullData[trnInd,], weights = W, target = "STATUS", positive = "True")
  # #estimateTimeTuneRanger(task, iters = itersWarm + itersTune, num.threads = no_cores, num.trees = ntree)
  # cat('\n')
  # res <- tuneRanger(task, measure = tuneRangerMeasure, iters = itersTune, iters.warmup = itersWarm, num.trees = ntree, parameters = list(replace = F, sample.fraction = sampfrac, verbose = T), tune.parameters = tuneParams, num.threads = no_cores, build.final.model = F)
  # idx <- which.min(res$results[,tuneRangerMeasureTxt])
  # opt.mtry <- res$results$mtry[idx]
  # opt.min.node.size <- res$results$min.node.size[idx]
  # 
  # rf_model <- ranger(data = fullData[trnInd,], classification = T,
  #                    dependent.variable.name = "STATUS", num.trees = ntree,
  #                    mtry =  opt.mtry, min.node.size = opt.min.node.size,
  #                    write.forest = T, probability = T, case.weights = W,
  #                    replace = F, sample.fraction = sampfrac,
  #                    num.threads = no_cores, verbose = T, importance = "impurity")
  # print(rf_model)
  # 
  # saveRDS(rf_model, paste0("rngRF-md_model_",format(Sys.time(),'_%Y%m%d_%H%M%S'),"_CV_",i,".rds"))
  # 
  # model <- rep("rngRF-md",length(target))
  # soft <- predict(rf_model,data=fullData[tstInd,-idxY])$predictions
  # score <- soft[,2]
  # myResults <- rbind(myResults,data.frame(tstInd,model,score,target))
  # 
  # DF<-as.data.frame(rf_model$variable.importance)
  # setDT(DF, keep.rownames = "newname")
  # colnames(DF) <- c("var", "importance")
  # write.csv(DF, file=paste0("myResults_RNG_RF_importance_",i,"_.csv"), row.names = TRUE)
  # # 
  # # 
  # ggplot(DF, aes(x=reorder(var,importance), y=importance,fill=importance))+
  #   geom_bar(stat="identity", position="dodge")+ coord_flip()+
  #   theme(text = element_text(size=7))+
  #   ylab("Variable Importance")+
  #   xlab("")+
  #   ggtitle("Information Value Summary")+
  #   scale_fill_gradient(low="red", high="blue")
}

Sys.time()-TIME

# some_results <- read.csv(paste0("Run_04__new_data_3RF\\myResults_RF-SMOTE_RF_RNG-RF_NEURALNET_02.csv"), header=T, stringsAsFactors = FALSE, sep=',')
# myResults <- some_results

################# EVALUATION ###################
myResults_back <- myResults

write.csv(myResults, file=paste0(using_model,"_myResults.csv"), row.names = FALSE)
#write.csv(DF, file="myResults_RF_01_importance.csv", row.names = FALSE)

myModels <- levels(myResults[,"model"])
myModels <- unique(myResults[,"model"])
myScores <- spread(myResults, model, score)

TIME <- Sys.time()
# confusion matrix @ EER threshold
myF <- NULL
numScores <- myScores
numScores$target <- as.numeric(numScores$target)
opt.thr <- optimal.thresholds(DATA = numScores, model.names = myModels, opt.methods ="MaxSens+Spec")
cat('\n')
print(data.frame(Threshold=t(opt.thr[-1])))
cat('\n')
for (model in myModels) {
  #opt.cut.result <- optimal.cutpoints(X = model, status = "target", tag.healthy = 0, methods = "SpEqualSe", data = myScores, trace = T)
  #threshold <- opt.cut.result$SpEqualSe$Global$optimal.cutoff$cutoff
  threshold <- opt.thr[1,model]
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

best_model <- readRDS("rngRF-all-year_model__20200506_165707_CV_2.rds")
best_model <- readRDS("rngRF-month-JAN_model__20200506_170740_CV_2.rds")
best_model <- readRDS("rngRF-month-JAN-big-imb_model__20200506_191709_CV_1.rds")

#myResults_test <- NULL
#best_model <- rf_model

idxY <- which(colnames(test_data_2018) %in% "STATUS")
col_target_ind <- which(colnames(test_data_2018) %in% "STATUS")

model <- rep("rngRF-month-JAN-big-imb",nrow(test_data_2018))
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
opt.thr <- optimal.thresholds(DATA = numScores_test, model.names = myModels_test, opt.methods = "MaxKappa")
cat('\n')
print(data.frame(Threshold=t(opt.thr[-1])))

mean_thr <- (opt.thr$`rngRF-all-year`+opt.thr$`rngRF-month-JAN`)/2
mean_thr <- 0.765
mean_thr <- opt.thr$`rngRF-all-year`
mean_thr <- opt.thr$`rngRF-month-JAN`
opt.thr$`rngRF-all-year` <- mean_thr
opt.thr$`rngRF-month-JAN` <- mean_thr

# thresholds_raw <- seq(from = 0, to = 1, by = 0.01)
# max_f1_thr <- 0
# for (model in myModels_test) {
#   for (thr in thresholds_raw) {
#     confusionMatrix_test <- caret::confusionMatrix(as.factor(myScores_test[,model]>=thr),as.factor(as.logical(myScores_test$target)),positive="TRUE",mode="everything")
#     if (!is.na(confusionMatrix_test$byClass[["F1"]])){
#       if (confusionMatrix_test$byClass[["F1"]] > max_f1_thr){
#         max_f1_thr <- confusionMatrix_test$byClass[["F1"]]
#         best_thresh <- thr
#       }
#     }
#   }
#   cat(model,"\t",best_thresh,"\n")
#   max_f1_thr <- 0
# }


cat('\n')
for (model in myModels_test) {
  threshold_test <- opt.thr[1,model]
  #threshold_test <- 0.032
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