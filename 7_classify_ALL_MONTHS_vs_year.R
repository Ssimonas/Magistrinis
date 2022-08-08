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

# month_string<- "JAN"
# fill_summary_result_table(confusionMatrix, month_string, "MTrn")

fill_summary_result_table <- function(cMatrix, month, row_prefix){
  ## row_prefixes: MTrn; MTst; YTst
  summary_results[paste0(row_prefix,"_Sensitivity"),month] <<- cMatrix$byClass[[1]]
  summary_results[paste0(row_prefix,"_Specificity"),month] <<- cMatrix$byClass[[2]]
  summary_results[paste0(row_prefix,"_Precision"),month] <<- cMatrix$byClass[[5]]
  summary_results[paste0(row_prefix,"_Recall"),month] <<- cMatrix$byClass[[6]]
  summary_results[paste0(row_prefix,"_F1"),month] <<- cMatrix$byClass[[7]]
  summary_results[paste0(row_prefix,"_Balanced_Acc"),month] <<- cMatrix$byClass[[11]]
  
  return(summary_results)
}

month.abb <- toupper(month.abb)
month.abb[1]  # Returns "JAN"
which(month.abb == "MAR")  # Returns 3

# multi-thread computing
no_cores <- detectCores()
cl <- makeCluster(no_cores)
registerDoParallel(cl)

# rngRF parameters
#mtry <- floor(sqrt(ncol(fullData)-1))
det.min.node.size <- 100
det.mtry <- 50
ntree <- 200
threshold <- 0.7
threshold <- 0.35
threshold_test <- threshold

# cross-fold validation
k_folds <- 5

# 1:n healthy companies to 1 bankrupt
disb_control <- 10

summary_results <- data.frame(matrix(ncol=12, nrow=18))
cnames <- month.abb
colnames(summary_results) <- cnames

rownames(summary_results) <- c("MTrn_Sensitivity", "MTrn_Specificity", "MTrn_Precision",
                               "MTrn_Recall", "MTrn_F1", "MTrn_Balanced_Acc",
                               "MTst_Sensitivity", "MTst_Specificity", "MTst_Precision",
                               "MTst_Recall", "MTst_F1", "MTst_Balanced_Acc",
                               "YTst_Sensitivity", "YTst_Specificity", "YTst_Precision",
                               "YTst_Recall", "YTst_F1", "YTst_Balanced_Acc")

# for every month

for(month_num in 1:12){

  ## result file paths
  main_result_path <- paste0("results\\",month_num,'\\')
  model_path <- paste0(main_result_path,"models\\")
  #result_path <- paste0(main_result_path,"results\\")
  var_imp_path <- paste0(main_result_path,"var_importance\\")
  #####

  month_string <- ifelse(month_num > 0, month.abb[month_num], "ALL-YEAR")
  cat("---------------- STARTED PROCESSING -",month_string,"\n")
  
  model_description <- paste0("rngRF_",month_string)
  
  TIME <- Sys.time()
  if (month_num == 0){
    trainData <- read.csv(paste0("data\\4_ready_data\\train_all_year.csv"), header=T, stringsAsFactors = FALSE, sep=',')
  } else {
    trainData <- read.csv(paste0("data\\3_data_to_rows\\MONTHLY\\",month_num,"\\train_",month_num,"_no2018.csv"), header=T, stringsAsFactors = FALSE, sep=',')
  }  
  cat("[",month_string,"] Data files are read.\n")
  print(Sys.time()-TIME)
  
  TIME <- Sys.time()
  set.seed(101)
  rand <- sample(nrow(trainData))
  trainData <- trainData[rand,]
  
  trainData <- trainData[!names(trainData) %in% c("DATE","ID")]
  #idxY <- which(colnames(trainData) == "STATUS")
  target_col_name <-"STATUS"
  target_col_ind <- which(colnames(trainData) == target_col_name)
  
  trainData[,target_col_ind] <- factor(as.character(trainData[,target_col_ind]),labels=c("False","True")) # This works
  
  cat("[",month_string,"] Bankrupt companies:",length(trainData$STATUS[trainData$STATUS == "True"]))
  cat("[",month_string,"] True disbalance:",length(trainData$STATUS[trainData$STATUS == "True"])/nrow(trainData))
  
  if (month_num > 0){
    trainData_healthy <- trainData[ sample( which( trainData$STATUS == "False" ) , (nrow(trainData[trainData$STATUS=="True",])*disb_control) ), ]
    nrow(trainData_healthy)
    trainData <- rbind(trainData_healthy,trainData[trainData$STATUS=="True",])
    cat("[",month_string,"] Disbalance after data downsampling ( 1 :",disb_control,"):",length(trainData$STATUS[trainData$STATUS == "True"])/nrow(trainData))
  }
  
  gc()
  month_results <- NULL
  fold_prediction_error <- NULL
  factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)], as.numeric))
  
  ## Caret package
  folds <- createFolds(trainData[,target_col_name],k_folds)

  TIME <- Sys.time()
  
  fullData <- trainData
  
  for (current_fold in 1:length(folds)) {

    ## All indexes of one fold (for validation)
    validation_ind <- folds[[current_fold]]
    
    train_ind <- as.logical(rep(1,1,nrow(fullData)))
    train_ind[validation_ind] <- FALSE
    train_ind <- which(train_ind)
    target <- as.logical(fullData[validation_ind,target_col_ind])
    
    Y <- as.factor(fullData[train_ind,target_col_ind])
    
    fold_trainData <- fullData[train_ind,]
    fold_testData <- fullData[validation_ind,-target_col_ind] # Test data with no target variable
    
    cat("[",month_string,"] Fold:",current_fold," Out of:",k_folds," Model:",model_description,"\n")
    
    ######### CLASSIFICATION MODEL ##########
    task <- makeClassifTask(data = fold_trainData, target = "STATUS", positive = "True")

    rf_model <- ranger(data = fold_trainData, classification = T,
                       dependent.variable.name = target_col_name, num.trees = ntree,
                       mtry = det.mtry, min.node.size = det.min.node.size,
                       write.forest = T, probability = T, replace = F,
                       num.threads = no_cores, verbose = T, importance = "impurity")
    
    cat("[",paste0(month_string,"_F",current_fold),"] Model trained.\n")
    
    print(rf_model)
    fold_prediction_error <- c(fold_prediction_error,rf_model$prediction.error)
    
    model <- rep(model_description,length(target))
    soft <- predict(rf_model,data=fold_testData)$predictions
    score <- soft[,2]
    cat("[",paste0(month_string,"_F",current_fold),"] Predicted.\n")
    
    month_results <- rbind(month_results,data.frame(validation_ind,model,score,target))
    
    ### 1) SAVE MODEL
    #saveRDS(rf_model, paste0(model_path,model_description,"_model_",format(Sys.time(),'_%Y%m%d_%H%M%S'),"_CV_",current_fold,".rds"))
    saveRDS(rf_model, paste0(model_path,model_description,"_model_CV_",current_fold,".rds"))
    
    ### 2) SAVE VAR IMPORTANCE
    DF<-as.data.frame(rf_model$variable.importance)
    setDT(DF, keep.rownames = "newname")
    colnames(DF) <- c("var", "importance")
    write.csv(DF, file=paste0(var_imp_path,model_description,"_var_importance_",current_fold,"_.csv"), row.names = TRUE)
    
    cat("[",paste0(month_string,"_F",current_fold),"] Model and var importance saved.\n")
    
    rm(rf_model)
  }
  print(Sys.time()-TIME)
  cat("ALL FOLDS FINISHED. WRITING RESULTS TO FILE. TRAIN CONFUSION MATRIX:\n")
  
  ### 3) SAVE MAIN RESULTS
  write.csv(month_results, file=paste0(main_result_path,model_description,"_results.csv"), row.names = FALSE)
  
  ## Fold of best model:
  which(fold_prediction_error == min(fold_prediction_error))
  
  
  month_Scores <- spread(month_results, model, score)
  
  TIME <- Sys.time()
  
  ### confusion matrix @ EER threshold
  month_Scores_num <- month_Scores
  month_Scores_num$target <- as.numeric(month_Scores_num$target)
  opt.thr <- optimal.thresholds(DATA = month_Scores_num, model.names = model_description, opt.methods ="MaxKappa")
  cat('\n')
  print(data.frame(Threshold=t(opt.thr[-1])))
  
  confusionMatrix <- caret::confusionMatrix(as.factor(month_Scores[,model_description]>=threshold),
                                            as.factor(as.logical(month_Scores$target)),positive="TRUE",mode="everything")
  cat(paste0(model_description,' ------- TRAINING \n'))
  print(confusionMatrix)
  
  write.csv(confusionMatrix$table, file=paste0(main_result_path,model_description,"_train_CONF_MATIX.csv"), row.names = TRUE)
  
  if (month_num == 0){
    YEARLY_TRAINED_MODEL <- paste0("rngRF_ALL-YEAR_model_CV_",which(fold_prediction_error == min(fold_prediction_error)))
    next
  }
  ###### SAVE STATS
  ## "MTrn_Sensitivity", "MTrn_Specificity", "MTrn_Precision",
  ## "MTrn_Recall", "MTrn_F1", "MTrn_Balanced_Acc"
  fill_summary_result_table(confusionMatrix, month_string, "MTrn")
  # summary_results["MTrn_Sensitivity",month_string] <- confusionMatrix$byClass[[1]]
  # summary_results["MTrn_Specificity",month_string] <- confusionMatrix$byClass[[2]]
  # summary_results["MTrn_Precision",month_string] <- confusionMatrix$byClass[[5]]
  # summary_results["MTrn_Recall",month_string] <- confusionMatrix$byClass[[6]]
  # summary_results["MTrn_F1",month_string] <- confusionMatrix$byClass[[7]]
  # summary_results["MTrn_Balanced_Acc",month_string] <- confusionMatrix$byClass[[11]]
  ######
  
  
  ##############################################################################################################################
  ##############################################################################################################################
  ############## TESTING ON 2018 MONTH DATA
  rf_model_month <- readRDS(paste0(model_path,model_description,"_model_CV_",which(fold_prediction_error == min(fold_prediction_error)),".rds"))
  rf_model_year <- readRDS(paste0("results\\0\\models\\",YEARLY_TRAINED_MODEL,".rds"))
  ##rf_model_year <- readRDS(paste0("results\\rngRF_YEAR-trained_model.rds"))
  testData <- read.csv(paste0("data\\3_data_to_rows\\MONTHLY\\",month_num,"\\test_",month_num,"_only2018.csv"), header=T, stringsAsFactors = FALSE, sep=',')
  testData <- testData[!names(testData) %in% c("DATE","ID")]
  col_target_ind <- which(colnames(testData) %in% "STATUS")
  
  ### MONTH
  model <- rep(model_description,nrow(testData))
  target <- as.logical(testData[,col_target_ind])
  soft <- predict(rf_model_month,data=testData[,-col_target_ind])$predictions #rngRF
  score <- soft[,2]
  myResults_test <- NULL
  myResults_test <- rbind(myResults_test,data.frame(c(1:nrow(testData)),model,score,target))
  
  ### YEAR
  model <- rep("rngRF_YEAR-trained",nrow(testData))
  target <- as.logical(testData[,col_target_ind])
  soft <- predict(rf_model_year,data=testData[,-col_target_ind])$predictions #rngRF
  score <- soft[,2]
  myResults_test <- rbind(myResults_test,data.frame(c(1:nrow(testData)),model,score,target))
  
  myModels_test <- unique(myResults_test$model)
  myScores_test <- spread(myResults_test, model, score)
  
  # confusion matrix @ EER threshold
  myF_test <- NULL
  numScores_test <- myScores_test
  numScores_test$target <- as.numeric(numScores_test$target)
  
  iterator <- 0
  for (model in myModels_test) {
    confusionMatrix <- caret::confusionMatrix(as.factor(numScores_test[,model]>=threshold_test),as.factor(as.logical(numScores_test$target)),positive="TRUE",mode="everything")
    cat(paste0(model,'\n'))
    print(confusionMatrix)
    
    if (iterator == 0){
      ###### SAVE STATS
      fill_summary_result_table(confusionMatrix, month_string, "MTst")
      write.csv(confusionMatrix$table, file=paste0(main_result_path,model_description,"_month_test_CONF_MATIX.csv"), row.names = TRUE)
    } else {
      ###### SAVE STATS
      fill_summary_result_table(confusionMatrix, month_string, "YTst")
      write.csv(confusionMatrix$table, file=paste0(main_result_path,model_description,"_year_test_CONF_MATIX.csv"), row.names = TRUE)
    }
    
    iterator <- iterator + 1
    
    myF_test <- c(myF_test,as.numeric(confusionMatrix$byClass['F1']))
  }
  
  print(summary_results)
  
  # ROC curves
  png(file=paste0(main_result_path,"year_month_",month_string,"_ROC.png"), )
  myModelNames_test <- NULL
  i <- 1
  performance <- roc.plot(myResults_test[myResults_test[,"model"]==myModels_test[i],],i,traditional=T)
  myModelNames_test[i] <- sprintf('%s AUC=%5.3f',myModels_test[i],1-performance['pAUC'])
  for (i in 2:length(myModels_test)) {
    performance <- roc.plot(myResults_test[myResults_test[,"model"]==myModels_test[i],],i,traditional=T)
    myModelNames_test[i] <- sprintf('%s AUC=%5.3f',myModels_test[i],1-performance['pAUC'])
  }
  legend(0.4,0.55,myModelNames_test,lty=rep(1,1,length(myModels_test)),col=1:length(myModels_test),cex=0.8)
  dev.off()
  
  
  # DET curves
  png(file=paste0(main_result_path,"year_month_",month_string,"_DET.png"), )
  myModelNames_test <- NULL
  det.plot(NULL,1,xmax=60,ymax=60)
  for (i in 1:length(myModels_test)) {
    performance <- det.plot(myResults_test[myResults_test[,"model"]==myModels_test[i],],nr=i+1)
    myModelNames_test[i] <- sprintf('%s EER=%5.2f%%',myModels_test[i],performance['eer'])
  }
  legend(log(0.042),log(0.28),myModelNames_test,lty=rep(1,1,length(myModels_test)),col=2:(length(myModels_test)+1),cex=0.8)
  dev.off()
  
  #install.packages("precrec")
  library(precrec)
  
  # Precision-Recall curves
  myScores_test <- spread(myResults_test, model, score)
  myLegend_test <- paste0(myModels_test, " F1=", format(myF_test,digits=3))
  
  msmdat <- mmdata(myScores_test[,-c(1,2)], myScores_test[,2], posclass = T, modnames = myLegend_test)
  
  png(file=paste0(main_result_path,"year_month_",month_string,"_PR.png"), )
  plot(autoplot(evalmod(msmdat), "PRC", type="b"))
  dev.off()
  
  #plot(autoplot(evalmod(msmdat), "PRC", type="b"))
  write.csv(myResults_test, file=paste0(main_result_path,model_description,"_",format(Sys.time(),'_%Y%m%d_%H%M%S'),"_results_test_month_and_year.csv"), row.names = FALSE)  
  
}
write.csv(summary_results, file =paste0("results\\summary_",format(Sys.time(),'_%Y%m%d_%H%M%S'),"_results_metrics.csv"))




