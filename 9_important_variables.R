

full_var_imp <- NULL

full_var_imp <- data.frame(matrix(ncol=0, nrow=212))

var_imp <- read.csv(paste0("results\\__DIFFERENT_DISBALANCE__\\",disb_rate,"\\var_importance\\rngRF_DISB_",disb_rate,"_var_importance_",fold_value,"_.csv"), head=TRUE)
row.names(full_var_imp) <- var_imp$var

for(disb_rate in disbalance_ratios_set){
  for(fold_value in 1:5){
    var_imp <- read.csv(paste0("results\\__DIFFERENT_DISBALANCE__\\",disb_rate,"\\var_importance\\rngRF_DISB_",disb_rate,"_var_importance_",fold_value,"_.csv"), head=TRUE)
    var_imp["X"] <- NULL
    col_name <- paste0("DISB_",disb_rate,"_CV_",fold_value)
    colnames(var_imp)[2] <- col_name
    
    full_var_imp <- cbind(full_var_imp, var_imp[2])
  }
}


for(month_num in 0:12){
  fl <- list.files(paste0("results\\",month_num,"\\var_importance\\"))
  
  for(i in 1:length(fl)){
    
    fileName <- fl[i]
    filePath <- paste0("results\\",month_num,"\\var_importance\\",fileName)
    
    var_imp <- read.csv(filePath, header=T, stringsAsFactors = FALSE, sep=',')
    var_imp["X"] <- NULL
    col_name <- paste0("MONTH_",month_num,"_CV_",i)
    colnames(var_imp)[2] <- col_name
    
    full_var_imp <- cbind(full_var_imp, var_imp[2])
  }
}


write.csv(full_var_imp, file =paste0("results\\VARIABLE_IMPORTANCE_ALL_1.csv"))
