# Combine healthy and bankrupt data sets

YEAR_DATA = c(2014,2015,2016,2017) # fix 2018

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#year <- 2017

for (year in YEAR_DATA){

  cat(paste0("[",year,"] Reading data files.\n"))
  myDataHealthy <- read.csv(paste0("data\\3_data_to_rows\\HEALTHY_",year,".csv"), header=T, stringsAsFactors = FALSE, sep=',')
  myDataBankrupt <- read.csv(paste0("data\\3_data_to_rows\\BANKR_",year,".csv"), stringsAsFactors = FALSE, sep=',')
  
  nrow(myDataHealthy)
  nrow(myDataBankrupt)
  
  if (year == 2014){
    myDataHealthy <- myDataHealthy[1:(nrow(myDataHealthy)/2),]
  }
  
  myDataFull <- rbind(myDataHealthy,myDataBankrupt)
  nrow(myDataFull[myDataFull$STATUS == 1,])
  nrow(myDataFull[myDataFull$STATUS == 0,])
  
  sum(is.na(myDataHealthy$STATUS))
  set.seed(101)
  
  rand <- sample(nrow(myDataFull))
  myDataFull <- myDataFull[rand,]

  nrow(myDataBankrupt[myDataBankrupt$STATUS == 1,])
  nrow(myDataFull)
  
  cat(paste0("[",year,"] Before - Amount of bankrupt companies:",nrow(myDataBankrupt),"\n"))
  cat(paste0("[",year,"] Before - Amount of healthy companies:",nrow(myDataHealthy),"\n"))
  cat(paste0("[",year,"] Amount of bankrupt companies:",nrow(myDataFull[myDataFull$STATUS == 1,]),"\n"))
  cat(paste0("[",year,"] Amount of healthy companies:",nrow(myDataFull[myDataFull$STATUS == 0,]),"\n"))
  cat(paste0("[",year,"] class disbalance:",round((nrow(myDataBankrupt)/nrow(myDataFull)),4),"\n"))
  
  write.csv(myDataFull, paste0('data\\4_ready_data\\',year,'.csv'), row.names = FALSE)
}

training_validation_interval <- c(2014, 2015, 2016)
testing_period <- c(2017)

for (year in training_validation_interval){
  myData <- read.csv(paste0("data\\4_ready_data\\",year,".csv"), header=T, stringsAsFactors = FALSE, sep=',')
  if ( year == min(training_validation_interval) ){
    fullData <- myData
  } else {
    fullData <- rbind(fullData,myData)
  }
}

bankrupt <- fullData[fullData$STATUS == 1,]
healthy <- fullData[fullData$STATUS == 0,]

nrow(bankrupt)+nrow(healthy)

healthy <- healthy[ !healthy$ID %in% unique(bankrupt$ID),]

healthy <- distinct(healthy[sample(1:nrow(healthy)), ], 
                    ID, 
                    .keep_all = TRUE)

bankrupt <- distinct(bankrupt[sample(1:nrow(bankrupt)), ], 
                    ID, 
                    .keep_all = TRUE)

nrow(fullData)
# 8999999456031
fullData <- rbind(healthy,bankrupt)
set.seed(101)
rand <- sample(nrow(fullData))
fullData <- fullData[rand,]
nrow(fullData[fullData$STATUS == 1, ])
nrow(fullData)/nrow(fullData[fullData$STATUS == 1, ])

# fullData16 <- read.csv(paste0("data\\2_clean_columns_NAs\\",2016,".csv"), header=T, stringsAsFactors = FALSE, sep=',')
# fullData17 <- read.csv(paste0("data\\2_clean_columns_NAs\\",2017,".csv"), header=T, stringsAsFactors = FALSE, sep=',')
# length(unique(fullData16$ID[fullData16$KODAS_STATUSAI == 5]))
# length(unique(fullData17$ID[fullData17$KODAS_STATUSAI == 5]))
# length(unique(fullData17$ID) %in% unique(fullData16$ID))
# tst <- c(unique(fullData17$ID),unique(fullData16$ID))
# length(tst[duplicated[tst]])
# sum(duplicated(tst))
