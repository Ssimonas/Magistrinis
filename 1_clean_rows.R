##### Pasalinami irasai
# 1. Sektorius X
# 2. Nera metiniu duomenu
# 3. Neegzistavo per praeitus metus nors viena men.

# Jeigu bankrutavo, nuo bankroto men. imam -3 men. - pirmas paskutinis irasas
library(dplyr)
library(stringr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(scipen = 999)

#YEAR_DATA = c("2013","2014","2015","2016","2017","2018")
YEAR_DATA = c("2018","2017","2016","2015","2014","2013")

year <- 2018

bankrupt_all <- 00

for (year in YEAR_DATA){
  print(paste0("[",year,"] Reading data file."))
  myData <- read.csv(paste0('data\\01_raw\\im',year,'_utf8.csv'),header=T, stringsAsFactors = FALSE, sep=',')
  colnames(myData)[1] <- "ID"
  
  ###################################################  
  ###### FIX NAs IN TARGET VARIABLE
  start_rows <- nrow(myData)
  sum(is.na(myData$KODAS_STATUSAI))
  myData$KODAS_STATUSAI[is.na(myData$KODAS_STATUSAI)] <- 0
  nrow(myData[myData$KODAS_STATUSAI == 5,])/nrow(myData)
  
  
  ###################################################  
  ###### Keep companies, that went bankrupt keep them throughout all the years.
  bankrupt_IDs <- myData$ID[myData$KODAS_STATUSAI == 5]
  bankrupt_IDs <- unique(bankrupt_IDs)
  bankrupt_all <- c(bankrupt_all, bankrupt_IDs)

  
  
  ####################################################################################################  
  ###### REMOVE ALL UNCLEAR COMPANIES (SECTOR = X)
  nrow(myData)
  
  ## Check other years for change in X?
  print(paste0("[",year,"] Removing all rows from section X."))
  IDs_to_drop <- myData$ID[myData$VKR_SEKCIJA == "X"]
  IDs_to_drop <- unique(IDs_to_drop)
  length(IDs_to_drop)
  
  ## exclude bankrupt companies
  #IDs_to_drop <- IDs_to_drop[!IDs_to_drop %in% bankrupt_all] #####?????
  #length(IDs_to_drop)  
  
  myData <- myData[!myData$ID %in% IDs_to_drop,]
  
  nrow(myData)
  
  rm(IDs_to_drop)
  
  
  ###################################################
  ##### REMOVES ALL, DOESN'T CARE ABOUT BANKRUPT ONES
  # myData <- myData[myData$VKR_SEKCIJA != "X",]
  #
  ##### BANKRUPTED COMPANIES???
  # bankrupcies <- aggregate(myData$KODAS_STATUSAI, by=list(ID=myData$ID), FUN=sum)
  # bankrupcies_IDs <- bankrupcies[bankrupcies$x >= 5, 1]
  # tempPrint <- myData[myData$ID %in% bankrupcies_IDs,]
  # write.csv(tempPrint, paste0('data\\1_clean_rows\\',year,'asdas.csv'), row.names = FALSE)
  
  
  ###################################################################
  ###### REMOVE ALL COMPANIES, THAT DID NOT EXIST FOR AT LEAST 1 YEAR
  print(paste0("[",year,"] Getting all IDs that are younger than 12 months."))
  
  did_exist <- aggregate(myData$AR_EGZISTAVO, by=list(ID=myData$ID), FUN=sum)
  IDs_to_drop <- did_exist[did_exist$x < 12, 1]

  # exclude bankrupt companies
  IDs_to_drop <- IDs_to_drop[!IDs_to_drop %in% bankrupt_all]

  # Do not drop everything if the company went bakrupt
  rm(did_exist)
  nrow(myData)
  nrow(myData[myData$KODAS_STATUSAI == 5,])
  nrow(myData[myData$KODAS_STATUSAI == 5,])/nrow(myData)
  
  print(paste0("[",year,"] Removing all too young companies."))
  myData <- myData[ !myData$ID %in% IDs_to_drop, ]
  rm(IDs_to_drop)
  nrow(myData)
  
  ## What if younger than 12 months and bankrupt (ID in bankrupt_IDs and in IDs_to_drop)? - useless
  
  
  
  #############################################################
  ###### REMOVE ALL COMPANIES, THAT DID NOT PROVIDE YEARLY DATA
  print(paste0("[",year,"] Getting all IDs that did not provide yearly data. (NO DATA ABOUT EMPLOYEES)."))
  myDataYearly <- select(filter(myData, str_detect(DATE, "DEC")),everything())
  #myDataYearly <- select(filter(myData, str_detect(DATE, "Dec")),everything())
  nrow(myDataYearly)
  ##### ENTER PREFERENCES
  col_id_start <- which(colnames(myDataYearly) %in% "darb_su_alga")
  col_id_end <- which(colnames(myDataYearly) %in% "NEAPMOK_PROC")
  ###!!!! Careful - selection of columns ":" includes the boundry columns as well.
  max_NA <- col_id_end-col_id_start+1
  
  #max_NA <- max(rowSums(is.na(myDataYearly[col_id_start:col_id_end])))
  #max_NA*0.95

  #nrow(myDataYearly)

  #nrow(myDataYearly[rowSums(is.na(myDataYearly[col_id_start:col_id_end])) >= max_NA, ])

  #summary(rowSums(is.na(myDataYearly[col_id_start:col_id_end])))
  
  IDs_to_drop <- unique(myDataYearly$ID[rowSums(is.na(myDataYearly[col_id_start:col_id_end])) >= max_NA])
  IDs_to_drop <- IDs_to_drop[!IDs_to_drop %in% bankrupt_all]

  #nrow(myDataYearly)
  
  print(paste0("[",year,"] Removing all companies, that did not provide full yearly data (no data about employees)."))
  myData <- myData[ !myData$ID %in% IDs_to_drop, ]
  rm(IDs_to_drop)
  rm(myDataYearly)
  
  nrow(myData)
  nrow(myData[myData$KODAS_STATUSAI == 5,])
  nrow(myData[myData$KODAS_STATUSAI == 5,])/nrow(myData)
  
  print(paste0("[",year,"] Started with ",start_rows," rows. Removed ",start_rows-nrow(myData)," (",100*round((start_rows-nrow(myData))/start_rows,4),"%). Rows left: ",nrow(myData)," (",nrow(myData)/12," companies out of ",start_rows/12,")."))
  print(paste0("[",year,"] Writing cleaned rows file to the file.."))
  write.csv(myData, paste0('data\\1_cleaned_rows\\',year,'.csv'), row.names = FALSE)
  rm(myData)
  gc()
  print(" ")
}
