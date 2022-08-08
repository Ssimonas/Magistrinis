##### Pasalinami irasai
# 1. Sektorius X
# 2. Nera metiniu duomenu
# 3. Neegzistavo per praeitus metus nors viena men.

# Jeigu bankrutavo, nuo bankroto men. imam -3 men. - pirmas paskutinis irasas
library(dplyr)
library(stringr)
library(tidyverse)
library(stringr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

periods_to_go_back <- 12      # Take into account 12 months back of data.
forecast_window <- 3          # If company went bankrupt 2018-08-01 - first monthly data row will be fom 2018-05-01.
month_to_use_yearly_info <- 7 # If company went bankrupt 2018-06-01 - can't use 2017 yearly data, if 2018-07-01 and up, can use 2017 yearly data.

#myDataBankruptIDs <- 0

month.abb <- toupper(month.abb)

##### Function to reduce given date (in specific format) by 1 month
# Argument: sDate - character, format - 01JAN2018 - DDMMMYYY
# Returns: date of same format and type
fReduceDateBy1Month <- function(sDate){
  month <- match(substring(sDate,3,5),month.abb)
  year <- as.numeric(substring(sDate,6,9))
  
  year <- ifelse(month == 1, year-1, year)
  month <- ifelse(month == 1, 12, month-1)
  return (paste0("01",month.abb[month],year))
}

fGetYearFromDate <- function(sDate){
  year <- as.numeric(substring(sDate,6,9))
  return(year)
}
fReduceDateBy1Month("01JAN2018")
fGetYearFromDate("01JAN2018")
###############################

YEAR_DATA = c(2018,2017,2016,2015,2014)
#YEAR_DATA = c("2018","2017")
year <- 2018

for (year in YEAR_DATA){
  print(paste0("[",year,"] Reading data files."))
  myData0_full <- read.csv(paste0('data\\2_clean_columns_NAs\\',year,'.csv'),header=T, stringsAsFactors = FALSE, sep=',')
  myData1_full <- read.csv(paste0('data\\2_clean_columns_NAs\\',year-1,'.csv'),header=T, stringsAsFactors = FALSE, sep=',')
  
  cols_to_rm <- c("kiek_adresu")
  
  if (year > 2014){
    myData2_full <- read.csv(paste0('data\\2_clean_columns_NAs\\',year-2,'.csv'),header=T, stringsAsFactors = FALSE, sep=',')
    myData2_full <- myData2_full[, !names(myData2_full) %in% cols_to_rm]
  }

  cols_to_rm <- c("kiek_adresu")
  myData0_full <- myData0_full[, !names(myData0_full) %in% cols_to_rm]    
  myData1_full <- myData1_full[, !names(myData1_full) %in% cols_to_rm]    
  
  nrow(myData0_full)
  start_rows <- nrow(myData0_full)
  
  ## Bankrupcies for current year
  
  myDataBankruptIDs <- unique(myData0_full$ID[myData0_full$KODAS_STATUSAI == 5])
  if (year == 2014) {
    myDataBankruptIDs <- unique(myData0_full$ID[myData0_full$KODAS_STATUSAI == 5 & match(substring(myData0_full$DATE,3,5),month.abb) >= 7])
  }

  ## Keep track of all bankrupt companies, to avoid duplicates
  if (year == 2018) {  ## first iteration
    
    all_bankrupt_IDs <- myDataBankruptIDs
    
  } else {
    
    ## Do not consider companies, that were already processed
    myDataBankruptIDs <- myDataBankruptIDs[!myDataBankruptIDs %in% all_bankrupt_IDs]
    
    all_bankrupt_IDs <- c(all_bankrupt_IDs, myDataBankruptIDs)
  }
  

  #########################################################################################
  ##### MONTHLY DATASET PREPARATION  #####
  ### GATHER MONTHLY DATA
  col_id_start <- which(colnames(myData0_full) %in% "pardavimai_pvm_dekl")
  col_id_end <- which(colnames(myData0_full) %in% "VADOV_POK")
  myDataM0 <- myData0_full[myData0_full$ID %in% myDataBankruptIDs,c(1:2,col_id_start:col_id_end)]
  
  myDataM1 <- myData1_full[myData1_full$ID %in% myDataBankruptIDs,c(1:2,col_id_start:col_id_end)]
  
  myDataM2 <- myData2_full[myData2_full$ID %in% myDataBankruptIDs,c(1:2,col_id_start:col_id_end)]
  
  
  ### GATHER YEARLY DATA
  col_id_start_Y1 <- which(colnames(myData0_full) %in% "B_PARDAVIMO_PAJAMOS")
  col_id_end_Y1 <- which(colnames(myData0_full) %in% "R_BENDR_MOK_KOEF")
  
  col_id_start_Y2 <- which(colnames(myData0_full) %in% "SEGMENTAS_MMM")
  col_id_end_Y2 <- which(colnames(myData0_full) %in% "Z_score")
  
  myDataY0 <- myData0_full[myData0_full$ID %in% myDataBankruptIDs,c(1:2,4,col_id_start_Y1:col_id_end_Y1,col_id_start_Y2:col_id_end_Y2)]
  myDataY0 <- select(filter(myDataY0, str_detect(DATE, "DEC")),everything())
  
  myDataY1 <- myData1_full[myData1_full$ID %in% myDataBankruptIDs,c(1:2,4,col_id_start_Y1:col_id_end_Y1,col_id_start_Y2:col_id_end_Y2)]
  myDataY1 <- select(filter(myDataY1, str_detect(DATE, "DEC")),everything())
  
  myDataY2 <- myData2_full[myData2_full$ID %in% myDataBankruptIDs,c(1:2,4,col_id_start_Y1:col_id_end_Y1,col_id_start_Y2:col_id_end_Y2)] 
  myDataY2 <- select(filter(myDataY2, str_detect(DATE, "DEC")),everything())
  
  ##################################################
  #### EMPTY DATA SET CONSTUCTION ######
  # base_vars_to_have_M <- c("pardavimai_pvm_dekl","pvm_dekl_36","pirkimai_pvm_dekl","n_PVM_MEN",
  #                        "vidut_parduot_pvm","darb_su_alga","darb_neapmok","viso_darbuotoju", 
  #                        "DARBUOTOJU_PAGAL_DD","DU_KASTAI","DIRBUSIU_BENT_DIENA", 
  #                        "idarbintu_sutart","DU_NUO_PARDAVIMU","PAJ_DARB", 
  #                        "NEAPMOK_PROC","FA_vald_dalis","likutis","VADOV_POK")
  
  base_vars_to_have_M <- colnames(myDataM0)[3:length(colnames(myDataM0))]

  base_vars_to_have_Y <- colnames(myDataY0)[3:length(colnames(myDataY0))]
  
  vars_to_have <- c("ID","DATE")
  
  for (var in base_vars_to_have_M) {
    for (i in 1:periods_to_go_back){
      vars_to_have <- c(vars_to_have, paste0(var,"_M_",i))
    }
  }
  
  ## INCLUDE: categorical vars, Z_SCORE var, imones_amzius_mm
  for (var in base_vars_to_have_Y) {
    vars_to_have <- c(vars_to_have, paste0(var,"_Y_",1))
  }
  
  vars_to_have <- c(vars_to_have, "STATUS")
  
  m <- matrix(0, ncol = length(vars_to_have), nrow = 0)
  myDataRows <- data.frame(m)
  names(myDataRows) <- vars_to_have
  
  rm(m)
  #########################################################################################
  
  
  
  #########################################################################################
  ##### CONSTRUCTING BACK MONTHLY DATA   ##### 
  
  #str(myData$DATE)
  #as.Date(myData$DATE[1], "%d%b%Y")
  #substring(myData$DATE[myData$ID == bID],3,5)
  #match(substring(myData$DATE[1:10],3,5),month.abb)
  
  iterator <- 0
  iterator_early_bankr <- 0
  
  # myDataCurr <- myDataY0
  # myData1Prev <- myData_1prev
  
  for (bID in myDataBankruptIDs){

    #bID <- myDataBankruptIDs[1]
    iterator <- iterator+1
    # Extract bankrupcy date, break it down
    bID_bankrupt_full_date <- tail(myDataM0$DATE[myDataM0$ID == bID],1)
    bID_bankrupt_month <- max(match(substring(bID_bankrupt_full_date,3,5),month.abb))
    bID_bankrupt_year <- as.numeric(substring(bID_bankrupt_full_date,6,9))
    
    # If company went bankrupt 2018-08-01 - first monthly data row will be fom 2018-05-01 (= forecast_window = 3)
    ## Calculate date of most recent known information about this company
    forecast_back <- bID_bankrupt_month-forecast_window
    starting_month <- ifelse(forecast_back <= 0, forecast_back+periods_to_go_back, forecast_back)
    starting_year <- ifelse(forecast_back <= 0, bID_bankrupt_year-1, bID_bankrupt_year)
    
    ## Construct start point date to go back from
    starting_yearly_year <- ifelse(bID_bankrupt_month < month_to_use_yearly_info, bID_bankrupt_year-2, bID_bankrupt_year-1)
    starting_full_date <- paste0("01",month.abb[starting_month],starting_year)
    
    cat(paste0("[",iterator,"]\t",bID,"\t Bankrupcy_date: ",bID_bankrupt_full_date,"\tStarting_info_date: ",starting_full_date,"\tYearly_year_data: ",starting_yearly_year,"\n"))
    
    current_date <- starting_full_date
    

    ## Create an empty row of new dataset
    current_row <- c(bID,bID_bankrupt_full_date)
    
    
    for (var in base_vars_to_have_M){

      current_date <- starting_full_date
      
      ## Get every period for this var
      ## Construct the variables for this period and attach them to new row
      for (i in 1:periods_to_go_back){
        current_year <- fGetYearFromDate(current_date)
        
        myData <- get(paste0("myDataM",year-fGetYearFromDate(current_date)))
        
        value_to_add <- myData[myData$DATE == current_date & myData$ID == bID,var]
        value_to_add <- ifelse(identical(value_to_add,numeric(0)), 0, value_to_add)
        value_to_add <- ifelse(is.na(value_to_add), 0, value_to_add)
        #cat(bID,"\t",current_date,"\t",var,"\t",myData[myData$DATE == current_date & myData$ID == bID,var],"\n")
        #cat(bID,"\t",current_date,"\t",var,"\t",value_to_add,"\n")

        current_row <- c(current_row, value_to_add)
        
        ### Re-construct monthly date
        current_date <- fReduceDateBy1Month(current_date)
      }

    }
    
    ## ATTACH THE APPROPRIATE YEARLY DATA
    myData <- get(paste0("myDataY",year-starting_yearly_year))
    
    for (var in base_vars_to_have_Y){
      value_to_add <- myData[myData$ID == bID,var]
      value_to_add <- ifelse(identical(value_to_add,numeric(0)), 0, value_to_add)
      value_to_add <- ifelse(is.na(value_to_add), 0, value_to_add)
      current_row <- c(current_row, value_to_add)
    }
    
    myDataRows[nrow(myDataRows)+1,] <- c(current_row,1) # 1 = for STATUS variable, which means a bankrupcy procedure was started for this company
    
  }
  
  write.csv(myDataRows, paste0('data\\3_data_to_rows\\BANKR_',year,'.csv'), row.names = FALSE)
  
  #write.csv(myDataBankrupt, paste0('data\\3_data_to_rows\\',year,'_bankrupt.csv'), row.names = FALSE)
  #write.csv(myData, paste0('data\\3_data_to_rows\\',year,'_bankrupt.csv'), row.names = FALSE)
  rm(myData)
  gc()
  print("\n")
}


for (year in YEAR_DATA){
  print(paste0("[",year,"] Reading bankrupt data files."))
  myData <- read.csv(paste0('data\\3_data_to_rows\\BANKR_',year,'.csv'),header=T, stringsAsFactors = FALSE, sep=',')

  if (year == 2018){
    full_bankrupt_data <- myData
  } else {
    full_bankrupt_data <- rbind(full_bankrupt_data, myData)
  }
  
}  
back <- full_bankrupt_data
full_bankrupt_data <- back


nrow(full_bankrupt_data)
length(unique(full_bankrupt_data$ID))
full_bankrupt_data <- full_bankrupt_data[-which(rowSums(is.na(full_bankrupt_data) | full_bankrupt_data == 0) > 215),]
nrow(full_bankrupt_data)
length(unique(full_bankrupt_data$ID))
## 7489
215/229

bankrupt_dates <- full_bankrupt_data$DATE
bankrupt_cases <- table(bankrupt_dates)
bankrupt_cases


bankruptIDs <- unique(full_bankrupt_data$ID)
bankruptIDs <- length(unique(full_bankrupt_data$ID))

write.csv(full_bankrupt_data, paste0('data\\3_data_to_rows\\FULL_BANKR.csv'), row.names = FALSE)
full_bankrupt_data <- read.csv(paste0('data\\3_data_to_rows\\FULL_BANKR.csv'),header=T, stringsAsFactors = FALSE, sep=',')

substring(full_bankrupt_data$DATE[1],3,5)
bankruptIDs <- unique(full_bankrupt_data$ID[substring(full_bankrupt_data$DATE,3,5) == "JAN"])
bankruptIDs <- length(unique(full_bankrupt_data$ID))



