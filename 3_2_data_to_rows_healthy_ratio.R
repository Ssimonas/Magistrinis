##### Pasalinami irasai
# Is 3_1 gauti "bankrupt_cases" ir "bankruptIDs"
bankrupt_cases
length(bankruptIDs)

full_bankrupt_data <- read.csv(paste0('data\\3_data_to_rows\\FULL_BANKR.csv'),header=T, stringsAsFactors = FALSE, sep=',')

month_being_processed <- "NOV"

substring(full_bankrupt_data$DATE[1],3,5)
bankruptIDs <- unique(full_bankrupt_data$ID[substring(full_bankrupt_data$DATE,3,5) == month_being_processed])
length(bankruptIDs)
#bankruptIDs <- length(unique(full_bankrupt_data$ID))

library(dplyr)
library(stringr)
library(tidyverse)
library(stringr)
library(doParallel)
library(foreach)
library(compiler)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

periods_to_go_back <- 12      # Take into account 12 months back of data.
forecast_window <- 3          # If company went bankrupt 2018-08-01 - first monthly data row will be fom 2018-05-01.
month_to_use_yearly_info <- 7 # If company went bankrupt 2018-06-01 - can't use 2017 yearly data, if 2018-07-01 and up, can use 2017 yearly data.

healthy_to_bankrupt_ratio <- 10

#healthy_year_cases_to_track <- c(12,2)

scale_to_date <- 1 # 0 - processes all companies fom this year (picks bankruptcy date by random), 1 - scales companies by bankruptcy dates

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
###############################

YEAR_DATA = c(2018, 2017, 2016, 2015, 2014)
#YEAR_DATA = c("2018","2017")
#year <- 2017
year <- 2018
#healthy_year_cases_to_track <- c(12,2)

#disbalance_ratios_set = c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50)
disbalance_ratios_set = c(1,10,25,30)

bankrupt_dates <- full_bankrupt_data$DATE
bankrupt_cases <- table(bankrupt_dates)

for (disbalance_ratio in disbalance_ratios_set){

  for (year in YEAR_DATA){
    bankruptIDs <- unique(full_bankrupt_data$ID[fGetYearFromDate(full_bankrupt_data$DATE) == year])
    
    processed_IDs <- bankruptIDs
    
    skipped <- 0
    cat(paste0("processed_IDs list size:",length(processed_IDs),"\n"))
    cat("\n")
    cat("\n")
    print(paste0("[",year,"] Reading data files."))
    
    cols_to_rm <- c("kiek_adresu", "VADOV_POK", "laik_tipas_pvm_M", "laik_tipas_pvm_P")
    
    myData0_full <- read.csv(paste0('data\\2_clean_columns_NAs\\',year,'.csv'),header=T, stringsAsFactors = FALSE, sep=',')
    myData1_full <- read.csv(paste0('data\\2_clean_columns_NAs\\',year-1,'.csv'),header=T, stringsAsFactors = FALSE, sep=',')
    
    myData0_full <- myData0_full[, !names(myData0_full) %in% cols_to_rm]    
    myData1_full <- myData1_full[, !names(myData1_full) %in% cols_to_rm]  
    
    if (year > 2014){
      myData2_full <- read.csv(paste0('data\\2_clean_columns_NAs\\',year-2,'.csv'),header=T, stringsAsFactors = FALSE, sep=',')
      myData2_full <- myData2_full[, !names(myData2_full) %in% cols_to_rm]
    }
    
    ## Remove all bankrupt companies
    myData0_full <- myData0_full[ !myData0_full$ID %in% processed_IDs,]
    myData1_full <- myData1_full[ !myData1_full$ID %in% processed_IDs,]
    myData2_full <- myData2_full[ !myData2_full$ID %in% processed_IDs,]
    
    # back_bankrupt_cases <- bankrupt_cases
    # 
    # length(unique(myData0_full$ID))
    # 
    healthy_year_cases_to_track <- bankrupt_cases[as.numeric(substring(names(bankrupt_cases),6,9)) == year]
    # 
    # healthy_year_cases_to_track <- floor((healthy_year_cases_to_track/sum(healthy_year_cases_to_track))*length(unique(myData0_full$ID)))
    # 
    # healthy_year_cases_to_track <- floor(healthy_year_cases_to_track*0.95)
    
  
  
    # how many unique healthy IDs are needed for this year
    #sum(bankrupt_cases[as.numeric(substring(names(bankrupt_cases),6,9)) == year])
    
    #year_bankrupcies <- sum(scaled_healthy_cases[as.numeric(substring(names(scaled_healthy_cases),6,9)) == year])
    healthy_year_cases_to_track <- healthy_year_cases_to_track*disbalance_ratio
    year_bankrupcies <- sum(healthy_year_cases_to_track)
    #print(paste0("[",year,"] Total amount of occurances needed:", year_bankrupcies))
    print(healthy_year_cases_to_track)
    cat("\n")
  
    #########################################################################################
    ##### MONTHLY DATASET PREPARATION  #####
    ### GATHER MONTHLY DATA
    col_id_start_M <- which(colnames(myData0_full) %in% "pardavimai_pvm_dekl")
    col_id_end_M <- which(colnames(myData0_full) %in% "likutis")
    
    myDataM0 <- myData0_full[,c(1:2,col_id_start_M:col_id_end_M)]
    myDataM1 <- myData1_full[,c(1:2,col_id_start_M:col_id_end_M)]
    myDataM2 <- myData2_full[,c(1:2,col_id_start_M:col_id_end_M)]
    
    starting_rows <- nrow(myData0_full)
    myDataM0_IDs <- unique(myData0_full$ID)
    length(myDataM0_IDs)
    
    
    ### GATHER YEARLY DATA
    col_id_start_Y1 <- which(colnames(myData0_full) %in% "B_PARDAVIMO_PAJAMOS")
    col_id_end_Y1 <- which(colnames(myData0_full) %in% "R_BENDR_MOK_KOEF")
    
    col_id_start_Y2 <- which(colnames(myData0_full) %in% "SEGMENTAS_MMM")
    col_id_end_Y2 <- which(colnames(myData0_full) %in% "Z_score")
    
    myDataY0 <- myData0_full[,c(1:2,4,col_id_start_Y1:col_id_end_Y1,col_id_start_Y2:col_id_end_Y2)]
    myDataY0 <- select(filter(myDataY0, str_detect(DATE, "DEC")),everything())
    
    myDataY1 <- myData1_full[,c(1:2,4,col_id_start_Y1:col_id_end_Y1,col_id_start_Y2:col_id_end_Y2)]
    myDataY1 <- select(filter(myDataY1, str_detect(DATE, "DEC")),everything())
  
    myDataY2 <- myData2_full[,c(1:2,4,col_id_start_Y1:col_id_end_Y1,col_id_start_Y2:col_id_end_Y2)] 
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
    
    for (var in base_vars_to_have_Y) {
      vars_to_have <- c(vars_to_have, paste0(var,"_Y_",1))
    }
    
    vars_to_have <- c(vars_to_have, "STATUS")
    
    m <- matrix(0, ncol = length(vars_to_have), nrow = 0)
    myDataRows <- data.frame(m)
    names(myDataRows) <- vars_to_have
    rm(m)
    #rm(myDataRows)
    #########################################################################################
    
    
    
    #########################################################################################
    ##### CONSTRUCTING BACK MONTHLY DATA   ##### 
    
    iterator <- 0
    iterator_early_bankr <- 0
    
    myDataM0_IDs <- unique(myData0_full$ID)
    myDataM0_IDs <- myDataM0_IDs[!myDataM0_IDs %in% processed_IDs]
    
    if (scale_to_date == 1){
     myDataM0_IDs <- sample(myDataM0_IDs, (year_bankrupcies+5000))
    }
    
    length(myDataM0_IDs)
    length(unique(myDataM0_IDs))
    
    ## Narrow down the datasets that will be needed
    myData0_full <- myData0_full[ myData0_full$ID %in% myDataM0_IDs,]
    myData1_full <- myData1_full[ myData1_full$ID %in% myDataM0_IDs,]
    myData2_full <- myData2_full[ myData2_full$ID %in% myDataM0_IDs,]
    
    
    for(bID in myDataM0_IDs) {
      
      # bID <- myDataM0_IDs[i]
      iterator <- iterator+1
      
      # every 5000 - reduce full data set
      if (iterator >= 5000 & iterator %% 5000 == 0){
        cat(paste0("Removing 5000 processed IDs from full data sets.\n"))
        myData0_full <- myData0_full[ !myData0_full$ID %in% unique(myDataRows$ID),]
        myData1_full <- myData1_full[ !myData1_full$ID %in% unique(myDataRows$ID),]
        myData2_full <- myData2_full[ !myData2_full$ID %in% unique(myDataRows$ID),]
        myDataY1 <- myDataY1[!myDataY1$ID %in% unique(myDataRows$ID),]
        myDataY2 <- myDataY2[!myDataY2$ID %in% unique(myDataRows$ID),]
        
        cat(paste0(nrow(myData0_full),"Data rows are left of ",starting_rows," (",1-(nrow(myData0_full)/starting_rows)," processed).\n"))
        
        if(scale_to_date == 1){
          write.csv(myDataRows, paste0('data\\3_data_to_rows\\custom_disbalance\\HEALTHY_scaled_',disbalance_ratio,'_ALL_',year,'_part_',iterator,'.csv'), row.names = FALSE)
        } else {
          write.csv(myDataRows, paste0('data\\3_data_to_rows\\custom_disbalance\\HEALTHY_NOT-scaled_',disbalance_ratio,'_ALL_',year,'_part_',iterator,'.csv'), row.names = FALSE)
        }
        
        myDataRows <- myDataRows[0,]
        gc()
      }
      
      ## Slice only data that will be needed now (by ID)
      myDataM0 <- myData0_full[myData0_full$ID == bID,c(1:2,col_id_start_M:col_id_end_M)]
      myDataM1 <- myData1_full[myData1_full$ID == bID,c(1:2,col_id_start_M:col_id_end_M)]
      myDataM2 <- myData2_full[myData2_full$ID == bID,c(1:2,col_id_start_M:col_id_end_M)] 
      
      # Take first available date for this year
      if (scale_to_date == 1){
        bID_bankrupt_full_date <- names(healthy_year_cases_to_track[1])
      } else {
        #bID_bankrupt_full_date <- paste0("01",month.abb[sample(1:12,1)],"2018")
        bID_bankrupt_full_date <- paste0("01",month_being_processed,year)
      }
      
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
      
      if (iterator >= 200 & iterator %% 200 == 0){
        cat(paste0("[",iterator,"]\t",bID,"\t Bankrupcy_date: ",bID_bankrupt_full_date,"\tStarting_info_date: ",starting_full_date,"\tYearly_year_data: ",starting_yearly_year,"\n"))
      }
      
      current_date <- starting_full_date
      
      ## Create an empty row?
      
      ##### THIS OKAY
      current_row <- c(bID,bID_bankrupt_full_date)
      
      for (var in base_vars_to_have_M){
        
        current_date <- starting_full_date
        
        ## Get every period for this var
        ## Construct the variables for this period and attach them to new row
        for (i in 1:periods_to_go_back){
          current_year <- fGetYearFromDate(current_date)
          
          myData <- get(paste0("myDataM",year-fGetYearFromDate(current_date)))
          
          value_to_add <- myData[myData$DATE == current_date,var]
          value_to_add <- ifelse(is.null(value_to_add), 0, value_to_add)
          value_to_add <- ifelse(identical(value_to_add,numeric(0)), 0, value_to_add)
          value_to_add <- ifelse(is.na(value_to_add), 0, value_to_add)
          
          current_row <- c(current_row, value_to_add)
          
          ### Re-construct monthly date
          current_date <- fReduceDateBy1Month(current_date)
        }
        
      }
      
      ## ATTACH THE APPROPRIATE YEARLY DATA
      myData <- get(paste0("myDataY",year-starting_yearly_year))
      myData <- myData[myData$ID == bID,]
      for (var in base_vars_to_have_Y){
        value_to_add <- myData[,var]
        value_to_add <- ifelse(identical(value_to_add,numeric(0)), 0, value_to_add)
        value_to_add <- ifelse(is.na(value_to_add), 0, value_to_add)
        current_row <- c(current_row, value_to_add)
      }
      
      if (sum(is.na(current_row) | current_row == 0) > 211){
        cat(paste0("TOO MUCH EMPTY CELLS IN THIS ROW. ID: ",bID,"\n"))
        skipped <- skipped + 1
  
      } else {
        if (scale_to_date == 1) {
          # Reduce the amount of dates needed for this year
          healthy_year_cases_to_track[1] <- healthy_year_cases_to_track[1] - 1
          # All corresponding healthy companies were created for this month
          if (healthy_year_cases_to_track[1] == 0){
            print(healthy_year_cases_to_track)
            cat("\n")
            cat(paste0("[",iterator,"]------- [",year,"] DATE ",names(healthy_year_cases_to_track[1])," finished. Skipped:",skipped,"-------\n"))
            cat("\n")
            healthy_year_cases_to_track <- healthy_year_cases_to_track[names(healthy_year_cases_to_track) != names(healthy_year_cases_to_track[1])]
          }      
        }
        myDataRows[nrow(myDataRows)+1,] <- c(current_row,0) # 1 = for STATUS variable, which means a bankrupcy procedure was started for this company
        
        if (iterator >= 200 & iterator %% 200 == 0){
          cat(paste0("processed_IDs list size:",length(processed_IDs),"\tof which accepted: ",length(processed_IDs)-skipped,"\n"))
        }
      }
      
      
      processed_IDs <- c(processed_IDs, bID)
      
      rm(myData,value_to_add,current_row,myDataM0,myDataM1,myDataM2)
      
      if (length(healthy_year_cases_to_track) == 0){
        cat("ALL COMPANIES FOR THIS YEAR WERE PROCESSED. ENDING THE LOOP.\n")
        #Sys.time()-TIME
        break
      }    
      
    }
    
  
    write.csv(myDataRows, paste0('data\\3_data_to_rows\\custom_disbalance\\HEALTHY_scaled_',disbalance_ratio,'_ALL_',year,'_part_last.csv'), row.names = FALSE)
    
    print("\n")
  }

}



disbalance_ratios_set = c(1,2,4,6,8,10,12,14,16,18,20,25,30)

full_bankrupt_data <- read.csv(paste0('data\\3_data_to_rows\\FULL_BANKR.csv'),header=T, stringsAsFactors = FALSE, sep=',')


for (disb_ratio in disbalance_ratios_set){
  fl <- list.files(paste0('data\\3_data_to_rows\\custom_disbalance\\',disb_ratio,"\\parts"))
  
  iterator <- 0
  
  for(i in 1:length(fl)){
    
    fileName <- fl[i]
    filePath <- paste0("data\\3_data_to_rows\\custom_disbalance\\",disb_ratio,"\\parts\\",fileName)
    
    myData <- read.csv(filePath, header=T, stringsAsFactors = FALSE, sep=',')
    
    if (iterator == 0){
      full_data_set <- myData
    } else {
      full_data_set <- rbind(full_data_set,myData)
    }
    
    iterator <- iterator + 1
  }
  
  full_data_set <- rbind(full_data_set,full_bankrupt_data)
  
  set.seed(101)
  rand <- sample(nrow(full_data_set))
  full_data_set <- full_data_set[rand,]
  
  cat("Disbalance ratio:",disb_ratio,
      "\tAll companies:",nrow(full_data_set),
      "\tHealthy companies:",nrow(full_data_set[full_data_set$STATUS == 0,]),
      "\tBankrupt companies:",nrow(full_data_set[full_data_set$STATUS == 1,]),
      "\tRatio:",round(nrow(full_data_set[full_data_set$STATUS == 1,])/nrow(full_data_set),2),"\n")
  
  write.csv(full_data_set, paste0("data\\3_data_to_rows\\custom_disbalance\\",disb_ratio,"\\FULL_DATASET_DISB_",disb_ratio,".csv"), row.names = FALSE)
  
  full_data_set_no2018 <- full_data_set[fGetYearFromDate(full_data_set$DATE) != 2018,]
  write.csv(full_data_set_no2018, paste0("data\\3_data_to_rows\\custom_disbalance\\",disb_ratio,"\\train_disb_",disb_ratio,"_no2018.csv"), row.names = FALSE)
  
  full_data_set_only2018 <- full_data_set[fGetYearFromDate(full_data_set$DATE) == 2018,]
  write.csv(full_data_set_only2018, paste0("data\\3_data_to_rows\\custom_disbalance\\",disb_ratio,"\\test_disb_",disb_ratio,"_only2018.csv"), row.names = FALSE)
}





