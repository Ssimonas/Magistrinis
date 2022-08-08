##### Pasalinami stulpeliai

library(dplyr)
library(stringr)
library(plyr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

YEAR_DATA = c("2013","2014","2015","2016","2017","2018")
#YEAR_DATA = c("2017","2018")
year <- 2017
all_bankrupt_IDs <- 0

for (year in YEAR_DATA){
  print(paste0("[",year,"] Reading data file."))
  myData <- read.csv(paste0('data\\1_cleaned_rows\\',year,'.csv'),header=T, stringsAsFactors = FALSE, sep=',')
  #myData <- read.csv(paste0('data\\1_clean_rows\\',year,'.csv'),header=T, stringsAsFactors = FALSE, sep=',')
  
  nrow(myData)
  start_rows <- nrow(myData)
  
  names(myData)[1] <- "ID"
  #################################################################
  ######################## 1. FIX VARIABLES #######################
  # Numbers: 1,000.00 are read as chars (change to 1000.00?)
  myData$pardavimai_pvm_dekl <- as.numeric(gsub(",","",myData$pardavimai_pvm_dekl))
  myData$pvm_dekl_36 <- as.numeric(gsub(",","",myData$pvm_dekl_36))
  myData$pirkimai_pvm_dekl <- as.numeric(gsub(",","",myData$pirkimai_pvm_dekl))
  #################################################################
  
  unique(myData$APS)
  
  ############################################################################
  ######################## 2. REMOVE REDUNDANT COLUMNS #######################
  cols_to_rm <- c("n_PVM_MEN",
                  "FA_vald_dalis",
                  "KLN_KLNT_TYPE",
                  "FIN_ATASKAITA",
                  "bal_men",
                  "PVM_MOK",
                  "B_FIN_ATSK_KAT",
                  "NAUJAS_PVM_MOK", ### iki cia (imtinai) - tiesiog netinkami, beprasmiai (pagal destytoja)
                  "KODAS_FORMA",    ### per mazas pasiskirstymas kategorijose
                  "APRASYMAS_FORMA",
                  "EV1_ID_NAME_TR",
                  "yra_nepriemoka",
                  "GRUPE")  ### iki cia dubliuojantys infromacija
  
  myData <- myData[, !names(myData) %in% cols_to_rm]
  ###########################################################################  
  
  ### CONVERT DATA FOR 2013 AND 2014 YEARS FROM LITAI TO EURO
  if (year < 2015) {
    myData$B_PARDAVIMO_PAJAMOS <- myData$B_PARDAVIMO_PAJAMOS/3.45
    myData$B_PELNAS_PRIES_APMOKEST <- myData$B_PELNAS_PRIES_APMOKEST/3.45
    myData$B_GRYNASIS_PELNAS <- myData$B_GRYNASIS_PELNAS/3.45
    myData$B_PARDAVIMU_SAVIKAINA <- myData$B_PARDAVIMU_SAVIKAINA/3.45
    myData$B_SANAUDOS <- myData$B_SANAUDOS/3.45
    myData$B_TURTO_IS_VISO <- myData$B_TURTO_IS_VISO/3.45
    myData$B_PINIGAI_IR_EKVIVALENTAI <- myData$B_PINIGAI_IR_EKVIVALENTAI/3.45
    myData$B_TRUMP_TURTAS <- myData$B_TRUMP_TURTAS/3.45
    myData$B_ATSARGOS <- myData$B_ATSARGOS/3.45
    myData$B_NUOSAVAS_KAPITALAS <- myData$B_NUOSAVAS_KAPITALAS/3.45
    myData$B_TRUMP_ISIPAREIG <- myData$B_TRUMP_ISIPAREIG/3.45
    myData$R_APYVART_KAPITALAS <- myData$R_APYVART_KAPITALAS/3.45
    myData$R_ISIPAREIGOJIMAI <- myData$R_ISIPAREIGOJIMAI/3.45
    myData$likutis <- myData$likutis/3.45
    myData$DU_KASTAI <- myData$DU_KASTAI/3.45
    myData$DU_NUO_PARDAVIMU <- myData$DU_NUO_PARDAVIMU/3.45
    myData$PAJ_DARB <- myData$PAJ_DARB/3.45
  }
  
  
  ####################################################################################################
  #### REMOVE ABSOLUTELY ALL ROWS AFTER FIRST OCCURANCE OF "KODAS_STATUSAI == 5" FOR EVERY ID #####
  print(paste0("Rows before removing bankrupted repetitions: ",nrow(myData)))
  print(paste0("Rows with bankrupt flag: ",nrow(myData[myData$KODAS_STATUSAI == 5,])))
  
  RowsPreRemoval <- nrow(myData)
  
  # Remove all bankrupt companies, that were declared bankrupt earlier.
  myData <- myData[!myData$ID %in% all_bankrupt_IDs,]
  myDataTemp <- myData
  
  myData <- group_by(myDataTemp, ID) %>% 
    mutate(first5 = min(which(KODAS_STATUSAI == 5 | dplyr::row_number() == n()))) %>%
    filter(row_number() <= first5) %>% 
    select(-first5)
  
  rm(myDataTemp)
  print(paste0("Rows after removing bakrupted repetitions(",RowsPreRemoval-nrow(myData),"): ",nrow(myData)))
  
  all_bankrupt_IDs <- c(all_bankrupt_IDs, unique(myData$ID[myData$KODAS_STATUSAI == 5]))
  
  
  ###################################################################################################################
  ###### 4. COUNT EMPTY ROWS FOR EVERY COLUMN, IF THERE ARE MORE THAN 98% OF EMPTY ROWS - REMOVE THIS COLUMN ########
  print(paste0("[",year,"] Counting empty values in columns. Removing empty columns. Columns pre-removal:",length(names(myData))))
  if (year > 2014){
    empty_values <- sapply(myData, function(x){sum(is.na(x))})
    empty_rows_99perc <- names(empty_values[empty_values >= nrow(myData)*0.98]) 
    myData <- select(myData, -empty_rows_99perc)
  } else {
    myData <- myData[, !names(myData) %in% c("gpm","pvm","pm","akc","kiti","VALST_SAV_SEGMENTAS", "B_SANAUDOS", "B_P1M_GAUT_SUMOS")]
  }
  print(paste0("[",year,"]   Columns after-removal:",length(names(myData))))
  ###################################################################################################################
  
  summary(myData)
  
  
  ##############################################################################
  ############################### 5. DEAL WITH NAs #############################
  myData$B_PELNAS_PRIES_APMOKEST <- ifelse(is.na(myData$B_PELNAS_PRIES_APMOKEST) & myData$B_GRYNASIS_PELNAS <= 0, myData$B_GRYNASIS_PELNAS, myData$B_PELNAS_PRIES_APMOKEST)
  # myData$B_FIN_ATSK_KAT[is.na(myData$B_FIN_ATSK_KAT)] <- "TRUE"
  
  # myData <- mutate_if(myData, is.integer, ~replace(., is.na(.), 0))
  # myData <- mutate_if(myData, is.numeric, ~replace(., is.na(.), 0))
  # myData <- mutate_if(myData, is.character, ~replace(., is.na(.), "Z01"))
  myData[is.na(myData)] <- 0
  ##############################################################################
  
  
  
  ###################################################################################
  ############################### 6. CATEGORY VARIABLES #############################
  # VKR_SEKCIJA,    APS,     !SEGMENTAS-manually!,     KODAS_FORMA-drop,    !laik_tipas_pvm-manually!
  #### USE cat_ranks and cat_values from "additional_data_analysis_2_1.R"!!!!
  #library(plyr) ## Needed for mapvalues(), but conflicts with row_number()....
  
  print(paste0("[",year,"] Converting categorical variables."))
  
  unique(myData$SEGMENTAS)
  myData$SEGMENTAS_MMM <- ifelse(myData$SEGMENTAS == "MMM",1,0)
  myData$SEGMENTAS_DMM <- ifelse(myData$SEGMENTAS == "DMM",1,0)
  myData$SEGMENTAS_VMM <- ifelse(myData$SEGMENTAS == "VMM",1,0)
  # drop myData$SEGMENTAS
  names(myData)
  unique(myData$laik_tipas_pvm)
  myData$laik_tipas_pvm_M <- ifelse(myData$laik_tipas_pvm == "M",1,0)
  myData$laik_tipas_pvm_P <- ifelse(myData$laik_tipas_pvm == "P",1,0)
  # drop myData$laik_tipas_pvm
  
  myData$VKR_SEKCIJA <- plyr::mapvalues(myData$VKR_SEKCIJA, from=cat_values$VKR_SEKCIJA, to=cat_ranks$VKR_SEKCIJA)
  for(unique_rank in sort(unique(myData$VKR_SEKCIJA))){
    myData[paste("VKR_SEKCIJA_", unique_rank, sep = "R")] <- ifelse(myData$VKR_SEKCIJA == unique_rank, 1, 0)
  }
  # drop myData$VKR_SEKCIJA
  
  myData_back <- myData
  myData <- myData_back
  colnames(myData_back)
  #myData$APS <- plyr::mapvalues(myData$APS, from=cat_values$APS, to=cat_ranks$APS)
  #myData <- myData %>% transmute(APS = plyr::mapvalues(myData$APS, from=cat_values$APS, to=cat_ranks$APS))
  #myData$APS <- revalue(myData$APS, c("NA"=3,10=3,8=1,2=2,3=2,5=1,6=1,7=1,1=1,9=1,4=1))
  # myData["APS_R1"] <- ifelse(myData$APS == 1 | myData$APS == 4 | myData$APS == 5 | myData$APS == 6 | myData$APS == 7 | myData$APS == 8 | myData$APS == 9, 1, 0)
  # myData$APS_2 <- ifelse(myData$APS == 2 | myData$APS == 3, 1, 0)
  # myData$APS_3 <- ifelse(myData$APS == 0 | myData$APS == 10, 1, 0)
  
  myData$APS <- plyr::mapvalues(myData$APS, from=cat_values$APS, to=cat_ranks$APS)
  for(unique_rank in sort(unique(myData$APS))){
    myData[paste("APS_", unique_rank, sep = "R")] <- ifelse(myData$APS == unique_rank, 1, 0)
  }
  # drop myData$APS
  
  cols_to_rm <- c("SEGMENTAS", "laik_tipas_pvm", "VKR_SEKCIJA", "APS")
  myData <- myData[, !names(myData) %in% cols_to_rm]
  
  #detach("package:plyr", unload = TRUE)
  ##############################################################################
  
  
  ###################################################################################
  ############################### 7. ALTMAN Z-SCORE #############################
  myData$Z_score <- (1.2*(myData$R_APYVART_KAPITALAS/myData$B_TURTO_IS_VISO) +
                       1.4*(myData$B_GRYNASIS_PELNAS/myData$B_TURTO_IS_VISO) +
                       3.3*(myData$B_PELNAS_PRIES_APMOKEST/myData$B_TURTO_IS_VISO) +
                       0.6*(myData$B_NUOSAVAS_KAPITALAS/myData$R_ISIPAREIGOJIMAI) +
                       0.99*(myData$B_PARDAVIMO_PAJAMOS/myData$B_TURTO_IS_VISO))
                    
  myData$Z_score <- ifelse(is.na(myData$Z_score) | is.infinite(myData$Z_score) | myData$Z_score == 0, 2.75, myData$Z_score)
  ###################################################################################
  print(paste0("[",year,"] Writing data to file."))

  write.csv(myData, paste0('data\\2_clean_columns_NAs\\',year,'.csv'), row.names = FALSE)
  rm(myData)
  gc()
  print("\n")
}
  
back_cat_vals <- cat_values
back_cat_ranks <- cat_ranks
myData_2013 <- read.csv(paste0('data\\2_clean_columns_NAs\\2013.csv'),header=T, stringsAsFactors = FALSE, sep=',')
myData_2014 <- read.csv(paste0('data\\2_clean_columns_NAs\\2014.csv'),header=T, stringsAsFactors = FALSE, sep=',')
myData_2015 <- read.csv(paste0('data\\2_clean_columns_NAs\\2015.csv'),header=T, stringsAsFactors = FALSE, sep=',')
myData_2016 <- read.csv(paste0('data\\2_clean_columns_NAs\\2016.csv'),header=T, stringsAsFactors = FALSE, sep=',')
myData_2017 <- read.csv(paste0('data\\2_clean_columns_NAs\\2017.csv'),header=T, stringsAsFactors = FALSE, sep=',')
myData_2018 <- read.csv(paste0('data\\2_clean_columns_NAs\\2018.csv'),header=T, stringsAsFactors = FALSE, sep=',')
# 
# myData_2013 <- myData_2013[, !names(myData_2013) %in% c("VALST_SAV_SEGMENTAS", "B_SANAUDOS", "B_P1M_GAUT_SUMOS")]
# myData_2014 <- myData_2014[, !names(myData_2014) %in% c("VALST_SAV_SEGMENTAS", "B_SANAUDOS")]
# 
colnames(myData_2013)
colnames(myData_2014)
colnames(myData_2015)
colnames(myData_2018)
# write.csv(myData_2013, paste0('data\\2_clean_columns_NAs\\',2013,'.csv'), row.names = FALSE)
# write.csv(myData_2014, paste0('data\\2_clean_columns_NAs\\',2014,'.csv'), row.names = FALSE)
# write.csv(myData_2015, paste0('data\\2_clean_columns_NAs\\',2015,'.csv'), row.names = FALSE)
# 
# 
# get_VKR_SEKCIJA_rank <- function (category){
#   return(cat_ranks$VKR_SEKCIJA[which(cat_values$VKR_SEKCIJA == category)])
# }
# 
# get_VKR_SEKCIJA_rank("U")
