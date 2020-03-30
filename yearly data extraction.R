#setwd('C:\\Users\\Simonas\\Desktop\\Magistrinis\\Magistras GIT')
# Set working directory to script directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# converts all factors to numeric type, if levels>3 dummyVars could be considered
# factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)], as.numeric))

TIME <- Sys.time()

YEAR_DATA = 2015

myData <- read.csv(paste0('data\\raw\\im',YEAR_DATA,'_utf8.csv'),header=T, stringsAsFactors = FALSE, sep=',')

summ <- summary(myData)

####### 1. comparison of datasets - column names ########### 
#columns2019 <- colnames(myData)
#columns2018 <- colnames(myData)
#columns2017 <- colnames(myData)
#columns2016 <- colnames(myData)
#columns2015 <- colnames(myData)
#columns2014 <- colnames(myData)
#columns2013 <- colnames(myData)
#setdiff(columns2018,columns2013)
#setdiff(columns2013,columns2018)

########## The only mismatcch - additional column "B_P1M_GAUT_SUMOS" in 2013 year data

summary(myData)

head(myData)

myDataRaw <- myData
myData <- myDataRaw

print(Sys.time() - TIME)

str(myData)

#!!!!!!!!!!
# pardavimai_pvm_dekl        : chr  " " " " " " " " ...
# pvm_dekl_36                : chr  " " " " " " " " ...
# pirkimai_pvm_dekl          : chr  " " " " " " " " ...
######################## 2. FIX VARIABLES #######################
# Numbers: 1,000.00 are read as chars (change to 1000.00?)
myData$pardavimai_pvm_dekl <- as.numeric(gsub(",","",myData$pardavimai_pvm_dekl))
myData$pvm_dekl_36 <- as.numeric(gsub(",","",myData$pvm_dekl_36))
myData$pirkimai_pvm_dekl <- as.numeric(gsub(",","",myData$pirkimai_pvm_dekl))

summary(myData$pardavimai_pvm_dekl)
summary(myData$pvm_dekl_36)
summary(myData$pirkimai_pvm_dekl)


######################## 3. REMOVE EMPTY OR NON-IMPORTANT VARIABLES (for mixed - month-year data) #######################
# NAUJAS_PVM_MOK (?)
# KLN_KLNT_TYPE
# FIN_ATASKAITA
# gpm, pvm, pm, akc, kiti - empty values
# B_SANAUDOS - no data in 2019, 2018 datasets (???)
# VKR_SEKCIJA, EV1_ID_NAME_TR - similiar variables (EV1_ID_NAME_TR - more detailed VKR_SEKCIJA)
# APRASYMAS_FORMA == KODAS_FORMA (drop APRASYMAS_FORMA)
library(dplyr)
myData <- select(myData, -c(KLN_KLNT_TYPE, NAUJAS_PVM_MOK, FIN_ATASKAITA, gpm, pvm, pm, akc, kiti, VKR_SEKCIJA, APRASYMAS_FORMA))



######################## Find duplicate bankrupt rows ########################
smallData <- myDataRaw[1:200000,]
write.csv(smallData, file="smallData_200k_raw.csv", row.names = FALSE)

# All bankrupt rows (rownames as original dataset row indexes)
Bankrupt2018Monthly <- subset(smallData, KODAS_STATUSAI == 5)
#write.csv(Bankrupt2018Monthly, file="2018_bankrupt.csv", row.names = FALSE)
nrow(Bankrupt2018Monthly)

# Getting row indexes and removing unique values or first of duplicates
# Only duplictaed bankrupt rows remain with original dataset row indexes
ind_rm <- which(!duplicated(Bankrupt2018Monthly$ekch))
length(ind_rm)
Bankrupt2018MonthlyOnlyDups <- Bankrupt2018Monthly[-ind_rm,]
#write.csv(Bankrupt2018MonthlyNoDup, file="2018_bankrupt_no_dups.csv", row.names = FALSE)

# Everything left are duplicates with original row indexes
ind_rm <- as.integer(rownames(Bankrupt2018MonthlyOnlyDups))
# Removing duplicated bakrupt rows, which were extracted earlier
smallData <- smallData[-ind_rm,]
write.csv(smallData, file="smallData200k_nodups.csv", row.names = FALSE)

###############################################################################

########## Variable correleation? - to see which columns are 1:1 () ###########
#str(smallData)
#smallData$pirkimai_pvm_dekl[12000:13000]
#cor(smallData, y=NULL,use="everything", method=c("pearson", "kendall", "spearman"))
#install.packages("polycor")
#library("polycor")
#hetcor(smallData)
#
################################################################################

#myData2018Year <- myData[rownames(myData) %like% "DEC",]
#myData2018Year <- myData[grep("DEC", rownames(myData$DATE)),]
#head(myData2018Year)

#myData %>% filter(str_detect(DATE, "DEC"))
library(stringr)

#TIME <- Sys.time()
#myData2018Year <- myData[str_detect(myData$DATE, "DEC"),]
#print(Sys.time() - TIME)

#summary(myData)

library(dplyr)
myDataYear <- select(filter(myData, str_detect(myData$DATE, "DEC")),everything())
myDataMonth <- select(filter(myData, str_detect(myData$DATE, "DEC",negate = TRUE)),everything())

#paste0('data\\raw\\yearly\\',YEAR_DATA,'_met.csv')

#write.csv(myDataYear, file="data\\raw\\yearly\\2015_met.csv", row.names = FALSE)
#write.csv(myDataMonth, file="data\\raw\\monthly\\2015_men.csv", row.names = FALSE)

write.csv(myDataYear, paste0('data\\raw\\yearly\\',YEAR_DATA,'_met.csv'), row.names = FALSE)
write.csv(myDataMonth, paste0('data\\raw\\monthly\\',YEAR_DATA,'_men.csv'), row.names = FALSE)

summary(myDataMonth)

head(myData)

empty_values_month <- sapply(myDataMonth, function(x){sum(is.na(x))})
empty_values_month[empty_values_month == nrow(myDataMonth)]
length(empty_values_month[empty_values_month == nrow(myDataMonth)])


empty_values <- sapply(myDataYear, function(x){sum(is.na(x))})
empty_values[empty_values == nrow(myDataYear)]
length(empty_values[empty_values == nrow(myDataYear)])


