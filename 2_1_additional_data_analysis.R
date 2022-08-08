


myData_additional <- read.csv(paste0('data\\4_ready_data\\FULL_DATA_no2018.csv'),header=T, stringsAsFactors = FALSE, sep=',')
myData2018 <- read.csv(paste0('data\\01_raw\\im2018_utf8.csv'),header=T, stringsAsFactors = FALSE, sep=',')
myData2017 <- read.csv(paste0('data\\01_raw\\im2017_utf8.csv'),header=T, stringsAsFactors = FALSE, sep=',')
myData2016 <- read.csv(paste0('data\\01_raw\\im2016_utf8.csv'),header=T, stringsAsFactors = FALSE, sep=',')
myData2015 <- read.csv(paste0('data\\01_raw\\im2015_utf8.csv'),header=T, stringsAsFactors = FALSE, sep=',')
myData2014 <- read.csv(paste0('data\\01_raw\\im2014_utf8.csv'),header=T, stringsAsFactors = FALSE, sep=',')
myData2013 <- read.csv(paste0('data\\01_raw\\im2013_utf8.csv'),header=T, stringsAsFactors = FALSE, sep=',')

names(myData2013)[1] <- "ID"
names(myData2014)[1] <- "ID"
names(myData2015)[1] <- "ID"
names(myData2016)[1] <- "ID"
names(myData2017)[1] <- "ID"
names(myData2018)[1] <- "ID"
##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################
# VKR_SEKCIJA,    APS,     !SEGMENTAS!,     KODAS_FORMA,    !laik_tipas_pvm!
variable <- "VKR_SEKCIJA"
variable <- "APS"
variable <- "KODAS_FORMA"

new_set2013 <- myData2013 %>% select(ID, matches(variable), KODAS_STATUSAI)
new_set2014 <- myData2014 %>% select(ID, matches(variable), KODAS_STATUSAI)
new_set2015 <- myData2015 %>% select(ID, matches(variable), KODAS_STATUSAI)
new_set2016 <- myData2016 %>% select(ID, matches(variable), KODAS_STATUSAI)
new_set2017 <- myData2017 %>% select(ID, matches(variable), KODAS_STATUSAI)
new_set2018 <- myData2018 %>% select(ID, matches(variable), KODAS_STATUSAI)

myData <- rbind(new_set2013, new_set2014, new_set2015, new_set2016, new_set2017, new_set2018)

myData$KODAS_STATUSAI[is.na(myData$KODAS_STATUSAI)] <- 0

bankr <- myData[myData$KODAS_STATUSAI == 5,] %>% select(ID, matches(variable))
bankr <- unique(bankr)
bankr$STATUS <- 1
nrow(bankr)

new_set <- myData %>% select(ID, matches(variable))
new_set <- unique(new_set)
new_set$STATUS <- 0
nrow(new_set)

new_set <- new_set[!new_set$ID %in% bankr$ID,]
new_set <- rbind(new_set, bankr)
nrow(new_set)

summary_ratios <- c()
summary_values <- c()

iterator <- 1
for(unique_value in unique(myData$KODAS_FORMA)){
  amount_healthy <- length(new_set$ID[new_set$KODAS_FORMA == unique_value & new_set$STATUS == 0])
  amount_bankrupt <- length(new_set$ID[new_set$KODAS_FORMA == unique_value & new_set$STATUS == 1])
  cat(paste0("Value: ",unique_value,"\tHealthy: ",amount_healthy,"\tBankrupt: ",amount_bankrupt,"\tRatio:",round(amount_bankrupt/(amount_healthy+amount_bankrupt),4),"\n"))
  summary_ratios[iterator] <- round(amount_bankrupt/(amount_healthy+amount_bankrupt),4)
  summary_values[iterator] <- unique_value
  #mydata[paste("nationality", unique_value, sep = ".")] <- ifelse(mydata$nationality == unique_value, 1, 0)
  iterator <- iterator + 1
}

divide_into <- 5 ## transform into (divide_into + 1) categories
step <- (max(summary_ratios)-min(summary_ratios))/50
summary_rank <- summary_ratios %/% step + 1

summary_values
summary_rank

# cat_values <- c()
# cat_ranks <- c()
cat_values$KODAS_FORMA <- summary_values
cat_ranks$KODAS_FORMA <- summary_rank


#######################
cat_ranks
cat_values
#######################
