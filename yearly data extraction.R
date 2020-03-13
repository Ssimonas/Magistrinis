#setwd('C:\\Users\\Simonas\\Desktop\\Magistrinis\\Magistras GIT')
# Set working directory to script directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# converts all factors to numeric type, if levels>3 dummyVars could be considered
# factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)], as.numeric))

TIME <- Sys.time()

myData <- read.csv('data\\doodle\\im2018.csv',header=T, stringsAsFactors = FALSE, sep=',')

myDataRaw <- myData
myData <- myDataRaw

print(Sys.time() - TIME)

str(myData)

#!!!!!!!!!!
# pardavimai_pvm_dekl        : chr  " " " " " " " " ...
# pvm_dekl_36                : chr  " " " " " " " " ...
# pirkimai_pvm_dekl          : chr  " " " " " " " " ...
# laik_tipas_pvm             : chr  "" "" "" "" ...


######################## FIX VARIABLES #######################
# Delete ones with all NA values
# Numbers: 1,000.00 are read as chars (change to 1000.00?)
smallData$pardavimai_pvm_dekl <- as.numeric(smallData$pardavimai_pvm_dekl)
str(smallData)
summary(myData$pardavimai_pvm_dekl)

######################## Find duplicate bankcrupt rows ########################
smallData <- myData[1:200000,]
write.csv(smallData, file="smallData_200k.csv", row.names = FALSE)

# All bankrupt rows (rownames as original dataset row indexes)
Bankrupt2018Monthly <- subset(smallData, KODAS_STATUSAI == 5)
#write.csv(Bankrupt2018Monthly, file="2018_bankrupt.csv", row.names = FALSE)
nrow(Bankrupt2018Monthly)

# Removing unique values or first of duplicates
ind_rm <- which(!duplicated(Bankrupt2018Monthly$ekch))
length(ind_rm)

Bankrupt2018MonthlyOnlyDups <- Bankrupt2018Monthly[-ind_rm,]
#write.csv(Bankrupt2018MonthlyNoDup, file="2018_bankrupt_no_dups.csv", row.names = FALSE)

# Everything left are duplicates with original row indexes
ind_rm <- as.integer(rownames(Bankrupt2018MonthlyOnlyDups))
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
#
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
myData2018Year <- select(filter(myData, str_detect(myData$DATE, "DEC")),everything())
str(myData2018Year)

write.csv(myData2018Year, file="Year2018_R.csv", row.names = FALSE)

summary(myData2018Year)

head(myData)
