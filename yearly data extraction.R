setwd('C:\\Users\\Simonas\\Desktop\\Magistrinis\\Magistras GIT') # this line doesn't work on MAC OS
# converts all factors to numeric type, if levels>3 dummyVars could be considered
factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)], as.numeric))

TIME <- Sys.time()

myData <- read.csv('data\\doodle\\im2018.csv',header=T, stringsAsFactors = FALSE, sep=',')

myDataRaw <- myData
myData <- myDataRaw

print(Sys.time() - TIME)

#str(myData)
#head(myData$APRASYMAS_FORMA,20)
#head(myData$ekch,20)
#myData$APRASYMAS_FORMA[1]
#myData$KODAS_STATUSAI[1]
str(myData)

#myData$DATE <- toString(myData$DATE)


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

# Everything left are duplicates
ind_rm <- as.integer(rownames(Bankrupt2018MonthlyOnlyDups))
smallData <- smallData[-ind_rm,]
write.csv(smallData, file="smallData200k_nodups.csv", row.names = FALSE)

##################################

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
