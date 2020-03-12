setwd('C:\\Users\\Simonas\\Desktop\\Magistrinis\\R') # this line doesn't work on MAC OS
# converts all factors to numeric type, if levels>3 dummyVars could be considered
factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)], as.numeric))

TIME <- Sys.time()

myData <- read.csv('data\\doodle\\im2018.csv',header=T, stringsAsFactors = FALSE, sep=',')

print(Sys.time() - TIME)

#str(myData)
#head(myData$APRASYMAS_FORMA,20)
#head(myData$ekch,20)
#myData$APRASYMAS_FORMA[1]
#myData$KODAS_STATUSAI[1]
#str(myData)

myData$DATE <- toString(myData$DATE)

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

write.csv(myData2018Year, file="Year2018.csv", row.names = FALSE)

summary(myData2018Year)

head(myData)
