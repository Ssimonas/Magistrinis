####### This script transforms selected data (extracted earlier) - to be suitbale for machine learning models.

YEAR_TO_PREDICT = 2018 # This year will not be used for prediction
NUM_OF_YEARS_TO_USE = 2 # X more years will be used for training

data_range <- seq(YEAR_TO_PREDICT-NUM_OF_YEARS_TO_USE, YEAR_TO_PREDICT, by=1)

### read all yearly data
filePaths <- paste0('data\\raw\\yearly\\',data_range,'_met.csv')

main_data <- lapply(filePaths, read.csv)
## [[1]] - 2015, [[2]] - 2016 ir t.t.

colnames(main_data[[1]])[1] <- "ID"
colnames(main_data[[2]])[1] <- "ID"
colnames(main_data[[3]])[1] <- "ID"

data2016 <- main_data[[1]]
data2017 <- main_data[[2]]
data2018 <- main_data[[3]]

colnames(data2016) <- paste(colnames(main_data[[1]]), data_range[1],sep="_")
colnames(data2017) <- paste(colnames(main_data[[2]]), data_range[2],sep="_")
colnames(data2018) <- paste(colnames(main_data[[3]]), data_range[3],sep="_")

colnames(data2016)[1] <- "ID"
colnames(data2017)[1] <- "ID"
colnames(data2018)[1] <- "ID"

test <- merge(data2016, data2017, by = "ID", all = TRUE, suffixes = c("_2015","_2016"))
test2 <- merge(test, data2018, by = "ID", all = TRUE, suffixes = c("_22222","_2017"))
colnames(test)
colnames(test2)

write.csv(test2, paste0('data\\raw\\yearly\\2016-2018_met.csv'), row.names = FALSE)
