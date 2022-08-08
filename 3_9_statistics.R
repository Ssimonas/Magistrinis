month.abb <- toupper(month.abb)

summary_stats <- data.frame(matrix(ncol=6, nrow=12))
cnames <- c("2013", "2014", "2015", "2016", "2017", "2018")
colnames(summary_stats) <- cnames
rownames(summary_stats) <- month.abb

summary_stats[is.na(summary_stats)] <- 0

monthly_yearly_healthy <- summary_stats
monthly_yearly_bankrupt <- summary_stats

for(month_num in 1:12){
  #month_num <- 1
  myData1 <- read.csv(paste0("data\\3_data_to_rows\\MONTHLY\\",month_num,"\\test_",month_num,"_only2018.csv"))
  myData2 <- read.csv(paste0("data\\3_data_to_rows\\MONTHLY\\",month_num,"\\train_",month_num,"_no2018.csv"))
  myData1 <- rbind(myData1,myData2)
  

  month <- month.abb[month_num]
  
  for(year in cnames){
    #year <- "2018"
    num_of_healty <- length(myData1$ID[substring(myData1$DATE,3,5) == month & as.numeric(substring(myData1$DATE,6,9)) == year & myData1$STATUS == 0])
    num_of_bankr <- length(myData1$ID[substring(myData1$DATE,3,5) == month & as.numeric(substring(myData1$DATE,6,9)) == year & myData1$STATUS == 1])
    
    monthly_yearly_healthy[month,year] <- num_of_healty
    
    monthly_yearly_bankrupt[month,year] <- num_of_bankr
  }
  mean(myData1$IMONES_AMZIUS_MM_Y_1[substring(myData1$DATE,3,5) == month & as.numeric(substring(myData1$DATE,6,9)) == year & myData1$STATUS == 1])
    
}

write.csv(monthly_yearly_healthy, file =paste0("results\\bankrupt_stats_healthy.csv"))
write.csv(monthly_yearly_bankrupt, file =paste0("results\\bankrupt_stats_bankr.csv"))


