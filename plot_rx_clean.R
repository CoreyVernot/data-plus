#day/taking/new_id####

library(ggplot2)
library(dplyr)
#graph for each individual

#graph which makes everybody in the same time range####
#days start from min date of metformin to the max date of metformin prescription observed in the dataset
#min_day <- min(days_take_met$day)
#max_day <- max(days_take_met$day)
#all_days <- as.data.frame(seq(min_day, max_day,by = 1))
#colnames(all_days) <- "date"
#repeat the dates for all ids
#n = length(unique(days_take_met$new_id)) #856
#m = length(all_days$date) #1099
#d <- rep(NA, n*m)
#index <- 1
#for(i in 1:nrow(rx_new3)){
#  for(j in 1:n[i]){
#    tu[index] <- rx_new3$StartTimeUnit[i] + j-1
#    index <- index + 1
#  }
#}
#days_take_met1 <- merge(days_take_met, all_days, by.x = day)


#graph which has different time range for every individual####

#time starts from a person's first prescription and ends at the last observed prescriptions
#data frame from rx with start and end observed dates for each individual
brands <- c("Fortamet", "Glucophage", "Glumetza", "Riomet", "Obimet",
            "Dianben", "Diaformin", "Siofor", "Metfogamma", "Janumet",
            "Kazano", "Invokamet", "Xigduo", "Synjardy", "Metaglip" ,
            "Jentaduo" , "Actoplus", "Prandimet", "Avandamet", "Kombiglyze", 
            "Glumetza", "Metformin")
metBrands <- getNewIDs(brands, rx=rx_keep)
  #Drug brands in database matching drug set 1:
  #METFORMIN (ALL)    JANUMET    GLUCOPHAGE (ALL)    AVANDAMET    METFORMIN    GLUCOPHAGE    
  #n = 980 
Brands <- metBrands$Brands[[1]]
IDs <- metBrands$IDs[[1]]
days_take_met <- days_sure_taking(rx=rx_keep, ids=IDs, brands = Brands, iri_week_dir = "D:/Duke Grad/2016 Summer/keep/tables")
met_ids <- unique(days_take_met$new_id)
rx_observe <- rx_keep[,c("Week","new_id")]
rx_observe <- rx_observe[rx_observe$new_id %in% met_ids,]
rx_observe <- rx_observe %>% group_by(new_id) %>% summarise(min_week = min(Week), max_week = max(Week))
rx_observe_date <- merge(rx_observe, IRI_week, by.x = "min_week", by.y = "IRI.Week")
rx_observe_date <- rx_observe_date[,c("new_id","min_week","Start.Date","max_week")]
#rx_observe_date <- rename(rx_observe_date, start = Start.Date)
rx_observe_date_fin <- merge(rx_observe_date, IRI_week, by.x = "max_week", by.y = "IRI.Week")
rx_observe_date_fin <- rx_observe_date_fin[,c("new_id","min_week","Start.Date.x","max_week","End.Date")]
rx_observe_date_fin <- rename(rx_observe_date_fin, start = Start.Date.x, end = End.Date)
rx_observe_date_fin$start <- as.Date(rx_observe_date_fin$start)
rx_observe_date_fin$end <- as.Date(rx_observe_date_fin$end)

#data frame with all the dates between start and end dates for each person
day_id <- data.frame(new_id = c(), day = c())
ids <- unique(rx_observe_date_fin$new_id)
for(i in 1:length(ids)){
  print(i)
  rx_id <- rx_observe_date_fin[rx_observe_date_fin$new_id == ids[i], ]
  days <- seq.Date(from = rx_id$start, to = rx_id$end, by = 1)
  to_bind <- data.frame(new_id = ids[i], day = days)
  day_id <- rbind(day_id, to_bind)
}

#merge the full set of days with the set of days in our drug data
day_take <- merge(day_id, days_take_met, by = c("new_id", "day"), all = T) 
day_take$taking[is.na(day_take$taking)] <- 0

s = 100
pdf("day_taking_by_person_met.pdf", height = s*.7 )
par(mfrow = c(s/2,2), mar = c(2,3,2,1)+ .1, oma = c(0, 0, 0, 0))
new_ids <- unique(day_take$new_id)
id_sample <- sample(new_ids, size = s)
for(i in 1:s){
  day_take_id <- day_take[day_take$new_id == id_sample[i], ]
  plot(day_take_id$taking ~ day_take_id$day, pch = NA)
  text(y = day_take_id$taking, x = day_take_id$day, labels="|", col="black", offset=0.6)
  
}
dev.off()
