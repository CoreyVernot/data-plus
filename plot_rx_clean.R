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
brands_sulf <- c("Diabinese", "Glucotrol", "Micronase","Glynase", "Diabeta",
                 "Amaryl", "chlorpropamide", "glimepiride", "glipizide", "glyburide",
                 "tolazamide", "tolbutamide")

panelids <- getNewIDs(brands_sulf, rx = rx_keep, HHSizes = c(1,2))
ids_sulf <- panelids$IDs[[1]]
Brands_sulf <- panelids$Brands[[1]]
take_sulf <- days_sure_taking(rx=rx_keep, ids = ids_sulf, brands = Brands_sulf, iri_week_dir = iri_week_dir, take_name = "taking_sulf")

brand_met <- c("Fortamet", "Glucophage", "Glumetza", "Riomet", "Obimet",
               "Dianben", "Diaformin", "Siofor", "Metfogamma", "Janumet",
               "Kazano", "Invokamet", "Xigduo", "Synjardy", "Metaglip" ,
               "Jentaduo" , "Actoplus", "Prandimet", "Avandamet", "Kombiglyze", 
               "Glumetza", "Metformin")
metIDs <- getNewIDs(brand_met, rx=rx_keep, HHSizes = c(1,2))
  #all household sizes
  #Drug brands in database matching drug set 1:
  #METFORMIN (ALL)    JANUMET    GLUCOPHAGE (ALL)    AVANDAMET    METFORMIN    GLUCOPHAGE    
  #n = 980
  #household size = 1
  #Drug brands in database matching drug set 1:
  #METFORMIN (ALL)    JANUMET    GLUCOPHAGE (ALL)    AVANDAMET    METFORMIN    GLUCOPHAGE    
  #n = 109 
Brands_met <- metIDs$Brands[[1]]
ids_met <- metIDs$IDs[[1]]
take_met <- days_sure_taking(rx=rx_keep, ids=ids_met, brands = Brands_met, take_name = "taking_met", iri_week_dir = "D:/Duke Grad/2016 Summer/keep/tables")

dpp_4_inhibitors <- c("Januvia","Onglyza","Tradjenta","Nesina","Glyxambi","Oseni")
dppIDs <- getNewIDs(dpp_4_inhibitors, rx=rx_keep, HHSizes = c(1,2))
brand_dpp <- dppIDs$Brands[[1]]
ids_dpp <- dppIDs$IDs[[1]]
take_dpp <- days_sure_taking(rx=rx_keep, ids = ids_dpp, brands = brand_dpp, take_name = "taking_dpp", iri_week_dir = iri_week_dir)

thio <- c("Avandia", "ACTOS", "Rezulin", "piogitazone", "rosiglitazone")
thioIDs <- getNewIDs(thio, rx=rx_keep, HHSizes = c(1,2))
brand_thio <- thioIDs$Brands[[1]]
ids_thio <- thioIDs$IDs[[1]]
take_thio <- days_sure_taking(rx=rx_keep, ids = ids_thio, brands = brand_thio, take_name = "taking_thio", iri_week_dir = iri_week_dir)



full_date_range <- function(days_take_drug, rx_keep, IRI_week){
  met_ids <- unique(days_take_drug$new_id)
  rx_observe <- rx_keep[,c("Week","new_id")]
  rx_observe <- rx_observe[rx_observe$new_id %in% met_ids,]
  rx_observe <- rx_observe %>% group_by(new_id) %>% summarise(min_week = min(Week), max_week = max(Week))
#need to load IRI_week
  rx_observe_date <- merge(rx_observe, IRI_week, by.x = "min_week", by.y = "IRI.Week")
  rx_observe_date <- rx_observe_date[,c("new_id","min_week","Start.Date","max_week")]
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
  day_take <- merge(day_id, days_take_drug, by = c("new_id", "day"), all = T)
  varname <- grep("taking", colnames(day_take), value = T)
  colnames(day_take)[colnames(day_take) == varname] <- "taking"
  day_take$taking[is.na(day_take$taking)] <- 0
  colnames(day_take)[colnames(day_take) == "taking"] <- varname
  
  return(day_take)
}

take_met_f <- full_date_range(days_take_drug = take_met, rx_keep = rx_keep, IRI_week = IRI_week)
take_sulf_f <- full_date_range(days_take_drug = take_sulf, rx_keep = rx_keep, IRI_week = IRI_week)
take_dpp_f <- full_date_range(days_take_drug = take_dpp, rx_keep = rx_keep, IRI_week = IRI_week)
take_thio_f <- full_date_range(days_take_drug = take_thio, rx_keep = rx_keep, IRI_week = IRI_week)


take_diabetes <- merge(take_met_f, take_sulf_f, by = c("new_id", "day"), all = T )
take_diabetes <- merge(take_diabetes, take_dpp_f, by = c("new_id", "day"), all = T )
take_diabetes <- merge(take_diabetes, take_thio_f, by = c("new_id", "day"), all = T )

take_diabetes$taking_sulf[is.na(take_diabetes$taking_sulf)] <- 0
take_diabetes$taking_met[is.na(take_diabetes$taking_met)] <- 0
take_diabetes$taking_thio[is.na(take_diabetes$taking_thio)] <- 0
take_diabetes$taking_dpp[is.na(take_diabetes$taking_dpp)] <- 0


#create plots for each person (draw samples)
s = 18
pdf("day_taking_diabetes_18.pdf", height = s*.7 )
par(mfrow = c(s/2,2), mar = c(2,3,2,1)+ .1, oma = c(0, 0, 0, 0))
new_ids <- ids_met[ids_met %in% unique(take_diabetes$new_id)]
#new_ids <- unique(take_diabetes$new_id)
#id_sample <- sample(new_ids, size = s)
id_sample <- c(211103, 57144, 225451,  86556, 56184, 93774, 105788, 245104, 241757, 251783, 254937, 95225, 251979, 255830, 50331, 103366, 79156, 95706) 
sep = .07
cex = .25
for(i in 1:s){
  print(i)
  take_diabetes_id <- take_diabetes[take_diabetes$new_id == id_sample[i], ]
  plot(take_diabetes_id$taking_met ~ take_diabetes_id$day, pch = NA, ylim = c(-.2, 1.2),
       xlim = c(min(take_diabetes_id$day), max(take_diabetes_id$day) + .75*sd(take_diabetes_id$day)) , xlab = "days", ylab = "taking")
  text(y = take_diabetes_id$taking_sulf + sep, x = take_diabetes_id$day, labels="|", col="blue", cex = cex)
  text(y = take_diabetes_id$taking_met + 2*sep, x = take_diabetes_id$day, labels="|", col="black", cex = cex)
  text(y = take_diabetes_id$taking_dpp - sep, x = take_diabetes_id$day, labels="|", col="red",  cex = cex)
  text(y = take_diabetes_id$taking_thio - 2*sep, x = take_diabetes_id$day, labels="|", col="green", cex = cex)
  legend(x = max(take_diabetes_id$day) + .2*sd(take_diabetes_id$day), y = .9, legend = c("met", "sulf", "dpp", "thio"), 
         fill = c("black", "blue", "red", "green"), cex = .85)
  #text(x = mean(take_diabetes_id$day), y = .6, labels = as.character(id_sample[i]))
}
dev.off()

take_ids <- take_diabetes[take_diabetes$new_id %in% id_sample, ]
num_days <- take_ids %>% group_by()