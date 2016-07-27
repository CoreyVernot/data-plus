#Change: Brands (line 12), working directory (line 4), drug (line 85)
#Functions: getNewids
#Drugs for Nathaniel: Thiazoladinediones , Sulfonylureas
#Drugs for Corey: Insulin
#Drugs for Guan-Wun: Metformin

setwd("/Users/corey/Desktop/Data+/data")
library(dplyr)
rx <- read.csv("rx_keep.csv")
iri_week <- read.csv("IRI_week.csv")
rx_w <- merge(rx, iri_week,by.x = "Week", by.y = "IRI.Week")
rx_w$Start.Date <- rx_w$Start.Date %>% as.character() %>% as.Date(format = "%Y-%m-%d")
rx_w$End.Date <- rx_w$End.Date %>% as.character() %>% as.Date(format = "%Y-%m-%d")

brandsMet <- c("Diabinese", "Glucotrol", "Micronase","Glynase", "Diabeta",
               "Amaryl", "chlorpropamide", "glimepiride", "glipizide", "glyburide",
               "tolazamide", "tolbutamide")

panelids1 <- getNewIDs(brandsMet, rx = rx, new = F, HHSizes = c(1))
BrandsMet <- panelids1$Brands[[1]]
ids1 <- panelids1$IDs[[1]]
#Subset ids to only ids without missing metformin prescriptions
rx_m <- rx_w[rx_w$new_id %in% ids1 & rx_w$Rx_Brand %in% BrandsMet, ]
rx_na <- rx_m %>% group_by(new_id) %>% summarise(mean_na = mean(is.na(RxDays)))
ids <- rx_na$new_id[rx_na$mean_na == 0]
d <- as.Date("3000-01-01", format <- "%Y-%m-%d")
rx_range <- data.frame(new_id = ids, min_day = rep(d, length(ids)), max_day = rep(d, length(ids)))
for(i in 1:length(ids)){
  id <- ids[i]
  rx_id <- rx_w[rx_w$new_id == id, ]
  rx_range$min_day[i] <- min(rx_id$Start.Date)
  rx_range$max_day[i] <- max(rx_id$End.Date)
  print(i)
}

rx_m <- merge(rx_m, rx_range, by = "new_id")


monte_carlo<- function(rx_m, id, pt = .95, iter= 100){
  rx_id_master <- rx_m[rx_m$new_id == id, ]
  t <- as.list(rep(NA, iter))
  for(i in 1:iter){
    print(i)
    rx_id <- rx_id_master
    rx_id$fill_day <- as.Date("3000-01-01", format <- "%Y-%m-%d")
    for(k in 1:nrow(rx_id)){
      rx_id$fill_day[k] <- sample(seq(rx_id$Start.Date[k], rx_id$End.Date[k], 1), 1)
    }
    rx_take <- data.frame(day = seq(rx_id$min_day[1], rx_id$max_day[1], 1), supply = NA)
    rx_id$fill <- 1
    rx_id <- rx_id[ , c("fill", "RxDays", "fill_day", "New_Refill_Sample")]
    rx_take <- merge(rx_take, rx_id, by.x = "day", by.y = "fill_day", all.x = T, )
    rx_take$fill[is.na(rx_take$fill)] <- rx_take$RxDays[is.na(rx_take$fill)] <- 0
    
    #If the first prescription observed is a refill, cut off all data prior to first refill. If first rx observed is "New", assign taking before first rx = 0
    first <- min(rx_take$day[rx_take$fill == 1])
    if(rx_take$New_Refill_Sample[rx_take$day == first] == "New"){
      rx_take$supply[rx_take$day < first] <- 0
    }else{
      rx_take <- rx_take[! rx_take$day < first, ]
    }
    
    index <- which(rx_take$day >=first) #the index for all days after and including first drug Rx
    take <- rep(NA, nrow(rx_take))
    
    
    rx_take$supply[index[1]] <- rx_take$RxDays[index[1]]
    take[index[1]] <- rbinom(1, 1, pt) 
    
    for(j in index[-1]){
      rx_take$supply[j] <- rx_take$supply[j-1] + rx_take$RxDays[j] - take[j-1]
      if(rx_take$supply[j] > 0){
        take[j] <- rbinom(1, 1, pt) 
      }else{ take[j] <- 0}
    }
    take <- data.frame(take, day = rx_take$day)
    colnames(take)[1] <- paste("take_", i, sep = "")
    t[[i]] <- take
    print(i)
  }
  take_f <- Reduce(function(...) merge(...), t)
  take_f$mean_take <- rowMeans(take_f[-1])
  to_return <- list(new_id = id, take_f = take_f, pt = pt)
  return(to_return)
}
drug = "sulf" # Change this for every drug
for(i in 1:length(ids)){
  id <- ids[i]
  varname <- paste("mc", id, drug, sep = "_")
  do <- paste(varname, "<- monte_carlo(rx_m, id, pt = .95, iter = 100)", sep = "")
  eval(parse(text = do))
  
  do <- paste("save(", varname, ", file = '", varname, ".RData' )", sep = "")
  eval(parse(text = do))
}
  
