library(dplyr)

PanelID_week <- function(panelids){ #helper method: get IDs from getPanelIDs
  ids <- c()
  for(i in 1:length(panelids)){
    add <- rep(panelids[i], 499)
    ids <- c(ids, add)
  }
  wk <- rep(1369:1867, length(panelids))
  id_wk <- data.frame(panelid = ids, wk = wk)
  return(id_wk)
}
#returns a data frame of every panelid paired with every week

PanelID_time <- function(panelids){ #helper method: get IDs from getPanelIDs
  ids <- c()
  for(i in 1:length(panelids)){
    add <- rep(panelids[i], 125)
    ids <- c(ids, add)
  }
  tu <- rep(1:125, length(panelids))
  id_tu <- data.frame(panelid = ids, tu = tu)
  return(id_tu)
}
#returns a data frame of every panelid paired with every timeunit

cleanData_impute <- function(rx, panelids, Brands,  id_tu){ #panelids are from getPanelids(which already specify brands)
  IRI_week <- read.csv("IRI_week.csv")
  IRI_week$Start.Date <- as.Date(IRI_week$Start.Date)
  IRI_week$End.Date <- as.Date(IRI_week$End.Date)
  rx_new <- rx[rx$panelid %in% panelids, ] #subset rx data by specified panelids
  rx_new <- rx_new[rx_new$Rx_Brand %in% Brands, ] #and specified brands
  rand <- sample(1:sum(is.na(rx_new$RxDays)))
  rx_new$RxDays[is.na(rx_new$RxDays)][rand] <- 90
  rx_new$RxDays[is.na(rx_new$RxDays)][-rand] <- 90 #some of the NAs are now 90???
  
  #rx_new <- rx_new[rx_new$New_Refill_Sample == "New", ] this line was already commented out so I'm not sure if it's important
  rx_new2 <- merge(rx_new, IRI_week, by.x = "Week", by.y = "IRI.Week", all.x = T)
  rx_new2 <- rx_new2[, c("panelid", "RxDays", "Week", "Start.Date", "timeunit")]
  rx_new2$Start.Date <- as.Date(rx_new2$Start.Date)
  rx_new2$EndRx <- rx_new2$Start.Date + rx_new2$RxDays
  colnames(rx_new2)[colnames(rx_new2) == "Start.Date"] <- "StartRx"
  colnames(rx_new2)[colnames(rx_new2) == "timeunit"] <- "StartTimeUnit"
  #rx_new2 <- rx_new2[, c(1,2,3,4,6,5)]
  
  #remove NA automatically
  temp <- merge(rx_new2, IRI_week)
  rx_new3 <- subset(temp,  EndRx >= Start.Date & EndRx <= End.Date)
  rx_new3 <- rx_new3[, c("panelid", "RxDays", "Week", "IRI.Week", "StartRx", "EndRx", "StartTimeUnit", "timeunit")]
  colnames(rx_new3)[colnames(rx_new3) == "Week"] <- "StartWeek"
  colnames(rx_new3)[colnames(rx_new3) == "IRI.Week"] <- "EndWeek"
  colnames(rx_new3)[colnames(rx_new3) == "timeunit"] <- "EndTimeUnit"
  #add rows based on the number of timeunits in RxDays
  n <- rx_new3$EndTimeUnit - rx_new3$StartTimeUnit + 1
  nrow = sum(n)
  tu <- rep(NA, nrow)
  wk <- rep(NA, nrow)
  
  index <- 1
  for(i in 1:nrow(rx_new3)){
    for(j in 1:n[i]){
      tu[index] <- rx_new3$StartTimeUnit[i] + j-1
      wk[index] <- rx_new3$StartWeek[i]
      index <- index + 1
    }
  }
  rx_new3$TimeUnit <- rx_new3$EndTimeUnit - rx_new3$StartTimeUnit + 1
  rx_new3 <- rx_new3[rep(row.names(rx_new3), rx_new3$TimeUnit), ]
  rx_new4 <- cbind(rx_new3, tu, wk)
  
  IRI_timeunit <- read.csv("IRI_timeunit.csv")
  rx_new4 <- merge(rx_new4, IRI_timeunit, by.x = "tu", by.y = "timeunit", all.x = T)
  #rx_new4 <- rx_new4[, colnames(rx_new4) %in% c(2,3,4,5,6,7,8,9,1,12,13)]
  rx_new4$end <- as.Date(rx_new4$end)
  rx_new4$start <- as.Date(rx_new4$start)
  rx_new4$timeRange <- ifelse((rx_new4$StartRx <= rx_new4$end) & (rx_new4$StartRx >= rx_new4$start), rx_new4$end - rx_new4$StartRx + 1,
                              ifelse((rx_new4$EndRx <= rx_new4$end) & (rx_new4$EndRx >= rx_new4$start), rx_new4$EndRx - rx_new4$start + 1, 28))
  rx_new4$percentage <- rx_new4$timeRange / 28
  #summary for the percentage taking of each prescription in individuals
  rx_sum <- rx_new4 %>% group_by(panelid, tu, wk) %>% summarize(taking = sum(percentage))
  rx_sum$taking2 <- rx_sum$taking
  for(i in 1:nrow(rx_sum)){
    rx_sum$taking[i] <- min(rx_sum$taking[i], 1)
  }
  #hist(rx_sum$taking)
  
  #merge by panelids
  
  id_wk <- PanelID_week(panelids)
  rx_full <- merge(rx_sum, id_wk, by = c("panelid", "wk"), all = T)
  rx_full$taking[is.na(rx_full$taking)] <- 0
  rx_full$taking2[is.na(rx_full$taking2)] <- 0
  hist(rx_full$taking)
  
  return(rx_full)
}

num_zero <- rx_full %>% group_by(panelid) %>% summarize(per_zero = mean(taking == 0))

zero_rx4 <- rx_new4 %>% group_by(panelid) %>% summarize(per_zero = mean(percent == 0))
length(unique(rx_new4$panelid))

Panelids <- panelids
panelids <- Panelids$IDs[[1]]



#graph for each individual
rx_full2 <- rx_full[, c(1,3)]
df <- melt(rx_full2, id.vars = 'taking', variable.name = 'panelid')
ggplot(df, aes(taking, panelid))+geom_line()+facet_grid(panelid ~ .)


IRI_week <- read.csv("IRI_week.csv")

rx_samp <- rx[rx$panelid %in% statin_ids2_good, ]

id_range <- rx_samp %>% group_by(panelid) %>% summarize(max_week = max(Week), min_week = min(Week))
#id_range_met <- rx_samp[rx_samp$Rx_Brand %in% Brands, ]%>% group_by(panelid) %>% summarize(max_week = max(Week), min_week = min(Week))

id_range_comp <- merge(id_range, id_range_met, by = "panelid")

id_range2 <- merge(id_range, IRI_week, by.x = "min_week", by.y = "IRI.Week", all.x = T)
colnames(id_range2)[colnames(id_range2) == "timeunit"] <- "min_tu"
id_range3 <- merge(id_range2, IRI_week, by.x = "max_week", by.y = "IRI.Week", all.x = T)

colnames(id_range3)[colnames(id_range3) == "timeunit"] <- "max_tu"

id_range <- id_range3[, colnames(id_range3) %in% c("panelid", "min_tu", "max_tu", "min_week", "max_week")]
id_range <- cbind(id_range$panelid,id_range$min_tu,id_range$max_tu,id_range$min_week,id_range$max_week) #simply reordering the columns
colnames(id_range) <- c("panelid", "min_tu", "max_tu", "min_week", "max_week")
somethings_wrong <- merge(rx_full,id_range)


View(rx_full)


good_ids <- demo$panelid[demo$projection61k_2008 > 0 &
                           demo$projection61k_2009 > 0 &
                           demo$projection61k_2010 > 0 &
                           demo$projection61k_2011 > 0 &
                           demo$projection61k_2012 > 0 ]

statin_ids_good <- statin_ids[statin_ids %in% good_ids]
pdf("taking_by_person_statin_tu2.pdf", height = 450 )
par(mfrow = c(300, 2))
id_sample <- sample(statin_ids2_good, size = 600)
for(i in 1:600){
  plot(taking ~ tu, xlab = paste("tu",id_sample[i]), data = rx_sum[rx_sum$panelid == id_sample[i], ])
  #abline(v = id_range3$max_tu[id_range3$panelid == id_sample[i]], col = "red" )
  #abline(v = id_range3$min_tu[id_range3$panelid == id_sample[i]], col = "blue" )
  
  #abline(v = id_range$max_week[id_range$panelid == id_sample[i]], col = "red" )
  #abline(v = id_range$min_week[id_range$panelid == id_sample[i]], col = "blue" )
}
dev.off()

a <- rx_full_wk[rx_full_wk$panelid == id_sample[1],]
View(a)


StatinList <- getPanelIDs(statin_brands, rx = rx, demo = demo, HHSizes = 2)
statin_ids2 <- StatinList[[1]][[1]]
length(statin_ids2)
statin_ids2_good <- statin_ids2[statin_ids2 %in% good_ids]
statin2_wk <- PanelID_week(statin_ids2_good)
statin2_tu <- PanelID_time(statin_ids2_good)

rx_full_wk <- cleanData_impute(rx=rx,panelids=statin_ids2_good,Brands=statin_brands,id_tu=statin2_wk)
rx_full_tu <- cleanData_impute(rx=rx,panelids=statin_ids2_good,Brands=statin_brands,id_tu=statin2_tu)


strange_id <- 902007451