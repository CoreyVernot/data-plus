library(dplyr)
setwd("C:/Users/Nathaniel Brown/workspace/BECR/")

#rx$panelid is now rx$new_id
rx <- read.csv("rx_keep.csv")

getNewIDs <- function(... , rx, static_panel = T, Medical_Condition = NA, new = T, only_first_rx = F, use_sample = F){
  allBrands <- list(...)
  #the drugs are vectors of brands that contain a certain drug of interest
  #ID <- rx$panelid[rx$Rx_Brand %in% Drug1,]  #we'll want to return ID's (panel or HH?) of ppl who took this drug and are in a certain household size
  panelIDs <- list()
  
  #cleaning brand names
  rx1 <- rx
  if(!is.na(Medical_Condition)){rx1 <- rx1[rx1$Medical_Condition %in% Medical_Condition, ]}
  #eliminate all whitespace and non-character/number things
  rx1$Rx_Brand_Print <- rx1$Rx_Brand
  rx1$Rx_Brand <- tolower(rx1$Rx_Brand)
  rx1$Rx_Brand <- gsub("[^[:alnum:]]", "", rx1$Rx_Brand)
  allBrands <- lapply( allBrands, function(x){ tolower(gsub("[^[:alnum:]]", "", x))})
  
  for(i in 1:length(allBrands)){
    brands <- allBrands[[i]]
    expr <- paste("(", paste(brands, collapse = "|"), ")", sep = "")
    index <- grep(expr, rx1$Rx_Brand)
    Brands <- unique(rx1$Rx_Brand_Print[index])
    ids <- unique(rx1$new_id[index])

    if(only_first_rx){
      k_table <- findK0(rx = rx, panelids = ids, brands = Brands)
      j_table <- findJ0(rx = rx, panelids = ids, Medical_Condition = Medical_Condition, use_sample = use_sample)
      J_K_table <- merge(j_table, k_table, by = "panelid", all.y = T)
      J_K_table$j_zero_timeunit[is.na(J_K_table$j_zero_timeunit)] <- -100
      use <- J_K_table$k_zero_timeunit <= J_K_table$j_zero_timeunit
      ids <- J_K_table$panelid[use]
    }
    
    if(new){
      rx_new <- rx[rx$New_Refill_Sample == "New", ]
      rx_new <- rx_new[rx_new$Rx_Brand %in% Brands, ]
      ids_new <- unique(rx_new$new_id)
      ids <- ids[ids %in% ids_new]
    }
    
    panelIDs$IDs[[i]] <- ids
    panelIDs$Brands[[i]] <- unique(as.vector((rx1$Rx_Brand_Print[index])))
    cat("Drug brands in database matching drug set ",i,":", "\n",sep="")
    cat(unique(as.vector((rx1$Rx_Brand_Print[index]))),"\n",sep="    ")
    cat("n =",length(ids),"\n","\n")
  }
  return(panelIDs)
}

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

cleanData_impute <- function(rx, panelids, Brands, after_first_rx = T){ #panelids are from getPanelids(which already specify brands)
  IRI_week <- read.csv("IRI_week.csv")
  IRI_week$Start.Date <- as.Date(IRI_week$Start.Date)
  IRI_week$End.Date <- as.Date(IRI_week$End.Date)
  rx_new <- rx[rx$panelid %in% panelids, ]
  rx_new <- rx_new[rx_new$Rx_Brand %in% Brands, ]
  if(mean(is.na(rx_new$RxDays)) != 0){ cat("Warning: NAs in RxDays, use imputed dataset instead")}
  #rx_new <- rx_new[rx_new$New_Refill_Sample == "New", ]
  rx_new2 <- merge(rx_new, IRI_week, by.x = "Week", by.y = "IRI.Week", all.x = T)
  rx_new2 <- rx_new2[, c("panelid", "RxDays", "Week", "Start.Date", "timeunit")]
  rx_new2$Start.Date <- as.Date(rx_new2$Start.Date)
  rx_new2$RxDays <- as.numeric(rx_new2$RxDays)
  rx_new2$EndRx <- rx_new2$Start.Date + rx_new2$RxDays
  colnames(rx_new2)[colnames(rx_new2) == "Start.Date"] <- "StartRx"
  colnames(rx_new2)[colnames(rx_new2) == "timeunit"] <- "StartTimeUnit"
  rx_new2 <- rx_new2[, c(1,2,3,4,6,5)]
  #remove NA automatically
  #retrieve timeunit and time range for EndRx
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
  
  index <- 1
  for(i in 1:nrow(rx_new3)){
    for(j in 1:n[i]){
      tu[index] <- rx_new3$StartTimeUnit[i] + j-1
      index <- index + 1
    }
  }
  rx_new3$TimeUnit <- rx_new3$EndTimeUnit - rx_new3$StartTimeUnit + 1
  rx_new3 <- rx_new3[rep(row.names(rx_new3), rx_new3$TimeUnit), ]
  rx_new4 <- cbind(rx_new3, tu)
  
  IRI_timeunit <- read.csv("IRI_timeunit.csv")
  rx_new4 <- merge(rx_new4, IRI_timeunit, by.x = "tu", by.y = "timeunit", all.x = T)
  rx_new4 <- rx_new4[, c(2,3,4,5,6,7,8,9,1,12,13)]
  rx_new4$end <- as.Date(rx_new4$end)
  rx_new4$start <- as.Date(rx_new4$start)
  rx_new4$timeRange <- ifelse((rx_new4$StartRx <= rx_new4$end) & (rx_new4$StartRx >= rx_new4$start), rx_new4$end - rx_new4$StartRx + 1,
                              ifelse((rx_new4$EndRx <= rx_new4$end) & (rx_new4$EndRx >= rx_new4$start), rx_new4$EndRx - rx_new4$start + 1, 28))
  rx_new4$percentage <- rx_new4$timeRange / 28
  #summary for the percentage taking of each prescription in individuals
  rx_sum <- rx_new4 %>% group_by(panelid, tu) %>% summarize(taking = sum(percentage))
  rx_sum$taking2 <- rx_sum$taking
  for(i in 1:nrow(rx_sum)){
    rx_sum$taking[i] <- min(rx_sum$taking[i], 1)
  }
  #hist(rx_sum$taking)
  
  #merge by panelids
  
  id_tu <- PanelID_time(panelids)
  rx_full <- merge(rx_sum, id_tu, by = c("panelid", "tu"), all = T)
  rx_full$taking[is.na(rx_full$taking)] <- 0
  rx_full$taking2[is.na(rx_full$taking2)] <- 0
  hist(rx_full$taking)
  
  if(after_first_rx){
    rx_first <- rx_full %>% subset(taking > 0) %>% group_by(panelid) %>% summarize(first_tu = min(tu))
    rx_full <- merge(rx_full, rx_first, by = "panelid")
    rx_full <- rx_full[rx_full$first_tu >= rx_full$tu, ]
  }
  colnames(rx_full)[colnames(rx_full) == "tu"] <- "timeunit"
  return(rx_full)
}

#num_zero <- rx_full %>% group_by(panelid) %>% summarize(per_zero = mean(taking == 0))

#zero_rx4 <- rx_new4 %>% group_by(panelid) %>% summarize(per_zero = mean(percent == 0))
#length(unique(rx_new4$panelid))

#Panelids <- panelids
#panelids <- Panelids$IDs[[1]]



#graph for each individual
#rx_full2 <- rx_full[, c(1,3)]
#df <- melt(rx_full2, id.vars = 'taking', variable.name = 'panelid')
#ggplot(df, aes(taking, panelid))+geom_line()+facet_grid(panelid ~ .)

#rx_samp <- rx[rx$panelid %in% panelids, ]
#id_range <- rx_samp %>% group_by(panelid) %>% summarize(max_week = max(Week), min_week = min(Week))
#id_range_met <- rx_samp[rx_samp$Rx_Brand %in% Brands, ]%>% group_by(panelid) %>% summarize(max_week = max(Week), min_week = min(Week))

#id_range_comp <- merge(id_range, id_range_met, by = "panelid")

#id_range2 <- merge(id_range, IRI_week, by.x = "min_week", by.y = "IRI.Week", all.x = T)
#colnames(id_range2)[colnames(id_range2) == "timeunit"] <- "min_tu"
#id_range3 <- merge(id_range2, IRI_week, by.x = "max_week", by.y = "IRI.Week", all.x = T)
#colnames(id_range3)[colnames(id_range3) == "timeunit"] <- "max_tu"

#id_range_m <- id_range3[, colnames(id_range3) %in% c("panelid", "min_tu", "max_tu")]
#rx_full_m <- merge(rx_full, id_range_m)
#pdf("taking_by_person_bb.pdf", height = 450 )
#par(mfrow = c(300, 2))
#id_sample <- sample(panelids, size = 600)
#for(i in 1:600){
#  plot(taking ~ tu, data = rx_full[rx_full$panelid == id_sample[i], ])
#  abline(v = id_range3$max_tu[id_range3$panelid == id_sample[i]], col = "red" )
#  abline(v = id_range3$min_tu[id_range3$panelid == id_sample[i]], col = "blue" )
#  
#}
#dev.off()

