cleanData <- function(trans, rx, brands){
  key_timeunit <- timeunitKey()
  panelids <- unique(trans$panelid)
  k_table <- findK0(rx = rx, panelids = panelids, brands = brands)
  trans <- trans[!(is.na(trans$totoz) & is.na(trans$floz)), ]
  trans$purdate2 <- as.Date(trans$purdate2, format = "%m-%d-%y")
  trans_rx <- merge(trans, k_table, by = "panelid", all.x = T)
  trans_rx <- merge(trans_rx, key_timeunit, by.x = "purdate2", by.y = "date", all.x = T)
  trans_rx$k <- trans_rx$timeunit - trans_rx$k_zero_timeunit
  trans_rx <- trans_rx[!(is.na(trans_rx$nfp_serv_size_us)),]
  tot_amount <- rowSums(trans_rx[, c("totfloz", "totoz")], na.rm = T)
  trans_rx$nfp_serv_size_us[trans_rx$nfp_serv_size_us < .001] <- NA
  trans_rx$num_serv <- tot_amount / trans_rx$nfp_serv_size_us
  
  if(Reduce("|", grepl("nfp_calories", colnames(trans_rx)))){
    trans_rx$nfp_calories[trans_rx$nfp_calories < 0] <- NA
    trans_rx$pur_calories <- trans_rx$num_serv*trans_rx$nfp_calories
  }else{trans_rx$pur_calories <- NA}
  if(Reduce("|", grepl("nfp_tot_fat", colnames(trans_rx)))){
    trans_rx$nfp_tot_fat[trans_rx$nfp_tot_fat < 0] <- NA
    trans_rx$pur_fat <- trans_rx$num_serv*trans_rx$nfp_tot_fat
  }else{trans_rx$pur_fat <- NA}
  if(Reduce("|", grepl("nfp_tot_carb", colnames(trans_rx)))){
    trans_rx$nfp_tot_carb[trans_rx$nfp_tot_carb < 0] <- NA
    trans_rx$pur_carb <- trans_rx$num_serv*trans_rx$nfp_tot_carb
  }else{trans_rx$pur_carb <- NA}
  if(Reduce("|", grepl("nfp_sugars", colnames(trans_rx)))){
    trans_rx$nfp_sugars[trans_rx$nfp_sugars < 0] <- NA
    trans_rx$pur_sugar <- trans_rx$num_serv*trans_rx$nfp_sugars
  }else{trans_rx$pur_sugar <- NA}
  if(Reduce("|", grepl("nfp_sodium", colnames(trans_rx)))){
    trans_rx$nfp_sodium[trans_rx$nfp_sodium < 0] <- NA
    trans_rx$pur_sodium <- trans_rx$num_serv*trans_rx$nfp_sodium
  }else{trans_rx$pur_sodium <- NA}
  if(Reduce("|", grepl("nfp_sat_fat", colnames(trans_rx)))){
    trans_rx$nfp_sat_fat[trans_rx$nfp_sat_fat < 0] <- NA
    trans_rx$pur_sat_fat <- trans_rx$num_serv*trans_rx$nfp_sat_fat
  }else{trans_rx$pur_sat_fat <- NA}
  if(Reduce("|", grepl("nfp_protein", colnames(trans_rx)))){
    trans_rx$nfp_protein[trans_rx$nfp_protein < 0] <- NA
    trans_rx$pur_protein <- trans_rx$num_serv*trans_rx$nfp_protein
  }else{trans_rx$pur_protein <- NA}
  if(Reduce("|", grepl("nfp_fiber", colnames(trans_rx)))){
    trans_rx$nfp_fiber[trans_rx$nfp_fiber < 0] <- NA
    trans_rx$pur_fiber <- trans_rx$num_serv*trans_rx$nfp_fiber
  }else{trans_rx$fiber <- NA}
  if(Reduce("|", grepl("nfp_cholesterol", colnames(trans_rx)))){
    trans_rx$nfp_cholesterol[trans_rx$nfp_cholesterol < 0] <- NA
    trans_rx$pur_cholesterol <- trans_rx$num_serv*trans_rx$nfp_cholesterol
  }else{trans_rx$pur_cholesterol <- NA}
  
  
  trans_sum <- trans_rx %>% group_by(panelid, timeunit, k, k_zero_timeunit ) %>%
    summarize(sum_fat = sum(pur_fat, na.rm = T),
              sum_carb = sum(pur_carb, na.rm = T),
              sum_sugar = sum(pur_sugar, na.rm = T), 
              sum_sodium = sum(pur_sodium, na.rm = T), 
              sum_sat_fat = sum(pur_sat_fat, na.rm = T),
              sum_protein = sum(pur_protein, na.rm = T), 
              sum_fiber= sum(pur_fiber, na.rm = T), 
              sum_cholesterol = sum(pur_cholesterol, na.rm = T),
              sum_calories = sum(pur_calories, na.rm = T))
  attach(trans_sum)
  trans_sum$fat_per_cal <- sum_fat/sum_calories
  trans_sum$carb_per_cal <- sum_carb/sum_calories
  trans_sum$sugar_per_cal <- sum_sugar/sum_calories
  trans_sum$sodium_per_cal <- sum_sodium/sum_calories
  trans_sum$sat_fat_per_cal <- sum_sat_fat/sum_calories
  trans_sum$protein_per_cal <- sum_protein/sum_calories
  trans_sum$fiber_per_cal <- sum_fiber/sum_calories
  trans_sum$cholesterol_per_cal <- sum_cholesterol/sum_calories
  detach(trans_sum)
  
  trans_sum <- trans_sum[trans_sum$timeunit %in% seq(29, 92), ]
  return(trans_sum)
}
