ids_control <- unique(Demo_MP$panelid[Demo_MP$hhsize ==1 & Demo_MP$Control == 1 ])
ids_control <- ids_control[!is.na(ids_control)]
ids_control <- ids_control %>% as.character() %>% as.numeric()
dem_control <- demo[demo$panelid %in% ids_control,]
rx_control <- rx[rx$panelid %in% ids_control, ]
mean(rx_control$Medical_Condition == "Diabetes")
trans_control <- getTrans(PanelIDs = ids_control)

# Functions ####
#creating timeunit to date key
timeunitKey <- function(){
  IRI_timeunit <- read.csv("IRI_timeunit.csv")
  IRI_timeunit$start <- IRI_timeunit$start %>% as.character() %>% as.Date(format = "%Y-%m-%d")
  IRI_timeunit$end <- IRI_timeunit$end %>% as.character() %>% as.Date(format = "%Y-%m-%d")
  
  date <- seq.Date(IRI_timeunit$start[1], IRI_timeunit$end[1], by = 1)
  timeunit1 <- rep(IRI_timeunit$timeunit[1], length(date))
  for(i in 2:nrow(IRI_timeunit)){
    a <- seq.Date(IRI_timeunit$start[i], IRI_timeunit$end[i], by = 1)
    date <- c(date, a)
    timeunit1 <- c(timeunit1, rep(IRI_timeunit$timeunit[i], length(a)))
  }
  key_timeunit <- data.frame(timeunit = timeunit1, date = date)
  return(key_timeunit)
}


cleanControlData <- function(trans){
  key_timeunit <- timeunitKey()
  panelids <- unique(trans$panelid)
  trans <- trans[!(is.na(trans$totoz) & is.na(trans$floz)), ]
  trans$purdate2 <- as.Date(trans$purdate2, format = "%m-%d-%y")
  trans_rx <- merge(trans, key_timeunit, by.x = "purdate2", by.y = "date", all.x = T)
  trans_rx <- trans_rx[!(is.na(trans_rx$nfp_serv_size_us)),]
  tot_amount <- rowSums(trans_rx[, c("totfloz", "totoz")], na.rm = T)
  trans_rx$nfp_serv_size_us[trans_rx$nfp_serv_size_us < .001] <- NA
  trans_rx$num_serv <- tot_amount / trans_rx$nfp_serv_size_us
  
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
  if(Reduce("|", grepl("nfp_calories", colnames(trans_rx)))){
    trans_rx$nfp_calories[trans_rx$nfp_calories < 0] <- NA
    trans_rx$pur_calories <- trans_rx$num_serv*trans_rx$nfp_calories
  }else{trans_rx$pur_calories <- NA}
  
  
  trans_sum <- trans_rx %>% group_by(panelid, timeunit) %>%
    summarize(sum_fat = sum(pur_fat, na.rm = T),
              sum_carb = sum(pur_carb, na.rm = T),
              sum_sugar = sum(pur_sugar, na.rm = T), 
              sum_sodium = sum(pur_sodium, na.rm = T), 
              sum_sat_fat = sum(pur_sat_fat, na.rm = T),
              sum_protein = sum(pur_protein, na.rm = T), 
              sum_fiber= sum(pur_fiber, na.rm = T), 
              sum_cholesterol = sum(pur_cholesterol, na.rm = T),
              sum_calories = sum(pur_calories, na.rm = T))
  return(trans_sum)
}
#END####

trans_control_cl <- cleanControlData(trans_control)
write.csv(trans_control_cl, "trans_control_cl.csv")
brands <- c("Fortamet", "Glucophage", "Glumetza", "Riomet", "Obimet",
                      "Dianben", "Diaformin", "Siofor", "Metfogamma", "Janumet",
                      "Kazano", "Invokamet", "Xigduo", "Synjardy", "Metaglip" ,
                      "Jentaduo" , "Actoplus", "Prandimet", "Avandamet", "Kombiglyze", 
                      "Glumetza", "Metformin")
ids <- getPanelIDs(brands, rx = rx, demo = demo)
Brands <- ids$Brands[[1]]
trans <- read.csv("MetforminTrans.csv")
trans_cl <- cleanData(trans, rx = rx, brands = Brands )
length(unique(trans_cl$panelid))

density_scatter_control <- function(trans_cl, trans_control_cl, nutrient, xlim = NA, ylim = NA){
  variable <- paste("sum", nutrient, sep = "_")
  trans_cl$control <- 0
  trans_control_cl$control <- 1
  make_tsum_t <- paste("tsum_t <- trans_cl %>% group_by(timeunit, control) %>% summarise(nutrient = mean(",
                       variable, "))", sep = "")
  make_tsum_c <- paste("tsum_c <- trans_control_cl %>% group_by(timeunit, control) %>% summarise(nutrient = mean(",
                       variable, "))", sep = "")
  eval(parse(text = make_tsum_t))
  eval(parse(text = make_tsum_c))
  tsum <- rbind(tsum_t, tsum_c)
  tsum$control <- factor(tsum$control)
  g <- ggplot(data  = tsum, aes(x = timeunit, y = nutrient, colour = control))
  if(is.na(xlim)){xlim <- c(30, 90)}
  if(is.na(ylim)){ylim <- c(min(tsum$nutrient- 10),max(tsum$nutrient) + 10 )}
  scatter <- g + geom_point() + xlim(xlim[1], xlim[2]) + ylim(ylim[1], ylim[2]) + geom_line(aes(group = control))
  k0 <- trans_cl %>% group_by(panelid, k_zero_timeunit) %>% summarize()
  density <- ggplot(k0, aes(k_zero_timeunit)) + geom_density(alpha = .5) + xlim(xlim[1], xlim[2])
  grid.arrange(scatter, density, ncol = 1, nrow = 2, heights = c(4,2))
}

density_scatter_control(trans_cl = trans_cl, trans_control_cl = trans_control_cl, nutrient = "carb", ylim = c(5800, 9000))
density_scatter_control(trans_cl = trans_cl, trans_control_cl = trans_control_cl, nutrient = "calories", ylim = c( 45000, 70000))
density_scatter_control(trans_cl = trans_cl, trans_control_cl = trans_control_cl, nutrient = "sugar", ylim = c(3000, 5000))


density_scatter_control(trans_cl = trans_cl,trans_control_cl = trans_control_cl)

length(unique(rx$panelid[rx$Rx_Brand %in% Brands & rx$New_Refill_Sample == "New"]))
length(unique(rx$panelid[rx$Rx_Brand %in% Brands])) #& rx$New_Refill_Sample == "New"])



par(mfrow = c(3,3))
hist(trans_cl$sum_fat)
hist(trans_cl$sum_carb)
hist(trans_cl$sum_sugar)
hist(trans_cl$sum_fiber)
hist(trans_cl$sum_sodium)
hist(trans_cl$sum_sat_fat)
hist(trans_cl$sum_protein)
hist(trans_cl$sum_calories)
hist(trans_cl$sum_cholesterol)
par(mfrow = c(1,2))
hist(trans_cl$sum_sugar, main = "Sugar")
hist(log(trans_cl$sum_sugar), main = "log(Sugar)")
