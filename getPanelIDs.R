getRx <- function(years = c(10,11,12), server = "SQLServer_IRI" ){
  ch <- odbcConnect(server)
  rx <- data.frame()
  allRx <- paste("Rx",years,sep="")
  for(i in 1:length(years)){
    rx_i <- sqlFetch(ch,allRx[i])
    rx <- rbind(rx, rx_i)
  }
  return(rx)
} 

getDemo <- function(server = "SQLServer_IRI"){
  ch <- odbcConnect(server)
  demo <- sqlFetch(ch, "DEMO")
  return(demo)
}

findK0 <- function(rx, panelids, brands){
  IRI_week <- read.csv("IRI_week.csv")
  rx2 <- rx[rx$panelid %in% panelids, ]
  rx2 <- rx2[rx2$Rx_Brand %in% brands, ]
  rx2 <- rx2[rx2$New_Refill_Sample == "New",]
  k_table <- rx2 %>% group_by(panelid) %>% summarize(IRI.Week = min(Week))
  k_table <- merge(k_table, IRI_week, by = "IRI.Week", all.x = T)
  colnames(k_table)[colnames(k_table) == "timeunit"] <- "k_zero_timeunit"
  k_table <- k_table[, c("panelid", "k_zero_timeunit")]
  return(k_table)
}

findJ0 <- function(rx, panelids, Medical_Condition, use_sample = F){ #J_zero is the first timeunit where the patient recieved a perscription for diabetes
  IRI_week <- read.csv("IRI_week.csv")
  rx2 <- rx[rx$panelid %in% panelids, ]
  rx2 <- rx2[rx2$Medical_Condition %in% Medical_Condition, ]
  if(!use_sample){
    rx2 <- rx2[rx2$New_Refill_Sample != "Sample", ]
    rx2 <- rx2[rx2$Rx_Brand != "OTHER PRESCRIPTION/SAMPLE", ]
  }
  j_table <- rx2 %>% group_by(panelid) %>% summarize(IRI.Week = min(Week))
  j_table <- merge(j_table, IRI_week, by = "IRI.Week", all.x = T)
  colnames(j_table)[colnames(j_table) == "timeunit"] <- "j_zero_timeunit"
  j_table <- j_table[, c("panelid", "j_zero_timeunit")]
  return(j_table)
}

getPanelIDs <- function(..., rx, demo, static_panel = T, HHSizes=1, Medical_Condition = NA, new = T, only_first_rx = F, use_sample = F){
  allBrands <- list(...)
  #the drugs are vectors of brands that contain a certain drug of interest
  #ID <- rx$panelid[rx$Rx_Brand %in% Drug1,]  #we'll want to return ID's (panel or HH?) of ppl who took this drug and are in a certain household size
  panelIDs <- list()
  
  if(static_panel){
    demo <- demo[demo$projection61k_2008 != 0 & demo$projection61k_2009 != 0 & demo$projection61k_2010 != 0 & demo$projection61k_2011 != 0 & demo$projection61k_2012 != 0, ]
  }
  demoids <- demo$panelid[demo$hhsize %in% HHSizes]
  
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
    rxids <- unique(rx1$panelid[index])
    ids <- rxids[rxids %in% demoids]
    
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
      ids_new <- unique(rx_new$panelid)
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


