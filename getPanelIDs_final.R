getPanelIDs <- function(..., rx, demo, static_panel = T, HHSizes=1, Medical_Condition = NA, new = T, only_first_rx = F, use_sample = F){
  allBrands <- list(...)
  #the drugs are vectors of brands that contain a certain drug of interest
  panelIDs <- list()
  
  if(static_panel){
    demo <- demo[demo$projection61k_2008 != 0 & demo$projection61k_2009 != 0 & demo$projection61k_2010 != 0 & demo$projection61k_2011 != 0 & demo$projection61k_2012 != 0, ]
    #subsetting the ids by their reliability, which is determined in the demograpic dataset.
  }
  demoids <- demo$panelid[demo$hhsize %in% HHSizes]
  #subsetting the ids by household size.
  
  #cleaning brand names to match despite capitalization or punctuation
  rx1 <- rx
  if(!is.na(Medical_Condition)){rx1 <- rx1[rx1$Medical_Condition %in% Medical_Condition, ]}
  #eliminate all whitespace and non-character/number things
  rx1$Rx_Brand_Print <- rx1$Rx_Brand
  rx1$Rx_Brand <- tolower(rx1$Rx_Brand)
  rx1$Rx_Brand <- gsub("[^[:alnum:]]", "", rx1$Rx_Brand)
  allBrands <- lapply( allBrands, function(x){ tolower(gsub("[^[:alnum:]]", "", x))})
  
  for(i in 1:length(allBrands)){
    #the loop that creates the return value
    brands <- allBrands[[i]]
    expr <- paste("(", paste(brands, collapse = "|"), ")", sep = "")
    index <- grep(expr, rx1$Rx_Brand)
    Brands <- unique(rx1$Rx_Brand_Print[index]) #Brands is the original formatting/capitalization of the matched brands in the prescription dataset
    rxids <- unique(rx1$panelid[index])
    ids <- rxids[rxids %in% demoids]
    
    if(only_first_rx){
      #subset IDs by who began taking their first drug of specified brands for specified medical conditions within the rx data
      k_table <- findK0(rx = rx, panelids = ids, brands = Brands)
      j_table <- findJ0(rx = rx, panelids = ids, Medical_Condition = Medical_Condition, use_sample = use_sample)
      J_K_table <- merge(j_table, k_table, by = "panelid", all.y = T)
      J_K_table$j_zero_timeunit[is.na(J_K_table$j_zero_timeunit)] <- -100
      use <- J_K_table$k_zero_timeunit <= J_K_table$j_zero_timeunit
      ids <- J_K_table$panelid[use]
    }
    
    if(new){
      #subset IDs by who started a new prescription of a specified drug type within the rx data
      rx_new <- rx[rx$New_Refill_Sample == "New", ]
      rx_new <- rx_new[rx_new$Rx_Brand %in% Brands, ]
      ids_new <- unique(rx_new$panelid)
      ids <- ids[ids %in% ids_new]
    }
    
    panelIDs$IDs[[i]] <- ids
    panelIDs$Brands[[i]] <- unique(as.vector((rx1$Rx_Brand_Print[index])))
    #the return value
    
    cat("Drug brands in database matching drug set ",i,":", "\n",sep="")
    cat(unique(as.vector((rx1$Rx_Brand_Print[index]))),"\n",sep="    ")
    cat("n =",length(ids),"\n","\n")
  
  }
  return(panelIDs)
}
