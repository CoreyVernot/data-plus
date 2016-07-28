findK0 <- function(rx, panelids, brands){
  IRI_week <- read.csv("IRI_week.csv") #table that converts weeks to timeunits (periods of 4 weeks)
  
  #subsetting rx
  rx2 <- rx[rx$panelid %in% panelids, ]
  rx2 <- rx2[rx2$Rx_Brand %in% brands, ]
  rx2 <- rx2[rx2$New_Refill_Sample == "New",]
  
  k_table <- rx2 %>% group_by(panelid) %>% summarize(IRI.Week = min(Week)) 
  k_table <- merge(k_table, IRI_week, by = "IRI.Week", all.x = T)
  #the first timeunit in which an individual appears with a new prescription in the brands character vector
  
  colnames(k_table)[colnames(k_table) == "timeunit"] <- "k_zero_timeunit"
  
  #returning the two columns of interest
  k_table <- k_table[, c("panelid", "k_zero_timeunit")]
  return(k_table)
}
