findJ0 <- function(rx, panelids, Medical_Condition, use_sample = F){
  IRI_week <- read.csv("IRI_week.csv")
  
  #subsetting rx
  rx2 <- rx[rx$panelid %in% panelids, ]
  rx2 <- rx2[rx2$Medical_Condition %in% Medical_Condition, ]
  if(!use_sample){
    rx2 <- rx2[rx2$New_Refill_Sample != "Sample", ]
    rx2 <- rx2[rx2$Rx_Brand != "OTHER PRESCRIPTION/SAMPLE", ]
  }
  
  j_table <- rx2 %>% group_by(panelid) %>% summarize(IRI.Week = min(Week))
  j_table <- merge(j_table, IRI_week, by = "IRI.Week", all.x = T)
  #J_zero is the first timeunit where the patient recieved ANY prescription brand for a specified medical condition
  
  colnames(j_table)[colnames(j_table) == "timeunit"] <- "j_zero_timeunit"

  #returning the two columns of interest
  j_table <- j_table[, c("panelid", "j_zero_timeunit")]
  return(j_table)
}
