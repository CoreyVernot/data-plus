#function: input of medical condition, output of a dataframe of brands and prescription times#
getBrands <- function(x, rx){
  #group by medical conditions and then drug brands(see how many patients inside the category)#
  rx_group <- rx %>%
    group_by(Medical_Condition, Rx_Brand)%>%
    arrange(Medical_Condition) %>%
    summarize(Number_Patients = length(unique(panelid)))
  rx_group <- transform(rx_group, Rx_Brand=as.character(Rx_Brand), Medical_Condition=as.character(Medical_Condition))
  #return the brands which match x and the corresponding patient numbers
  rx_return <- rx_group[rx_group$Medical_Condition == x, c("Rx_Brand", "Number_Patients")]
  rx_return <- rx_return[order(rx_return$Number_Patients, decreasing = TRUE), ]
  
  rx_return$Proportion <- prop.table(rx_return$Number_Patients)
  rx_return$Cumulative_Proportion <- cumsum(rx_return$Proportion)
  
  rx_return$Proportion <- round(rx_return$Proportion,4)
  rx_return$Cumulative_Proportion <- round(rx_return$Cumulative_Proportion,4)
  return(rx_return)
}