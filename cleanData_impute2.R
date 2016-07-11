# clean data impute
library(dplyr)

rx_keep <- read.csv("D:/Duke Grad/2016 Summer/keep/tables/Fake Identifiers/rx_keep.csv")

brandMet <- c("Fortamet", "Glucophage", "Glumetza", "Riomet", "Obimet",
              "Dianben", "Diaformin", "Siofor", "Metfogamma", "Janumet",
              "Kazano", "Invokamet", "Xigduo", "Synjardy", "Metaglip" ,
              "Jentaduo" , "Actoplus", "Prandimet", "Avandamet", "Kombiglyze", 
              "Glumetza", "Metformin")

metIDs <- getNewIDs(brandMet, rx = rx_keep, Medical_Condition = "Diabetes")

getNewIDs <- function(... , rx, Medical_Condition = NA, new = T){
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