getFood <- function(server = "SQLServer_IRI", data, PanelIDs){
  ch <- odbcConnect(server)
  selection <- "SELECT panelid, purdate, purdate2, purmo, puryr, upc, category, oz, totoz, floz, totfloz FROM "
  where <- " WHERE panelid IN ("
  #panelids is a numeric vector from getTrans function
  ids <-  PanelIDs %>% as.character() %>% paste(collapse = " , " )
  
  query <- paste(selection, data, where, ids,")" ,sep= "")
  
  food <- list(rep(NA, length(query)))
  for(i in 1: length(query)){
    food[[i]] <- sqlQuery(ch, query[i])
  }
  
  #merge dataset together
  food_m <- food[[1]]
  if(length(query > 1)){
    for(i in 2:length(query)){
      food_m <- rbind(food_m, food[[i]])
    }
  }
  
  return(food_m)
}
