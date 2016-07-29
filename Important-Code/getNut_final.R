getNut <- function(becr_server = "SQLServer_BECR", nutrition){
  ch <- odbcConnect(becr_server)
  
  select <- "SELECT nfp_serv_size_us, upc,"
  column <- paste("nfp_", nutrition, sep = "")
  from <- "FROM"
  tables <- paste("upc_nutrition_master_", nutrition, sep = "")
  query <- paste(select, column, from, tables)
  
  nut_dfs <- list(rep(NA, length(query)))
  a <- sqlQuery(ch, query[1])
  for(i in 1:length(query)){
    nut_dfs[[i]]<- sqlQuery(ch, query[i])
  }
  
  nutrition_m <- nut_dfs[[1]]
  if(length(query) > 1){
    for(i in 2:length(query)){
      nutrition_m <- merge(nutrition_m, nut_dfs[[i]][c(2,3)], by = "upc")
    }
  }
  return(nutrition_m)
}
