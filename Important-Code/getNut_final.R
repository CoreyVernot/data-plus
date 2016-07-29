getNut <- function(becr_server = "SQLServer_BECR", nutrition){
  ch <- odbcConnect(becr_server)
  
  select <- "SELECT nfp_serv_size_us, upc,"                       #this query selects serving size, UPC, and...
  column <- paste("nfp_", nutrition, sep = "")                    #...nutrient column..
  from <- "FROM"
  tables <- paste("upc_nutrition_master_", nutrition, sep = "")   #...from corresponding nutrient table
  query <- paste(select, column, from, tables)
  
  nut_dfs <- list(rep(NA, length(query)))
  a <- sqlQuery(ch, query[1])
  for(i in 1:length(query)){
    nut_dfs[[i]]<- sqlQuery(ch, query[i])                         #a list of dataframes for different nutrients
  }
  
  nutrition_m <- nut_dfs[[1]]
  if(length(query) > 1){
    for(i in 2:length(query)){
      nutrition_m <- merge(nutrition_m, nut_dfs[[i]][c(2,3)], by = "upc") #merges the serving size and nutrient columns so we do not replicate UPC and serving size
    }
  }
  return(nutrition_m)
}
