getFoodList <- function(){
  foodtype <- list()
  foodtype["Baked Goods and Deserts"] <- "BAK_DES"
  foodtype["Beverages"] <- "BEV"
  foodtype["Breakfast and Dairy"] <- "BFAST_DAIRY"
  foodtype["Baby Food, Liquor, Supplements, and Over-the-Counter Medications"] <- "BFLS"
  foodtype["Candy"] <- "CANDY"
  foodtype["Condiments, Sauces, and Baking Ingredients"] <- "CSB"
  foodtype["Frozen and Refrigerated Meals"] <- "FR_MEAL"
  foodtype["Fruit, Vegetables, and Meat"] <- "FVM"
  foodtype["Random Weight"] <- "RW" #certain types of meat and produce that require Price-Lookup Codes
  foodtype["Snacks"] <- "SNACKS"
  foodtype["Shelf-Stable Meals"] <- "SS_MEAL"
  foodtype[["All"]] <- NULL
  
  for(i in 1:length(foodtype)){
    foodtype[["All"]][i] <- foodtype[[i]]}
  foodtype["All"]
  
  return(foodtype)
}

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

getTrans <- function(ShowFoodOptions = FALSE,
                     ShowNutrientOptions = FALSE,
                     ShowYearOptions = FALSE,
                     AllIDs = FALSE,
                     FoodType = c("BAK_DES","BEV","BFAST_DAIRY","BFLS","CANDY","CSB","FR_MEAL","FVM","RW","SNACKS","SS_MEAL"),
                     Nutrition = c("calories", "cholesterol", "fiber", "protein", "sat_fat", "sodium", "sugars", "tot_carb", "tot_fat"),
                     PanelIDs = NA,
                     Year = c(8,9,10,11,12),
                     becr_server = "SQLServer_BECR",
                     iri_server = "SQLServer_IRI"){
  
  foodlist <- getFoodList()
  nutritionlist <- c("calories", "cholesterol", "fiber", "protein", "sat_fat", "sodium", "sugars", "tot_carb", "tot_fat")
  yearlist <- 8:12
  
  if(ShowFoodOptions == TRUE){
    food <- foodlist[select.list(names(foodlist),multiple=TRUE)]
  }else{
    food <- FoodType
  }
  
  if(ShowNutrientOptions == TRUE){
    nutrition <- select.list(nutritionlist,multiple=TRUE)
  }else{
    nutrition <- Nutrition
  }
  
  if(ShowYearOptions == TRUE){
    year <- select.list(yearlist,multiple=TRUE)
  }else{
    year <- Year
  }
  
  
  year <- as.character(year)
  year[nchar(year) < 2] <- paste("0", year[nchar(year) < 2], sep = "")
  foodyear <- rep(NA, length(year)*length(food))
  for(i in 1:length(food)){
    for(j in 1:length(year)){
      foodyear[j + length(year)*(i-1)] <- paste(food[i], year[j], sep = "")
    }
  }
  
  nut <- getNut(becr_server = becr_server, nutrition = nutrition)
  food <- getFood(server = iri_server, data = foodyear, PanelIDs = PanelIDs)
  trans <- merge(nut, food, by = "upc")
  return(trans)
}

