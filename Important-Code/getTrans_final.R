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

