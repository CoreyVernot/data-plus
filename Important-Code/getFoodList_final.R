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
