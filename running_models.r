sum_model <- function(data, nutrient = "carb", calorie_cut_points = c(800, 7000), sdcut = 2,  k_range =c(-12, 12), k_ref = "-1", tu_per_id = 10){
  data <- data[!is.na(data$k_zero_timeunit), ] # eliminate drug takers that didn't start a new prescription
  
  variable <- paste("sum", nutrient, sep = "_")  # Create name of dependent variable in dataset
  total_cal <- calorie_cut_points*28             # Cut observations with too many or too few calories to be realistic
  data_use <- data[data$sum_calories <= total_cal[2] & data$sum_calories >= total_cal[1], ]
  #do <- paste("data <- data[data$", variable, "< mean(data$", variable, ") + 3*sd(data$", 
  #            variable, "), ]", sep = "")
  #eval(parse(text = do))

  kvals <- as.character(seq(k_range[1], k_range[2], 1)) #k values to keep as factors in the k_range
  bmin <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) == "-" #negative k values not in kvals are bmin
  bmax <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9") #positive k values not in range are bmax
  data_use$k[bmin] <- "bmin"
  data_use$k[bmax] <- "bmax"
  
  num_id <- data_use %>% group_by(panelid) %>% summarize(n = n())
  few_id <- num_id$panelid[num_id$n < tu_per_id ]
  data_use <- data_use[! data_use$panelid %in% few_id, ]
  # Cut out all values for a specific panelid that are far above or below that panelid's average during the study period
  do <- paste("sd_s <- data_use %>% group_by(panelid) %>% summarize(upper = mean(", variable, ") + ",
              sdcut, "*sd(", variable, "), lower = mean(",variable, ") - ", sdcut, "*sd(",
              variable, "))", sep = "")
  eval(parse(text = do))
  data_use <- merge(data_use, sd_s, by = "panelid", all.x = T)
  do <- paste("keep <- data_use$", variable, " <= data_use$upper & data_use$", variable, 
              " >= data_use$lower", sep = "")
  eval(parse(text = do))
  data_use <- data_use[keep, ]
  
  control_ids <- data_use$panelid[data_use$k == "control"] %>% unique()
  data_use$panelid[data_use$panelid %in% control_ids] <- 0
  
  data_use$k.f <- factor(data_use$k)   #convert all k's, panelid's, and timeunits to factors
  if(!is.na(k_ref)){data_use$k.f <- relevel(data_use$k.f, ref = k_ref)}
  data_use$panelid.f <- factor(data_use$panelid)
  data_use$timeunit.f <- factor(data_use$timeunit)
  model <- paste("lm <- lm(log(", variable, "+ .1) ~ k.f + panelid.f + timeunit.f , data = data_use)", sep = "")
  eval(parse(text = model))
  return(lm)
}

sum_model_new <- function(data, nutrient = "carb", calorie_cut_points = c(800, 7000), sdcut = 2,  k_range =c(-12, 12), k_ref = "-1", tu_per_id = 10){
  data <- data[!is.na(data$k_zero_timeunit), ] # eliminate drug takers that didn't start a new prescription
  
  variable <- paste("sum", nutrient, sep = "_")  # Create name of dependent variable in dataset
  total_cal <- calorie_cut_points*28             # Cut observations with too many or too few calories to be realistic
  data_use <- data[data$sum_calories <= total_cal[2] & data$sum_calories >= total_cal[1], ]
  #do <- paste("data <- data[data$", variable, "< mean(data$", variable, ") + 3*sd(data$", 
  #            variable, "), ]", sep = "")
  #eval(parse(text = do))
  
  kvals <- as.character(seq(k_range[1], k_range[2], 1)) #k values to keep as factors in the k_range
  bmin <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) == "-" #negative k values not in kvals are bmin
  bmax <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9") #positive k values not in range are bmax
  data_use$k[bmin] <- "bmin"
  data_use$k[bmax] <- "bmax"
  
  num_id <- data_use %>% group_by(new_id) %>% summarize(n = n())
  few_id <- num_id$new_id[num_id$n < tu_per_id ]
  data_use <- data_use[! data_use$new_id %in% few_id, ]
  # Cut out all values for a specific new_id that are far above or below that new_id's average during the study period
  do <- paste("sd_s <- data_use %>% group_by(new_id) %>% summarize(upper = mean(", variable, ") + ",
              sdcut, "*sd(", variable, "), lower = mean(",variable, ") - ", sdcut, "*sd(",
              variable, "))", sep = "")
  eval(parse(text = do))
  data_use <- merge(data_use, sd_s, by = "new_id", all.x = T)
  do <- paste("keep <- data_use$", variable, " <= data_use$upper & data_use$", variable, 
              " >= data_use$lower", sep = "")
  eval(parse(text = do))
  data_use <- data_use[keep, ]
  
  control_ids <- data_use$new_id[data_use$k == "control"] %>% unique()
  data_use$new_id[data_use$new_id %in% control_ids] <- 0
  
  data_use$k.f <- factor(data_use$k)   #convert all k's, new_id's, and timeunits to factors
  if(!is.na(k_ref)){data_use$k.f <- relevel(data_use$k.f, ref = k_ref)}
  data_use$new_id.f <- factor(data_use$new_id)
  data_use$timeunit.f <- factor(data_use$timeunit)
  model <- paste("lm <- lm(log(", variable, "+ .1) ~ k.f + new_id.f + timeunit.f , data = data_use)", sep = "")
  eval(parse(text = model))
  return(lm)
}

sum_resid_model <- function(data, nutrient = "carb", calorie_cut_points = c(800, 7000), sdcut = 2,  k_range =c(-12, 12), k_ref = "-1", tu_per_id = 10){
  data <- data[!is.na(data$k_zero_timeunit), ] # eliminate drug takers that didn't start a new prescription
  
  if("control" %in% unique(data$k)){ cat("Warning: 'control' is level in k - you shouldn't use non-drug-users in residualized model")}
  
  variable <- paste("sum", nutrient, sep = "_")  # Create name of dependent variable in dataset
  total_cal <- calorie_cut_points*28            # Cut observations with too many or too few calories to be realistic
  data_use <- data[data$real_calories <= total_cal[2] & data$real_calories >= total_cal[1], ]
  #do <- paste("data <- data[data$", variable, "< mean(data$", variable, ") + 3*sd(data$", 
  #            variable, "), ]", sep = "")
  #eval(parse(text = do))
  if(class(data_use$k) %in% c("numeric", "integer")){data_use$k <- as.character(data_use$k)}
  kvals <- as.character(seq(k_range[1], k_range[2], 1)) #k values to keep as factors in the k_range
  bmin <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) == "-" #negative k values not in kvals are bmin
  bmax <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9") #positive k values not in range are bmax
  data_use$k[bmin] <- "bmin"
  data_use$k[bmax] <- "bmax"
  
  num_id <- data_use %>% group_by(panelid) %>% summarize(n = n())
  few_id <- num_id$panelid[num_id$n < tu_per_id ]
  data_use <- data_use[! data_use$panelid %in% few_id, ]
  # Cut out all values for a specific panelid that are far above or below that panelid's average during the study period
  do <- paste("sd_s <- data_use %>% group_by(panelid) %>% summarize(upper = mean(", variable, ") + ",
              sdcut, "*sd(", variable, "), lower = mean(",variable, ") - ", sdcut, "*sd(",
              variable, "))", sep = "")
  eval(parse(text = do))
  data_use <- merge(data_use, sd_s, by = "panelid", all.x = T)
  do <- paste("keep <- data_use$", variable, " <= data_use$upper & data_use$", variable, 
              " >= data_use$lower", sep = "")
  eval(parse(text = do))
  data_use <- data_use[keep, ]
  
  data_use$k.f <- factor(data_use$k)   #convert all k's, panelid's to factors
  if(!is.na(k_ref)){data_use$k.f <- relevel(data_use$k.f, ref = k_ref)}
  data_use$panelid.f <- factor(data_use$panelid)
  model <- paste("lm <- lm( ", variable, " ~ k.f + panelid.f , data = data_use)", sep = "")
  eval(parse(text = model))
  return(lm)
}

sum_resid_model_new <- function(data, nutrient = "carb", calorie_cut_points = c(800, 7000), sdcut = 2,  k_range =c(-12, 12), k_ref = "-1", tu_per_id = 10){
  data <- data[!is.na(data$k_zero_timeunit), ] # eliminate drug takers that didn't start a new prescription
  
  if("control" %in% unique(data$k)){ cat("Warning: 'control' is level in k - you shouldn't use non-drug-users in residualized model")}
  
  variable <- paste("sum", nutrient, sep = "_")  # Create name of dependent variable in dataset
  total_cal <- calorie_cut_points*28            # Cut observations with too many or too few calories to be realistic
  data_use <- data[data$real_calories <= total_cal[2] & data$real_calories >= total_cal[1], ]
  #do <- paste("data <- data[data$", variable, "< mean(data$", variable, ") + 3*sd(data$", 
  #            variable, "), ]", sep = "")
  #eval(parse(text = do))
  if(class(data_use$k) %in% c("numeric", "integer")){data_use$k <- as.character(data_use$k)}
  kvals <- as.character(seq(k_range[1], k_range[2], 1)) #k values to keep as factors in the k_range
  bmin <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) == "-" #negative k values not in kvals are bmin
  bmax <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9") #positive k values not in range are bmax
  data_use$k[bmin] <- "bmin"
  data_use$k[bmax] <- "bmax"
  
  num_id <- data_use %>% group_by(new_id) %>% summarize(n = n())
  few_id <- num_id$new_id[num_id$n < tu_per_id ]
  data_use <- data_use[! data_use$new_id %in% few_id, ]
  # Cut out all values for a specific new_id that are far above or below that new_id's average during the study period
  do <- paste("sd_s <- data_use %>% group_by(new_id) %>% summarize(upper = mean(", variable, ") + ",
              sdcut, "*sd(", variable, "), lower = mean(",variable, ") - ", sdcut, "*sd(",
              variable, "))", sep = "")
  eval(parse(text = do))
  data_use <- merge(data_use, sd_s, by = "new_id", all.x = T)
  do <- paste("keep <- data_use$", variable, " <= data_use$upper & data_use$", variable, 
              " >= data_use$lower", sep = "")
  eval(parse(text = do))
  data_use <- data_use[keep, ]
  
  data_use$k.f <- factor(data_use$k)   #convert all k's, new_id's to factors
  if(!is.na(k_ref)){data_use$k.f <- relevel(data_use$k.f, ref = k_ref)}
  data_use$new_id.f <- factor(data_use$new_id)
  model <- paste("lm <- lm( ", variable, " ~ k.f + new_id.f , data = data_use)", sep = "")
  eval(parse(text = model))
  return(lm)
}
taking_model <-  function(data, nutrient = "carb", calorie_cut_points = c(800, 7000), sdcut = 2,  k_range =c(-12, 12), k_ref = "-1", tu_per_id = 10){
  data <- data[!is.na(data$k_zero_timeunit), ] # eliminate drug takers that didn't start a new prescription
  
  if("control" %in% unique(data$k)){ cat("Warning: 'control' is level in k - you shouldn't use non-drug-users in residualized model")}
  
  variable <- paste("sum", nutrient, sep = "_")  # Create name of dependent variable in dataset
  total_cal <- calorie_cut_points*28            # Cut observations with too many or too few calories to be realistic
  data_use <- data[data$real_calories <= total_cal[2] & data$real_calories >= total_cal[1], ]
  #do <- paste("data <- data[data$", variable, "< mean(data$", variable, ") + 3*sd(data$", 
  #            variable, "), ]", sep = "")
  #eval(parse(text = do))
  
  kvals <- as.character(seq(k_range[1], k_range[2], 1)) #k values to keep as factors in the k_range
  bmin <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) == "-" #negative k values not in kvals are bmin
  bmax <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9") #positive k values not in range are bmax
  data_use$k[bmin] <- "bmin"
  data_use$k[bmax] <- "bmax"
  
  num_id <- data_use %>% group_by(panelid) %>% summarize(n = n())
  few_id <- num_id$panelid[num_id$n < tu_per_id ]
  data_use <- data_use[! data_use$panelid %in% few_id, ]
  # Cut out all values for a specific panelid that are far above or below that panelid's average during the study period
  do <- paste("sd_s <- data_use %>% group_by(panelid) %>% summarize(upper = mean(", variable, ") + ",
              sdcut, "*sd(", variable, "), lower = mean(",variable, ") - ", sdcut, "*sd(",
              variable, "))", sep = "")
  eval(parse(text = do))
  data_use <- merge(data_use, sd_s, by = "panelid", all.x = T)
  do <- paste("keep <- data_use$", variable, " <= data_use$upper & data_use$", variable, 
              " >= data_use$lower", sep = "")
  eval(parse(text = do))
  data_use <- data_use[keep, ]
  
  data_use$k.f <- factor(data_use$k)   #convert all k's, panelid's to factors
  if(!is.na(k_ref)){data_use$k.f <- relevel(data_use$k.f, ref = k_ref)}
  data_use$panelid.f <- factor(data_use$panelid)
  model <- paste("lm <- lm( ", variable, " ~ k.f + panelid.f , data = data_use)", sep = "")
  eval(parse(text = model))
  return(lm)
}


per_model <- function(data, nutrient = "carb", calorie_cut_points = c(800, 7000), sdcut = 2,  k_range =c(-12, 12), k_ref = NA){
  variable <- paste(nutrient, "per", "cal", sep = "_")
  total_cal <- calorie_cut_points*28
  data_use <- data[data$sum_calories <= total_cal[2] & data$sum_calories >= total_cal[1], ]
  do <- paste("data <- data[data$", variable, "< mean(data$", variable, ") + 2*sd(data$", 
              variable, "), ]", sep = "")
  eval(parse(text = do))
  bmin <- data_use$k < k_range[1]
  bmax <- data_use$k > (k_range[2])
  data_use$k[bmin] <- "bmin"
  data_use$k[bmax] <- "bmax"
  do <- paste("sd_s <- data_use %>% group_by(panelid) %>% summarize(upper = mean(", variable, ") + ", sdcut, "*sd(", variable, 
              "), lower = mean(",variable, ") - ", sdcut, "*sd(", variable, "))", sep = "")
  eval(parse(text = do))
  data_use <- merge(data_use, sd_s, by = "panelid", all.x = T)
  do <- paste("keep <- data_use$", variable, " <= data_use$upper & data_use$", variable, 
              " >= data_use$lower", sep = "")
  eval(parse(text = do))
  data_use$k.f <- factor(data_use$k)
  if(!is.na(k_ref)){data_use$k.f <- relevel(data_use$k.f, ref = k_ref)}
  data_use$panelid.f <- factor(data_use$panelid)
  data_use$timeunit.f <- factor(data_use$timeunit)
  variable<- paste("sum", nutrient, sep = "_")
  model <- paste("lm <- lm(log(", variable, "+ .1) ~ k.f + panelid.f + timeunit.f , data = data_use)", sep = "")
  eval(parse(text = model))
  return(lm)
}


sam_model_NoID <- function(data, calorie_cut_points = c(-100000, 100000000), k_range =c(-12, 12), nutrient = "cholesterol"){
  total_cal <- calorie_cut_points*28
  data_use <- data[data$sum_calories <= total_cal[2] & data$sum_calories >= total_cal[1], ]
  bmin <- data_use$k < k_range[1]
  bmax <- data_use$k > k_range[2]
  data_use$k[bmin] <- "bmin"
  data_use$k[bmax] <- "bmax"
  data_use$k.f <- factor(data_use$k)
  data_use$panelid.f <- factor(data_use$panelid)
  data_use$timeunit.f <- factor(data_use$timeunit)
  variable<- paste("sum", nutrient, sep = "_")
  model <- paste("lm <- lm(log(", variable, "+ .1) ~ k.f + timeunit.f , data = data_use)", sep = "")
  eval(parse(text = model))
  return(lm)
}

sam_model_tu <- function(data, calorie_cut_points = c(800, 7000), k_range =c(-6, 6), nutrient = "carb"){
  total_cal <- calorie_cut_points*28
  data_use <- data[data$sum_calories <= total_cal[2] & data$sum_calories >= total_cal[1], ]
  bmin <- data_use$k < k_range[1]
  bmax <- data_use$k > (k_range[2])
  data_use$k[bmin] <- "bmin"
  data_use$k[bmax] <- "bmax"
  data_use$k.f <- factor(data_use$k)
  data_use$panelid.f <- factor(data_use$panelid)
  data_use$timeunit.f <- factor(data_use$timeunit)
  variable<- paste("sum", nutrient, sep = "_")
  model <- paste("lm <- lm(log(", variable, "+ .1) ~ timeunit.f , data = data_use)", sep = "")
  eval(parse(text = model))
  return(lm)
}
#leaving out k.f
sam_model_r<- function(data, calorie_cut_points = c(800, 7000), k_range =c(-6, 6), nutrient = "calories"){
  total_cal <- calorie_cut_points*28
  data_use <- data[data$sum_calories <= total_cal[2] & data$sum_calories >= total_cal[1], ]
  data_use <- data_use[data_use$k <= k_range[2] & data_use$k >= k_range[1], ]
  data_use$k.f <- factor(data_use$k)
  data_use$panelid.f <- factor(data_use$panelid)
  data_use$timeunit.f <- factor(data_use$timeunit)
  variable<- paste("sum", nutrient, sep = "_")
  model <- paste("lm <- lm(log(", variable, "+ .1) ~ panelid.f + timeunit.f , data = data_use)", sep = "")
  eval(parse(text = model))
  return(lm)
}

binary_model <- function(data, calorie_cut_points = c(800, 7000), k_range =c(-12, 12), nutrient = "carb"){
  variable<- paste(nutrient, "per", "cal",  sep = "_")
  total_cal <- calorie_cut_points*28
  data_use <- data[data$sum_calories <= total_cal[2] & data$sum_calories >= total_cal[1], ]
  data_use$after <- as.numeric(data_use$k >= 0)
  bmin <- data_use$k < k_range[1]
  bmax <- data_use$k > (k_range[2])
  data_use$k[bmin] <- "bmin"
  data_use$k[bmax] <- "bmax"
  data_use$after[bmin] = "bmin"
  data_use$after[bmax] = "bmax"
  data_use$after.f = factor(data_use$after)
  data_use$k.f <- factor(data_use$k)
  data_use$panelid.f <- factor(data_use$panelid)
  data_use$timeunit.f <- factor(data_use$timeunit)
  model <- paste("lm <- lm(log(", variable, "+ .1) ~ after.f + panelid.f + timeunit.f , data = data_use)", sep = "")
  eval(parse(text = model))
  return(lm)
}

