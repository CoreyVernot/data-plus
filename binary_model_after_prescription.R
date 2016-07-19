binary_model_per <- function(data, calorie_cut_points = c(800, 7000), k_range =c(-12, 12), nutrient = "carb"){
  #trim the sample down to a reasonable range
  variable<- paste(nutrient, "per", "cal",  sep = "_")
  total_cal <- calorie_cut_points*28
  data_use <- data[data$sum_calories <= total_cal[2] & data$sum_calories >= total_cal[1], ]
  do <- paste("data_use <- data_use[data_use$", variable, "< mean(data_use$", variable, ") + 3*sd(data_use$", 
              variable, "), ]", sep = "")
  eval(parse(text = do))
  
  #assign "after" to binary variable
  data_use$after <- as.numeric(data_use$k >= 0)
  #convert k values to bmin and bmax out of k_range
  bmin <- data_use$k < k_range[1]
  bmax <- data_use$k > k_range[2]
  data_use$k[bmin] <- "bmin"
  data_use$k[bmax] <- "bmax"
  #convert "after" to bmin and bmax based on k_range
  data_use$after[bmin] = "bmin"
  data_use$after[bmax] = "bmax"
  
  #run the model
  data_use$after.f <- factor(data_use$after)
  data_use$new_id.f <- factor(data_use$new_id)
  data_use$timeunit.f <- factor(data_use$timeunit)
  model <- paste("lm <- lm(log(", variable, "+ .1) ~ after.f + new_id.f + timeunit.f , data = data_use)", sep = "")
  eval(parse(text = model))
  return(lm)
}

ct1_trans <- read.csv("D:/Duke Grad/2016 Summer/keep/tables/Fake Identifiers/de_identified aggregated data/control_1_cl_keep.csv")
met1_trans <- read.csv("D:/Duke Grad/2016 Summer/keep/tables/Fake Identifiers/de_identified aggregated data/met1_trans_clean_keep.csv")
met1 = combine_control_new(met1_trans, ct1_trans)
met2 = combine_control_new(met2_trans, ct2_trans)

binary_model <- function(data, calorie_cut_points = c(800,7000), k_range=c(-6,6),nutrient="calories", ref = 0){
  library(dplyr)
  #cut the sample to a reasonable range
  variable <- paste("sum", nutrient, sep = "_")
  total_cal <- calorie_cut_points*28
  data_use <- data[data$sum_calories <= total_cal[2] & data$sum_calories >= total_cal[1], ]
  do <- paste("data_use <- data_use[data_use$", variable, "< mean(data_use$", variable, ") + 3*sd(data_use$", 
              variable, "), ]", sep = "")
  eval(parse(text = do))
  
  #assign "after" to binary variable
  data_use$after <- ifelse(data_use$k == "control", "control", as.numeric(data_use$k >= 0))
  #convert k values to bmin and bmax out of k_range
  kvals <- as.character(seq(k_range[1], k_range[2], 1)) #k values to keep as factors in the k_range
  bmin <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) == "-" #negative k values not in kvals are bmin
  bmax <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9") #positive k values not in range are bmax
  data_use$k[bmin] <- "bmin"
  data_use$k[bmax] <- "bmax"
  #convert "after" to bmin and bmax based on k_range
  data_use$after[bmin & data_use$k != "control"] = "bmin"
  data_use$after[bmax & data_use$k != "control"] = "bmax"
  
  #assign a new id to the control group
  control_ids <- data_use$new_id[data_use$k == "control"] %>% unique()
  data_use$new_id[data_use$new_id %in% control_ids] <- 0
  
  #run the model
  data_use$after.f = factor(data_use$after)
  if(ref != 0){data_use$after.f <- relevel(data_use$after.f, ref = ref)}
  data_use$new_id.f <- factor(data_use$new_id)
  data_use$timeunit.f <- factor(data_use$timeunit)
  model <- paste("lm <- lm(log(", variable, "+ .1) ~ after.f + new_id.f + timeunit.f , data = data_use,
                  model = FALSE, qr = FALSE)", sep = "")
  eval(parse(text = model))
  return(lm)
}

sum_model_new <- function(data, nutrient = "calories", calorie_cut_points = c(800, 7000), k_range =c(-6, 6), k_ref = "-1"){
  variable <- paste("sum", nutrient, sep = "_")  # Create name of dependent variable in dataset
  total_cal <- calorie_cut_points*28             # Cut observations with too many or too few calories to be realistic
  data_use <- data[data$sum_calories <= total_cal[2] & data$sum_calories >= total_cal[1], ]
  do <- paste("data_use <- data_use[data_use$", variable, "< mean(data_use$", variable, ") + 3*sd(data_use$", 
              variable, "), ]", sep = "")
  eval(parse(text = do))
  
  #convert k values to bmin and bmax out of k_range
  kvals <- as.character(seq(k_range[1], k_range[2], 1)) #k values to keep as factors in the k_range
  bmin <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) == "-" #negative k values not in kvals are bmin
  bmax <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9") #positive k values not in range are bmax
  data_use$k[bmin] <- "bmin"
  data_use$k[bmax] <- "bmax"
  
  
  control_ids <- data_use$new_id[data_use$k == "control"] %>% unique()
  data_use$new_id[data_use$new_id %in% control_ids] <- 0
  
  data_use$k.f <- factor(data_use$k)   #convert all k's, new_id's, and timeunits to factors
  if(!is.na(k_ref)){data_use$k.f <- relevel(data_use$k.f, ref = k_ref)}
  data_use$new_id.f <- factor(data_use$new_id)
  data_use$timeunit.f <- factor(data_use$timeunit)
  model <- paste("lm <- lm(log(", variable, "+ .1) ~ k.f + new_id.f + timeunit.f , data = data_use,
                  model = FALSE, qr = FALSE)", sep = "")
  eval(parse(text = model))
  return(lm)
}

multirange_model <- function(data, nutrient = "calories", calorie_cut_points = c(800, 7000), k_range =c(-6, -1, 3, 6), time_ref = "-1"){
  #cut the sample to a reasonable range
  variable <- paste("sum", nutrient, sep = "_")
  total_cal <- calorie_cut_points*28
  data_use <- data[data$sum_calories <= total_cal[2] & data$sum_calories >= total_cal[1], ]
  do <- paste("data_use <- data_use[data_use$", variable, "< mean(data_use$", variable, ") + 3*sd(data_use$", 
              variable, "), ]", sep = "")
  eval(parse(text = do))
  
  #convert k values to bmin and bmax out of k_range
  kvals <- as.character(seq(k_range[1], k_range[4], 1)) #k values to keep as factors in the k_range
  bmin <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) == "-" #negative k values not in kvals are bmin
  bmax <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9") #positive k values not in range are bmax
  data_use$k[bmin] <- "bmin"
  data_use$k[bmax] <- "bmax"
  
  #assign variable for 5 time ranges
  data_use$time <- ifelse(data_use$k %in% k_range[2]:k_range[3] & data_use$k != "control", 0, 
                          ifelse(data_use$k %in% (k_range[3]+1):k_range[4] & data_use$k != "control", 1,
                                 ifelse(data_use$k %in% k_range[1]:(k_range[2]-1) & data_use$k != "control", -1,
                                        ifelse(data_use$k == "bmax" & data_use$k != "control", 2, 
                                               ifelse(data_use$k == "bmin" & data_use$k != "control",-2, "control")))))
  
  #assign a new id to the control group
  control_ids <- data_use$new_id[data_use$k == "control"] %>% unique()
  data_use$new_id[data_use$new_id %in% control_ids] <- 0
  
  #run the model
  data_use$time.f <- factor(data_use$time)
  if(!is.na(time_ref)){data_use$time.f <- relevel(data_use$time.f, ref = time_ref)}
  data_use$new_id.f <- factor(data_use$new_id)
  data_use$timeunit.f <- factor(data_use$timeunit)
  model <- paste("lm <- lm(log(", variable, "+ .1) ~ time.f + new_id.f + timeunit.f , data = data_use,
                  model = FALSE, qr = FALSE)", sep = "")
  eval(parse(text = model))
  return(lm)
}

middle_model <- function(data, nutrient = "calories", calorie_cut_points = c(800, 7000), k_range =c(-1, 3), time_ref = "-1"){
  #cut the sample to a reasonable range
  variable <- paste("sum", nutrient, sep = "_")
  total_cal <- calorie_cut_points*28
  data_use <- data[data$sum_calories <= total_cal[2] & data$sum_calories >= total_cal[1], ]
  do <- paste("data_use <- data_use[data_use$", variable, "< mean(data_use$", variable, ") + 3*sd(data_use$", 
              variable, "), ]", sep = "")
  eval(parse(text = do))
  
  #convert k values to bmin and bmax out of k_range
  kvals <- as.character(seq(k_range[1], k_range[2], 1)) #k values to keep as factors in the k_range
  bmin <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) == "-" #negative k values not in kvals are bmin
  bmax <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9") #positive k values not in range are bmax
  data_use$k[bmin] <- "bmin"
  data_use$k[bmax] <- "bmax"
  
  #assign variable for 5 time ranges
  data_use$time <- ifelse(data_use$k %in% k_range[1]:k_range[2] & data_use$k != "control", 0, 
                          ifelse(data_use$k == "bmax" & data_use$k != "control", 1, 
                                 ifelse(data_use$k == "bmin" & data_use$k != "control",-1, "control")))
  
  #assign a new id to the control group
  control_ids <- data_use$new_id[data_use$k == "control"] %>% unique()
  data_use$new_id[data_use$new_id %in% control_ids] <- 0
  
  #run the model
  data_use$time.f <- factor(data_use$time)
  if(!is.na(time_ref)){data_use$time.f <- relevel(data_use$time.f, ref = time_ref)}
  data_use$new_id.f <- factor(data_use$new_id)
  data_use$timeunit.f <- factor(data_use$timeunit)
  model <- paste("lm <- lm(log(", variable, "+ .1) ~ time.f + new_id.f + timeunit.f , data = data_use,
                  model = FALSE, qr = FALSE)", sep = "")
  eval(parse(text = model))
  return(lm)
}

no_k_model <- function(data, nutrient = "calories", calorie_cut_points = c(800, 7000)){
  #cut the sample to a reasonable range
  variable <- paste("sum", nutrient, sep = "_")
  total_cal <- calorie_cut_points*28
  data_use <- data[data$sum_calories <= total_cal[2] & data$sum_calories >= total_cal[1], ]
  do <- paste("data_use <- data_use[data_use$", variable, "< mean(data_use$", variable, ") + 3*sd(data_use$", 
              variable, "), ]", sep = "")
  eval(parse(text = do))
  
  #assign a new id to the control group
  control_ids <- data_use$new_id[data_use$k == "control"] %>% unique()
  data_use$new_id[data_use$new_id %in% control_ids] <- 0
  
  #run the model
  data_use$new_id.f <- factor(data_use$new_id)
  data_use$timeunit.f <- factor(data_use$timeunit)
  model <- paste("lm <- lm(log(", variable, "+ .1) ~ new_id.f + timeunit.f , data = data_use,
                  model = FALSE, qr = FALSE)", sep = "")
  eval(parse(text = model))
  return(lm)
}


sum_resid_model_new <- function(data, nutrient = "carb", calorie_cut_points = c(800, 7000), sdcut = 2,  k_range =c(-6, 6), k_ref = min(k_range), tu_per_id = 10){
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
  
  num_id <- data_use %>% group_by(new_id) %>% summarise(n = n())
  few_id <- num_id$new_id[num_id$n < tu_per_id ]
  data_use <- data_use[! data_use$new_id %in% few_id, ]
  # Cut out all values for a specific new_id that are far above or below that new_id's average during the study period
  do <- paste("sd_s <- data_use %>% group_by(new_id) %>% summarise(upper = mean(", variable, ") + ",
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

binary_model_resid <- function(data, nutrient = "carb", calorie_cut_points = c(800, 7000), sdcut = 2,  k_range =c(-6, 6), ref = "0", tu_per_id = 10){
  library(dplyr)
  data <- data[!is.na(data$k_zero_timeunit), ] # eliminate drug takers that didn't start a new prescription
  
  if("control" %in% unique(data$k)){ cat("Warning: 'control' is level in k - you shouldn't use non-drug-users in residualized model")}
  
  variable <- paste("sum", nutrient, sep = "_")  # Create name of dependent variable in dataset
  total_cal <- calorie_cut_points*28            # Cut observations with too many or too few calories to be realistic
  data_use <- data[data$real_calories <= total_cal[2] & data$real_calories >= total_cal[1], ]
  
  num_id <- data_use %>% group_by(new_id) %>% summarise(n = n())
  few_id <- num_id$new_id[num_id$n < tu_per_id ]
  data_use <- data_use[! data_use$new_id %in% few_id, ]
  # Cut out all values for a specific new_id that are far above or below that new_id's average during the study period
  do <- paste("sd_s <- data_use %>% group_by(new_id) %>% summarise(upper = mean(", variable, ") + ",
              sdcut, "*sd(", variable, "), lower = mean(",variable, ") - ", sdcut, "*sd(",
              variable, "))", sep = "")
  eval(parse(text = do))
  data_use <- merge(data_use, sd_s, by = "new_id", all.x = T)
  do <- paste("keep <- data_use$", variable, " <= data_use$upper & data_use$", variable, 
              " >= data_use$lower", sep = "")
  eval(parse(text = do))
  data_use <- data_use[keep, ]
  
  
  #assign "after" to binary variable
  data_use$after <- ifelse(data_use$k == "control", "control", as.numeric(data_use$k >= 0))
  #convert k values to bmin and bmax out of k_range
  kvals <- as.character(seq(k_range[1], k_range[2], 1)) #k values to keep as factors in the k_range
  bmin <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) == "-" #negative k values not in kvals are bmin
  bmax <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9") #positive k values not in range are bmax
  data_use$k[bmin] <- "bmin"
  data_use$k[bmax] <- "bmax"
  #convert "after" to bmin and bmax based on k_range
  data_use$after[bmin & data_use$k != "control"] = "bmin"
  data_use$after[bmax & data_use$k != "control"] = "bmax"
  
  #run the model
  data_use$after.f = factor(data_use$after)
  if(ref != "0"){data_use$after.f <- relevel(data_use$after.f, ref = ref)}
  data_use$new_id.f <- factor(data_use$new_id)
  data_use$timeunit.f <- factor(data_use$timeunit)
  model <- paste("lm <- lm(", variable, " ~ after.f + new_id.f + timeunit.f , data = data_use)", sep = "")
  eval(parse(text = model))
  return(lm)
}

multirange_model_resid <- function(data, nutrient = "carb", calorie_cut_points = c(800, 7000), sdcut = 2,  k_range =c(-6, -1, 3, 6), ref = "-2", tu_per_id = 10){
  library(dplyr)
  data <- data[!is.na(data$k_zero_timeunit), ] # eliminate drug takers that didn't start a new prescription
  
  if("control" %in% unique(data$k)){ cat("Warning: 'control' is level in k - you shouldn't use non-drug-users in residualized model")}
  
  variable <- paste("sum", nutrient, sep = "_")  # Create name of dependent variable in dataset
  total_cal <- calorie_cut_points*28            # Cut observations with too many or too few calories to be realistic
  data_use <- data[data$real_calories <= total_cal[2] & data$real_calories >= total_cal[1], ]
  
  num_id <- data_use %>% group_by(new_id) %>% summarise(n = n())
  few_id <- num_id$new_id[num_id$n < tu_per_id ]
  data_use <- data_use[! data_use$new_id %in% few_id, ]
  # Cut out all values for a specific new_id that are far above or below that new_id's average during the study period
  do <- paste("sd_s <- data_use %>% group_by(new_id) %>% summarise(upper = mean(", variable, ") + ",
              sdcut, "*sd(", variable, "), lower = mean(",variable, ") - ", sdcut, "*sd(",
              variable, "))", sep = "")
  eval(parse(text = do))
  data_use <- merge(data_use, sd_s, by = "new_id", all.x = T)
  do <- paste("keep <- data_use$", variable, " <= data_use$upper & data_use$", variable, 
              " >= data_use$lower", sep = "")
  eval(parse(text = do))
  data_use <- data_use[keep, ]
  
  
  #convert k values to bmin and bmax out of k_range
  kvals <- as.character(seq(k_range[1], k_range[4], 1)) #k values to keep as factors in the k_range
  bmin <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) == "-" #negative k values not in kvals are bmin
  bmax <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9") #positive k values not in range are bmax
  data_use$k[bmin] <- "bmin"
  data_use$k[bmax] <- "bmax"
  
  #assign variable for 5 time ranges
  data_use$time <- ifelse(data_use$k %in% k_range[2]:k_range[3] & data_use$k != "control", 0, 
                          ifelse(data_use$k %in% (k_range[3]+1):k_range[4] & data_use$k != "control", 1,
                                 ifelse(data_use$k %in% k_range[1]:(k_range[2]-1) & data_use$k != "control", -1,
                                        ifelse(data_use$k == "bmax" & data_use$k != "control", 2, 
                                               ifelse(data_use$k == "bmin" & data_use$k != "control",-2, "control")))))
  
  #run the model
  data_use$time.f = factor(data_use$time)
  if(ref != "0"){data_use$time.f <- relevel(data_use$time.f, ref = ref)}
  data_use$new_id.f <- factor(data_use$new_id)
  data_use$timeunit.f <- factor(data_use$timeunit)
  model <- paste("lm <- lm(", variable, " ~ time.f + new_id.f + timeunit.f , data = data_use)", sep = "")
  eval(parse(text = model))
  return(lm)
}

middle_model_resid <- function(data, nutrient = "carb", calorie_cut_points = c(800, 7000), sdcut = 2,  k_range =c(-6, -1, 3, 6), ref = "-1", tu_per_id = 10){
  library(dplyr)
  data <- data[!is.na(data$k_zero_timeunit), ] # eliminate drug takers that didn't start a new prescription
  
  if("control" %in% unique(data$k)){ cat("Warning: 'control' is level in k - you shouldn't use non-drug-users in residualized model")}
  
  variable <- paste("sum", nutrient, sep = "_")  # Create name of dependent variable in dataset
  total_cal <- calorie_cut_points*28            # Cut observations with too many or too few calories to be realistic
  data_use <- data[data$real_calories <= total_cal[2] & data$real_calories >= total_cal[1], ]
  
  num_id <- data_use %>% group_by(new_id) %>% summarise(n = n())
  few_id <- num_id$new_id[num_id$n < tu_per_id ]
  data_use <- data_use[! data_use$new_id %in% few_id, ]
  # Cut out all values for a specific new_id that are far above or below that new_id's average during the study period
  do <- paste("sd_s <- data_use %>% group_by(new_id) %>% summarise(upper = mean(", variable, ") + ",
              sdcut, "*sd(", variable, "), lower = mean(",variable, ") - ", sdcut, "*sd(",
              variable, "))", sep = "")
  eval(parse(text = do))
  data_use <- merge(data_use, sd_s, by = "new_id", all.x = T)
  do <- paste("keep <- data_use$", variable, " <= data_use$upper & data_use$", variable, 
              " >= data_use$lower", sep = "")
  eval(parse(text = do))
  data_use <- data_use[keep, ]
  
  #convert k values to bmin and bmax out of k_range
  kvals <- as.character(seq(k_range[1], k_range[2], 1)) #k values to keep as factors in the k_range
  bmin <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) == "-" #negative k values not in kvals are bmin
  bmax <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9") #positive k values not in range are bmax
  data_use$k[bmin] <- "bmin"
  data_use$k[bmax] <- "bmax"
  
  #assign variable for 5 time ranges
  data_use$time <- ifelse(data_use$k %in% k_range[1]:k_range[2] & data_use$k != "control", 0, 
                          ifelse(data_use$k == "bmax" & data_use$k != "control", 1, 
                                 ifelse(data_use$k == "bmin" & data_use$k != "control",-1, "control")))
  #run the model
  data_use$time.f = factor(data_use$time)
  if(ref != "-1"){data_use$time.f <- relevel(data_use$time.f, ref = ref)}
  data_use$new_id.f <- factor(data_use$new_id)
  data_use$timeunit.f <- factor(data_use$timeunit)
  model <- paste("lm <- lm(", variable, " ~ time.f + new_id.f + timeunit.f , data = data_use)", sep = "")
  eval(parse(text = model))
  return(lm)
}

no_k_model_resid <- function(data, nutrient = "carb", calorie_cut_points = c(800, 7000), sdcut = 2,  k_range =c(-6, -1, 3, 6), ref = "-1", tu_per_id = 10){
  library(dplyr)
  data <- data[!is.na(data$k_zero_timeunit), ] # eliminate drug takers that didn't start a new prescription
  
  if("control" %in% unique(data$k)){ cat("Warning: 'control' is level in k - you shouldn't use non-drug-users in residualized model")}
  
  variable <- paste("sum", nutrient, sep = "_")  # Create name of dependent variable in dataset
  total_cal <- calorie_cut_points*28            # Cut observations with too many or too few calories to be realistic
  data_use <- data[data$real_calories <= total_cal[2] & data$real_calories >= total_cal[1], ]
  
  num_id <- data_use %>% group_by(new_id) %>% summarise(n = n())
  few_id <- num_id$new_id[num_id$n < tu_per_id ]
  data_use <- data_use[! data_use$new_id %in% few_id, ]
  # Cut out all values for a specific new_id that are far above or below that new_id's average during the study period
  do <- paste("sd_s <- data_use %>% group_by(new_id) %>% summarise(upper = mean(", variable, ") + ",
              sdcut, "*sd(", variable, "), lower = mean(",variable, ") - ", sdcut, "*sd(",
              variable, "))", sep = "")
  eval(parse(text = do))
  data_use <- merge(data_use, sd_s, by = "new_id", all.x = T)
  do <- paste("keep <- data_use$", variable, " <= data_use$upper & data_use$", variable, 
              " >= data_use$lower", sep = "")
  eval(parse(text = do))
  data_use <- data_use[keep, ]
  
  #convert k values to bmin and bmax out of k_range
  kvals <- as.character(seq(k_range[1], k_range[2], 1)) #k values to keep as factors in the k_range
  bmin <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) == "-" #negative k values not in kvals are bmin
  bmax <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9") #positive k values not in range are bmax
  data_use$k[bmin] <- "bmin"
  data_use$k[bmax] <- "bmax"
  
  #assign variable for 5 time ranges
  data_use$time <- ifelse(data_use$k %in% k_range[1]:k_range[2] & data_use$k != "control", 0, 
                          ifelse(data_use$k == "bmax" & data_use$k != "control", 1, 
                                 ifelse(data_use$k == "bmin" & data_use$k != "control",-1, "control")))
  #run the model
  data_use$time.f = factor(data_use$time)
  if(ref != "-1"){data_use$time.f <- relevel(data_use$time.f, ref = ref)}
  data_use$new_id.f <- factor(data_use$new_id)
  data_use$timeunit.f <- factor(data_use$timeunit)
  model <- paste("lm <- lm(", variable, " ~ new_id.f + timeunit.f , data = data_use)", sep = "")
  eval(parse(text = model))
  return(lm)
}


sum_model_new <- function(data, nutrient = "calories", calorie_cut_points = c(800, 7000), k_range =c(-6, 6), k_ref = "-1"){
  variable <- paste("sum", nutrient, sep = "_")  # Create name of dependent variable in dataset
  total_cal <- calorie_cut_points*28             # Cut observations with too many or too few calories to be realistic
  data_use <- data[data$sum_calories <= total_cal[2] & data$sum_calories >= total_cal[1], ]
  do <- paste("data_use <- data_use[data_use$", variable, "< mean(data_use$", variable, ") + 3*sd(data_use$", 
              variable, "), ]", sep = "")
  eval(parse(text = do))
  
  #convert k values to bmin and bmax out of k_range
  kvals <- as.character(seq(k_range[1], k_range[2], 1)) #k values to keep as factors in the k_range
  bmin <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) == "-" #negative k values not in kvals are bmin
  bmax <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9") #positive k values not in range are bmax
  data_use$k[bmin] <- "bmin"
  data_use$k[bmax] <- "bmax"
  
  
  control_ids <- data_use$new_id[data_use$k == "control"] %>% unique()
  data_use$new_id[data_use$new_id %in% control_ids] <- 0
  
  data_use$k.f <- factor(data_use$k)   #convert all k's, new_id's, and timeunits to factors
  if(!is.na(k_ref)){data_use$k.f <- relevel(data_use$k.f, ref = k_ref)}
  data_use$new_id.f <- factor(data_use$new_id)
  data_use$timeunit.f <- factor(data_use$timeunit)
  model <- paste("lm <- lm(log(", variable, "+ .1) ~ k.f + new_id.f + timeunit.f , data = data_use,
                 model = FALSE, qr = FALSE)", sep = "")
  eval(parse(text = model))
  return(lm)
}

multirange_model <- function(data, nutrient = "calories", calorie_cut_points = c(800, 7000), k_range =c(-6, -1, 3, 6), time_ref = "-1"){
  #cut the sample to a reasonable range
  variable <- paste("sum", nutrient, sep = "_")
  total_cal <- calorie_cut_points*28
  data_use <- data[data$sum_calories <= total_cal[2] & data$sum_calories >= total_cal[1], ]
  do <- paste("data_use <- data_use[data_use$", variable, "< mean(data_use$", variable, ") + 3*sd(data_use$", 
              variable, "), ]", sep = "")
  eval(parse(text = do))
  
  #convert k values to bmin and bmax out of k_range
  kvals <- as.character(seq(k_range[1], k_range[4], 1)) #k values to keep as factors in the k_range
  bmin <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) == "-" #negative k values not in kvals are bmin
  bmax <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9") #positive k values not in range are bmax
  data_use$k[bmin] <- "bmin"
  data_use$k[bmax] <- "bmax"
  
  #assign variable for 5 time ranges
  data_use$time <- ifelse(data_use$k %in% k_range[2]:k_range[3] & data_use$k != "control", 0, 
                          ifelse(data_use$k %in% (k_range[3]+1):k_range[4] & data_use$k != "control", 1,
                                 ifelse(data_use$k %in% k_range[1]:(k_range[2]-1) & data_use$k != "control", -1,
                                        ifelse(data_use$k == "bmax" & data_use$k != "control", 2, 
                                               ifelse(data_use$k == "bmin" & data_use$k != "control",-2, "control")))))
  
  #assign a new id to the control group
  control_ids <- data_use$new_id[data_use$k == "control"] %>% unique()
  data_use$new_id[data_use$new_id %in% control_ids] <- 0
  
  #run the model
  data_use$time.f <- factor(data_use$time)
  if(!is.na(time_ref)){data_use$time.f <- relevel(data_use$time.f, ref = time_ref)}
  data_use$new_id.f <- factor(data_use$new_id)
  data_use$timeunit.f <- factor(data_use$timeunit)
  model <- paste("lm <- lm(log(", variable, "+ .1) ~ time.f + new_id.f + timeunit.f , data = data_use,
                 model = FALSE, qr = FALSE)", sep = "")
  eval(parse(text = model))
  return(lm)
}

middle_model <- function(data, nutrient = "calories", calorie_cut_points = c(800, 7000), k_range =c(-1, 3), time_ref = "-1"){
  #cut the sample to a reasonable range
  variable <- paste("sum", nutrient, sep = "_")
  total_cal <- calorie_cut_points*28
  data_use <- data[data$sum_calories <= total_cal[2] & data$sum_calories >= total_cal[1], ]
  do <- paste("data_use <- data_use[data_use$", variable, "< mean(data_use$", variable, ") + 3*sd(data_use$", 
              variable, "), ]", sep = "")
  eval(parse(text = do))
  
  #convert k values to bmin and bmax out of k_range
  kvals <- as.character(seq(k_range[1], k_range[2], 1)) #k values to keep as factors in the k_range
  bmin <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) == "-" #negative k values not in kvals are bmin
  bmax <- (!data_use$k %in% kvals) & substr(data_use$k, 1, 1) %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9") #positive k values not in range are bmax
  data_use$k[bmin] <- "bmin"
  data_use$k[bmax] <- "bmax"
  
  #assign variable for 5 time ranges
  data_use$time <- ifelse(data_use$k %in% k_range[1]:k_range[2] & data_use$k != "control", 0, 
                          ifelse(data_use$k == "bmax" & data_use$k != "control", 1, 
                                 ifelse(data_use$k == "bmin" & data_use$k != "control",-1, "control")))
  
  #assign a new id to the control group
  control_ids <- data_use$new_id[data_use$k == "control"] %>% unique()
  data_use$new_id[data_use$new_id %in% control_ids] <- 0
  
  #run the model
  data_use$time.f <- factor(data_use$time)
  if(!is.na(time_ref)){data_use$time.f <- relevel(data_use$time.f, ref = time_ref)}
  data_use$new_id.f <- factor(data_use$new_id)
  data_use$timeunit.f <- factor(data_use$timeunit)
  model <- paste("lm <- lm(log(", variable, "+ .1) ~ time.f + new_id.f + timeunit.f , data = data_use,
                 model = FALSE, qr = FALSE)", sep = "")
  eval(parse(text = model))
  return(lm)
}

no_k_model <- function(data, nutrient = "calories", calorie_cut_points = c(800, 7000)){
  #cut the sample to a reasonable range
  variable <- paste("sum", nutrient, sep = "_")
  total_cal <- calorie_cut_points*28
  data_use <- data[data$sum_calories <= total_cal[2] & data$sum_calories >= total_cal[1], ]
  do <- paste("data_use <- data_use[data_use$", variable, "< mean(data_use$", variable, ") + 3*sd(data_use$", 
              variable, "), ]", sep = "")
  eval(parse(text = do))
  
  #assign a new id to the control group
  control_ids <- data_use$new_id[data_use$k == "control"] %>% unique()
  data_use$new_id[data_use$new_id %in% control_ids] <- 0
  
  #run the model
  data_use$new_id.f <- factor(data_use$new_id)
  data_use$timeunit.f <- factor(data_use$timeunit)
  model <- paste("lm <- lm(log(", variable, "+ .1) ~ new_id.f + timeunit.f , data = data_use,
                 model = FALSE, qr = FALSE)", sep = "")
  eval(parse(text = model))
  return(lm)
}

#model selection####
a <- Sys.time()
met1_bi <- binary_model(met1)
met1_sum <- sum_model_new(met1)
met1_multi <- multirange_model(met1)
met1_mid <- middle_model(met1)
met1_nok <- no_k_model(met1)
b <- Sys.time()
a - b
AIC(met1_bi, met1_sum, met1_multi, met1_nok)
#            df      AIC
#met1_bi    141 468805.3
#met1_sum   152 468811.9
#met1_multi 142 468800.2
#met1_mid       468800.6
#met1_nok   138 468812.0

AICc(met1_bi, met1_sum, met1_multi, met1_nok)
#            df     AICc
#met1_bi    141 468805.4
#met1_sum   152 468812.0
#met1_multi 142 468800.3
#met1_nok   138 468812.1

BIC(met1_bi, met1_sum, met1_multi, met1_nok)
#            df      BIC
#met1_bi    141 470341.0
#met1_sum   152 470467.3
#met1_multi 142 470346.8
#met1_mid       470325.4
#met1_nok   138 470315.0


anova(met1_nok, met1_mid)
#Analysis of Variance Table
#Model 1: log(sum_calories + 0.1) ~ new_id.f + timeunit.f
#Model 2: log(sum_calories + 0.1) ~ time.f + new_id.f + timeunit.f
#  Res.Df   RSS Df Sum of Sq      F    Pr(>F)    
#1 396652 75667                                  
#2 396650 75664  2    2.9355 7.6942 0.0004555 ***



#met_2
met2_bi <- binary_model(met2)
met2_sum <- sum_model_new(met2)
met2_multi <- multirange_model(met2)
met2_mid <- middle_model(met2)
met2_nok <- no_k_model(met2)

AIC(met2_bi, met2_sum, met2_multi, met2_mid, met2_nok)
#            df     AIC
#met2_bi    351 1011451
#met2_sum   362 1011467
#met2_multi 352 1011451
#met2_mid   350 1011448
#met2_nok   348 1011447

BIC(met2_bi, met2_sum, met2_multi, met2_mid, met2_nok)
#            df     BIC
#met2_bi    351 1015487
#met2_sum   362 1015630
#met2_multi 352 1015499
#met2_mid   350 1015473
#met2_nok   348 1015449

anova(met2_nok, met2_mid, met2_bi)
#Analysis of Variance Table
#Model 1: log(sum_calories + 0.1) ~ new_id.f + timeunit.f
#Model 2: log(sum_calories + 0.1) ~ time.f + new_id.f + timeunit.f
#Model 3: log(sum_calories + 0.1) ~ after.f + new_id.f + timeunit.f
#  Res.Df    RSS Df Sum of Sq      F Pr(>F)
#1 729216 170714                           
#2 729214 170713  2   0.79529 1.6986 0.1829
#3 729213 170713  1  -0.20186  



#model selection_test####
met1_c_binary <- binary_model(data=met1)
met1_c_sum <- sum_model_new(data=met1)
met1_c_multi <- multirange_model(data=met1)
AIC(met1_c_binary, met1_c_sum, met1_c_multi, met1_c_middle, met1_c_nok )
met1_c_middle <- middle_model(data=met1)
met1_c_nok <- no_k_model(data=met1)
AIC(met1_c_middle, met1_c_nok)

met1_binary <- binary_model(data=met1_trans_clean_keep)
met1_sum <- sum_model_new(data=met1_trans_clean_keep, k_ref = "-6")
#met1_sum12 <- sum_model_new(data = met1_trans_clean_keep, k_range = c(-12, 12), k_ref = "-12")
#met1_sum20 <- sum_model_new(data = met1_trans_clean_keep, k_range = c(-20, 20), k_ref = "-20")
met1_multi <- multirange_model(data=met1_trans_clean_keep)
#met1_multi_10123 <-multirange_model(data=met1_trans_clean_keep, k_range = c(-6, -1, 3, 6))
met1_mid <- middle_model(data=met1_trans_clean_keep)
met1_no_k <- no_k_model(data = met1_trans_clean_keep)
AIC(met1_binary)
AIC(met1_sum)
AIC(met1_multi)
AIC(met1_mid)
AIC(met1_no_k)
AIC(met1_binary, met1_sum, met1_multi, met1_multi_10123) #met1_binary seem to be a better one
BIC(met1_binary, met1_sum, met1_multi, met1_multi_10123) #met1_binary seem to be a better one


#data to test on####
#only analyzing the coefficients of Intercept, after.f1, after.fbmax, and after.fbmin
#bb1_trans_clean_keep
lm_sugar_per <- binary_model_per(bb1_trans_clean_keep, nutrient = "sugar")  #significant before the start of prescription
lm_cal <- binary_model(bb1_trans_clean_keep) #significant for all
  
#bb2_trans_clean_keep
lm_sugar_per <- binary_model_per(bb2_trans_clean_keep, nutrient = "sugar")  #significant within window before start of prescription
lm_cal <- binary_model(bb2_trans_clean_keep) #significant within window before k0

#met1_trans_clean_keep
lm_sugar_per <- binary_model_per(met1_trans_clean_keep, nutrient = "sugar") #significant within window
lm_cal <- binary_model(met1_trans_clean_keep) #significant within window

#met2_trans_clean_keep
lm_sugar_per <- binary_model_per(met2_trans_clean_keep, nutrient = "sugar") #significant within window before k0
lm_cal <- binary_model(met2_trans_clean_keep) #significant for all except within window after k0

#SSRI_trans_clean_keep
lm_sugar_per <- binary_model_per(SSRI_trans_clean_keep, nutrient = "sugar") #significant for intercept and fbmax
lm_cal <- binary_model(SSRI_trans_clean_keep) #significant for anytime within the window and time after the window

#SNRI_trans_clean_keep
lm_sugar_per <- binary_model_per(SNRI_trans_clean_keep, nutrient = "sugar") #significant within window before k0
lm_cal <- binary_model(SNRI_trans_clean_keep) #significant anytime within the window and time before the window

#TCA_trans_clean_keep
lm_sugar_per <- binary_model_per(TCA_trans_clean_keep, nutrient = "sugar") #significant before k0
lm_cal <- binary_model(TCA_trans_clean_keep) #significant within window and time after window


#plot model####
plot_bi_model <- function(model, mult = 1.645){
  library(ggplot2)
  coef <- data.frame(summary(model)$coefficients)
  colnames(coef) <- c("estimate", "SE", "t_value", "p")
  plot_table <- coef[ rownames(coef) %in% c("(Intercept)","after.f1","after.fbmin","after.fbmax"), c(1,2)]
  plot_table$time <- rownames(plot_table)
  plot_table$time[plot_table$time =="after.fbmin"] <- -2
  plot_table$time[plot_table$time == "(Intercept)"] <- -1
  plot_table$time[plot_table$time == "after.f1"] <- 1
  plot_table$time[plot_table$time == "after.fbmax"] <- 2
  g <- ggplot(plot_table, aes(x= time , y= estimate)) + 
    geom_errorbar(aes(ymin=estimate-mult*SE, ymax= estimate + mult*SE, colour = "error_bar"), width=.3) +
    geom_line() +
    geom_point() +
    geom_abline(intercept = 0, slope = 0)
  return(g)
}

plot_sum_model <- function(model, k_vals = as.character(-6:6), mult = 1.645){
  library(ggplot2)
  coef <- data.frame(summary(model)$coefficients)
  colnames(coef) <- c("estimate", "SE", "t_value", "p")
  rownames <- paste("k.f", k_vals, sep = "")
  plot_table <- coef[ rownames(coef) %in% rownames, c(1,2)]
  k_ref <- rownames[!rownames %in% rownames(plot_table)]
  add <- data.frame(estimate = 0, SE = NA)
  rownames(add) <- k_ref
  plot_table <- rbind(plot_table, add)
  plot_table$k_level <- rownames(plot_table)
  plot_table$k <- plot_table$k_level %>% gsub("k.f", "", .) %>% as.numeric()
  g <- ggplot(plot_table, aes(x= k , y= estimate)) + 
    geom_errorbar(aes(ymin=estimate-mult*SE, ymax= estimate + mult*SE, colour = "error_bar"), width=.3) +
    geom_line() +
    geom_point() +
    geom_abline(intercept = 0, slope = 0)
  return(g)
}
