getwd()
setwd("/Users/corey/Desktop/Data+/data")

rx <- read.csv("rx_keep.csv")
trans1 <- read.csv("met1_trans_clean_keep.csv")
trans2 <- read.csv("met2_trans_clean_keep.csv")
brands <- c("Fortamet", "Glucophage", "Glumetza", "Riomet", "Obimet",
            "Dianben", "Diaformin", "Siofor", "Metfogamma", "Janumet",
            "Kazano", "Invokamet", "Xigduo", "Synjardy", "Metaglip" ,
            "Jentaduo" , "Actoplus", "Prandimet", "Avandamet", "Kombiglyze", 
            "Glumetza", "Metformin")

panelids1 <- getNewIDs(brands, rx = rx, new = T, HHSizes = c(1))
ids1 <- panelids1$IDs[[1]]
Brands <- panelids1$Brands[[1]]
panelids2 <- getNewIDs(brands, rx = rx, new = T, HHSizes = c(2))
ids2 <- panelids2$IDs[[1]]

control_1 <- read.csv("control_1_cl_keep.csv")
control_2<- read.csv("control_2_cl_keep.csv")

trans_met_1_cl <- trans1[trans1$new_id %in% ids1,  ]
trans_met_2_cl <- trans2[trans2$new_id %in% ids2,  ]



met1 = combine_control_new(trans_met_1_cl, control_1)
met2 = combine_control_new(trans_met_2_cl, control_2)

model_dir <- "/Users/corey/Desktop/Data+/Models/met_models"
model_dir <- paste(box,"\\random stuff\\",sep="")
trans_met_1_cl_resid <- residualize_new(trans_met_1_cl, model_dir =  model_dir, nutrients = c("carb", "calories"))
trans_met_2_cl_resid <- residualize_new(trans_met_2_cl, hhsize = 2, model_dir = model_dir, nutrients = c("carb", "calories"))
#it says res_mod_2_carb.RData don't exist...


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

# Demonstrating consistent decrease k values -1:3 #####
met_1_matt_cal <- sum_model_new(met1, nutrient = "calories", k_range = c(-6, 6), k_ref = "-6")
met_2_matt_cal <- sum_model_new(met2, nutrient = "calories", k_range = c(-6, 6), k_ref = "-6")

mult <- 1.645
plot_model(met_1_matt_cal, title = "1 person")

plot_model(met_2_matt_cal, title = "2 person")

#Nathaniel start here
trans_met_2_cl_resid
trans_met_1_cl_resid <- read.csv()
k <- 12
met_1_resid_cal <- sum_resid_model_new(trans_met_1_cl_resid, nutrient = "calories", k_range = c(-k, k), k_ref = as.character(min(k_range)))
met_2_resid_cal <- sum_resid_model_new(trans_met_2_cl_resid, nutrient = "calories", k_range = c(-k, k), k_ref = as.character(min(k_range)))
met_1_resid_carb <- sum_resid_model_new(trans_met_1_cl_resid, nutrient = "carb", k_range = c(-k, k), k_ref = as.character(min(k_range)))
met_2_resid_carb <- sum_resid_model_new(trans_met_2_cl_resid, nutrient = "carb", k_range = c(-k, k), k_ref = as.character(min(k_range)))

p1 = plot_model(met_1_resid_cal, k_vals = as.character(-k:k) , title = "Calorie Residualized Estimates\n(one-person households)")
p2 = plot_model(met_2_resid_cal, title = "Calorie Residualized Estimates\n(two-person households)", k_vals = as.character(-12:12))
p3 = plot_model(met_1_resid_carb, k_vals = as.character(-k:k), title = "Carb Residualized Estimates\n(one-person households)")
p4 = plot_model(met_2_resid_carb, k_vals = as.character(-k:k), title = "Carb Residualized Estimates\n(two-person households)")

multiplot(p1, p2, p3, p4, cols=2)


p1 = plot_model(met_1_resid_cal, k_vals = as.character(-k:k) , title = "Calorie Residualized Estimates\n(one-person households)", use_group = T)
p2 = plot_model(met_2_resid_cal, title = "Calorie Residualized Estimates\n(two-person households)", k_vals = as.character(-k:k), use_group = T)
p3 = plot_model(met_1_resid_carb, k_vals = as.character(-k:k), title = "Carb Residualized Estimates\n(one-person households)", use_group = T)
p4 = plot_model(met_2_resid_carb, k_vals = as.character(-k:k), title = "Carb Residualized Estimates\n(two-person households)", use_group = T)

multiplot(p1, p2, p3, p4, cols=2)
##### END #####
# Nathaniel End Here #

met_1_resid_cal <- sum_resid_model_new(trans_met_1_cl_resid, nutrient = "calories", k_range = c(-6, 6), k_ref = "-6")
met1_bin_cal_res <- binary_model_resid(trans_met_1_cl_resid, nutrient = "calories", k_range = c(-6, 6), ref = "0")
met1_multi_cal_res <- multirange_model_resid(trans_met_1_cl_resid, nutrient = "calories", k_range = c(-6, -1, 3, 6))
met1_mid_cal_res <- middle_model_resid(trans_met_1_cl_resid, nutrient = "calories", k_range = c(-1, 3), ref = "-1")
met1_nok_cal_res <- no_k_model_resid(trans_met_1_cl_resid, nutrient = "calories")

AIC(met1_bin_cal_res, met1_multi_cal_res, met1_mid_cal_res, met1_nok_cal_res, met_1_resid_cal)
BIC(met1_bin_cal_res, met1_multi_cal_res, met1_mid_cal_res, met1_nok_cal_res, met_1_resid_cal)

k <- 9
met_1_resid_cal <- sum_resid_model_new(trans_met_1_cl_resid, nutrient = "calories", k_range = c(-k, k), k_ref = as.character(-k))
met1_bin_cal_res <- binary_model_resid(trans_met_1_cl_resid, nutrient = "calories", k_range = c(-k, k), ref = "0")
met1_multi_cal_res <- multirange_model_resid(trans_met_1_cl_resid, nutrient = "calories", k_range = c(-k, -1, 3, k))
met1_mid_cal_res <- middle_model_resid(trans_met_1_cl_resid, nutrient = "calories", k_range = c(-1, 3), ref = "-1")
met1_nok_cal_res <- no_k_model_resid(trans_met_1_cl_resid, nutrient = "calories")

AIC(met1_bin_cal_res, met1_multi_cal_res, met1_mid_cal_res, met1_nok_cal_res, met_1_resid_cal)
BIC(met1_bin_cal_res, met1_multi_cal_res, met1_mid_cal_res, met1_nok_cal_res, met_1_resid_cal)


k <- 12
met_1_resid_cal <- sum_resid_model_new(trans_met_1_cl_resid, nutrient = "calories", k_range = c(-k, k), k_ref = as.character(-k))
met1_bin_cal_res <- binary_model_resid(trans_met_1_cl_resid, nutrient = "calories", k_range = c(-k, k), ref = "0")
met1_multi_cal_res <- multirange_model_resid(trans_met_1_cl_resid, nutrient = "calories", k_range = c(-k, -1, 3, k))
met1_mid_cal_res <- middle_model_resid(trans_met_1_cl_resid, nutrient = "calories", k_range = c(-1, 3), ref = "-1")
met1_nok_cal_res <- no_k_model_resid(trans_met_1_cl_resid, nutrient = "calories")

AIC(met1_bin_cal_res, met1_multi_cal_res, met1_mid_cal_res, met1_nok_cal_res, met_1_resid_cal)
BIC(met1_bin_cal_res, met1_multi_cal_res, met1_mid_cal_res, met1_nok_cal_res, met_1_resid_cal)

met1_c_binary <- binary_model(data=met1)
met1_c_sum <- sum_model_new(data=met1)
met1_c_multi <- multirange_model(data=met1)
AIC(met1_c_binary, met1_c_sum, met1_c_multi, met1_c_middle, met1_c_nok )
met1_c_middle <- middle_model(data=met1)
met1_c_nok <- no_k_model(data=met1)
AIC(met1_c_middle, met1_c_nok)





models$met_1_resid_sugar <- sum_resid_model(trans_met_1_cl_resid, nutrient = "sugar")
models$met_1_resid_fat <- sum_resid_model(trans_met_1_cl_resid, nutrient = "fat")

##### HHSize = 2, residualized ####
models$met_2_resid_carb <- sum_resid_model(trans_met_2_cl_resid)
models$met_2_resid_sugar <- sum_resid_model(trans_met_2_cl_resid, nutrient = "sugar")
models$met_2_resid_fat <- sum_resid_model(trans_met_2_cl_resid, nutrient = "fat")


