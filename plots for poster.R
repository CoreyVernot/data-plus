# Models for poster
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



middle_model <- function(data, nutrient = "calories", calorie_cut_points = c(800, 7000), k_range =c(-1, 3), time_ref = "-1"){
  library(dplyr)
  library(plm)
  library(car)
  library(lmtest)
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
  data_use$new_id_ind <- data_use$new_id
  data_use$new_id[data_use$new_id %in% control_ids] <- 0
  
  #run the model
  data_use$time.f <- factor(data_use$time)
  if(!is.na(time_ref)){data_use$time.f <- relevel(data_use$time.f, ref = time_ref)}
  data_use$new_id.f <- factor(data_use$new_id)
  data_use$timeunit.f <- factor(data_use$timeunit)
  model <- paste("plm <- plm(log(", variable, "+ .1) ~ time.f + new_id.f + timeunit.f , data = data_use, index = c('new_id_ind', 'timeunit.f'), model = 'within')", sep = "")
  eval(parse(text = model))
  return(plm)
}
#1
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
  
  
  #assign a new id to the control group
  control_ids <- data_use$new_id[data_use$k == "control"] %>% unique()
  data_use$new_id_ind <- data_use$new_id
  data_use$new_id[data_use$new_id %in% control_ids] <- 0
  
  data_use$k.f <- factor(data_use$k)   #convert all k's, new_id's, and timeunits to factors
  if(!is.na(k_ref)){data_use$k.f <- relevel(data_use$k.f, ref = k_ref)}
  data_use$new_id.f <- factor(data_use$new_id)
  data_use$timeunit.f <- factor(data_use$timeunit)
  model <- paste("plm <- plm(log(", variable, "+ .1) ~ k.f + new_id.f + timeunit.f , data = data_use, 
                 index = c('new_id_ind', 'timeunit.f'), model = 'within')", sep = "")
  eval(parse(text = model))
  return(plm)
}

mid_mod1_cal <- middle_model(met1)
#2
sum_mod1_cal <- sum_model_new(met1)
#3
mid_mod2_cal <- middle_model(met2)
#4 
sum_mod2_cal <- sum_model_new(met2)


library(car)
library(lmtest)
coef_mid1_cal <- coeftest(mid_mod1_cal, vcov = vcovHC)
coef_mid2_cal <- coeftest(mid_mod2_cal, vcov = vcovHC)
coef_sum1_cal <- coeftest(sum_mod1_cal, vcov = vcovHC)
coef_sum2_cal <- coeftest(sum_mod2_cal, vcov = vcovHC)

mods_for_poster <- list(sum1 = sum_mod1_cal, mid1 = mid_mod1_cal, sum2 = sum_mod2_cal, mid2 = mid_mod2_cal,
                        coef_mid1 = coef_mid1_cal, coef_mid2 = coef_mid2_cal)
save(mods_for_poster, file = "mods_for_poster.RData")
