binary_model <- function(data, calorie_cut_points = c(800,7000), k_range=c(-6,6),nutrient="calories", ref = 0){
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
  data_use$new_id_ind <- data_use$new_id
  data_use$new_id[data_use$new_id %in% control_ids] <- 0
  
  #run the model
  data_use$after.f = factor(data_use$after)
  if(ref != 0){data_use$after.f <- relevel(data_use$after.f, ref = ref)}
  data_use$new_id.f <- factor(data_use$new_id)
  data_use$timeunit.f <- factor(data_use$timeunit)
  model <- paste("plm <- plm(log(", variable, "+ .1) ~ after.f + new_id.f + timeunit.f , data = data_use, index = c('new_id_ind', 'timeunit.f'), model = 'within')", sep = "")
  eval(parse(text = model))
  return(plm)
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
  model <- paste("plm <- plm(log(", variable, "+ .1) ~ k.f + new_id.f + timeunit.f , data = data_use,
                 index = c('new_id', 'timeunit'), model = 'within')", sep = "")
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
  model <- paste("plm <- plm(log(", variable, "+ .1) ~ time.f + new_id.f + timeunit.f , data = data_use,
                 index = c('new_id', 'timeunit'), model = 'within')", sep = "")
  eval(parse(text = model))
  return(lm)
}

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

ftest <- function(plm_mod){
  vcov=vcovHC(plm_mod,type="HC0",cluster="group")
  k_rows <- rownames(summary(plm_mod)$coefficients)[! grepl("timeunit", rownames(summary(plm_mod)$coefficients))]
  k_hyp <- paste(k_rows, "= 0")
  return(linearHypothesis(plm_mod, k_hyp, vcov = vcov, test = "F"))
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

ftest <- function(plm_mod){
  vcov=vcovHC(plm_mod,type="HC0",cluster="group")
  k_rows <- rownames(summary(plm_mod)$coefficients)[! grepl("timeunit", rownames(summary(plm_mod)$coefficients))]
  k_hyp <- paste(k_rows, "= 0")
  return(linearHypothesis(plm_mod, k_hyp, vcov = vcov, test = "F"))
}
a <- Sys.time()
mid_mod <- middle_model(met1)
b <- Sys.time()
a - b
#Linear hypothesis test
#Res.Df Df      F  Pr(>F)  
#1 388835                    
#2 388833  2 2.7941 0.06117 .
a <- Sys.time()
f <- ftest(mid_mod)
b <- Sys.time()
a - b

coeftest(mid_mod, vcov = vcovHC)
#Estimate Std. Error t value Pr(>|t|)    
#time.f0      -0.0827512  0.0361467 -2.2893  0.02206 *  
#time.f1      -0.0419871  0.0311565 -1.3476  0.17778    
mid_mod2 <- middle_model(met2)
ftest(mid_mod2)
#Res.Df Df      F  Pr(>F)  
#1 716851                    
#2 716849  2 2.3369 0.09663 .

coeftest(mid_mod2, vcov = vcovHC)

cal10 <- mean(met1$sum_calories)
cal20 <- mean(met2$sum_calories)
cal11 <- exp(log(cal10 + 0.1) - .08275) - .1
cal21 <- exp(log(cal20 + 0.1) -0.0164066) - .1

dif1 <- cal10-cal11
dif1
dif2 <- cal20-cal21
dif2
