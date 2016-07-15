
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

met1 = combine_control_new(met1_trans_clean_keep, control_1_cl_keep)
met2 = combine_control_new(met2_trans_clean_keep, control_2_cl_keep)
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
  model <- paste("lm <- lm(log(", variable, "+ .1) ~ after.f + new_id.f + timeunit.f , data = data_use)", sep = "")
  eval(parse(text = model))
  return(lm)
}

sum_model_new <- function(data, nutrient = "calories", calorie_cut_points = c(800, 7000), sdcut = 2,  k_range =c(-6, 6), k_ref = "-1", tu_per_id = 10){
  variable <- paste("sum", nutrient, sep = "_")  # Create name of dependent variable in dataset
  total_cal <- calorie_cut_points*28             # Cut observations with too many or too few calories to be realistic
  data_use <- data[data$sum_calories <= total_cal[2] & data$sum_calories >= total_cal[1], ]
  do <- paste("data <- data[data$", variable, "< mean(data$", variable, ") + 3*sd(data$", 
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
  model <- paste("lm <- lm(log(", variable, "+ .1) ~ k.f + new_id.f + timeunit.f , data = data_use)", sep = "")
  eval(parse(text = model))
  return(lm)
}

multirange_model <- function(data, nutrient = "calories", calorie_cut_points = c(800, 7000), sdcut = 2,  
                        k_range =c(-6, -2, 2, 6), time_ref = "-2"){
  #cut the sample to a reasonable range
  variable <- paste("sum", nutrient, sep = "_")
  total_cal <- calorie_cut_points*28
  data_use <- data[data$sum_calories <= total_cal[2] & data$sum_calories >= total_cal[1], ]
  do <- paste("data <- data[data$", variable, "< mean(data$", variable, ") + 3*sd(data$", 
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
  model <- paste("lm <- lm(log(", variable, "+ .1) ~ time.f + new_id.f + timeunit.f , data = data_use)", sep = "")
  eval(parse(text = model))
  return(lm)
}


middle_model <- function(data, nutrient = "calories", calorie_cut_points = c(800, 7000), sdcut = 2,  
                             k_range =c(-2, 3), time_ref = "-2"){
  #cut the sample to a reasonable range
  variable <- paste("sum", nutrient, sep = "_")
  total_cal <- calorie_cut_points*28
  data_use <- data[data$sum_calories <= total_cal[2] & data$sum_calories >= total_cal[1], ]
  do <- paste("data <- data[data$", variable, "< mean(data$", variable, ") + 3*sd(data$", 
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
  model <- paste("lm <- lm(log(", variable, "+ .1) ~ time.f + new_id.f + timeunit.f , data = data_use)", sep = "")
  eval(parse(text = model))
  return(lm)
}


#model selection####
met1_binary <- binary_model(data=met1_trans_clean_keep)
met1_sum <- sum_model_new(data=met1_trans_clean_keep, k_ref = "-6")
met1_sum12 <- sum_model_new(data = met1_trans_clean_keep, k_range = c(-12, 12), k_ref = "-12")
met1_sum20 <- sum_model_new(data = met1_trans_clean_keep, k_range = c(-20, 20), k_ref = "-20")
met1_multi <- multirange_model(data=met1_trans_clean_keep)
met1_multi_10123 <-multirange_model(data=met1_trans_clean_keep, k_range = c(-6, -1, 3, 6))
AIC(met1_binary)
AIC(met1_sum)
AIC(met1_multi)
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


## plot model####
plot_model <- function(model){
  library(ggplot2)
  coef <- data.frame(summary(model)$coefficients)
  colnames(coef) <- c("estimate", "SE", "t_value", "p")
  plot_table <- coef[ rownames(coef) %in% c("(Intercept)","after.f1","after.fbmin","after.fbmax"), c(1,2)]
  plot_table$time <- rownames(plot_table)
  plot_table$time[plot_table$time =="after.fbmin"] <- -2
  plot_table$time[plot_table$time == "(Intercept)"] <- -1
  plot_table$time[plot_table$time == "after.f1"] <- 1
  plot_table$time[plot_table$time == "after.fbmax"] <- 2
  g1 <- ggplot(plot_table, aes(x= time , y= estimate)) + 
    geom_errorbar(aes(ymin=estimate-1.96*SE, ymax= estimate + 1.96*SE, colour = "error_bar"), width=.3) +
    geom_line() +
    geom_point() +
    geom_abline(slope = 0)
  
  #plot_table <- coef[, c(1,2)]
  #plot_table$timeunit_level <- rownames(plot_table)
  #plot_table$timeunit <- plot_table$timeunit_level %>% gsub("timeunit.f", "", .) %>% as.numeric()
  #plot_table <- plot_table[!is.na(plot_table$timeunit),]
  #g2 <- ggplot(plot_table, aes(x= timeunit , y= estimate)) + 
    #geom_errorbar(aes(ymin=estimate-1.96*SE, ymax= estimate + 1.96*SE, colour = "error_bar"), width=.3) +
    #geom_line() +
    #geom_point() +
    #geom_abline(slope = 0)
}
