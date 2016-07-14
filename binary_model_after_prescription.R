met1 = combine_control_new(met1_trans_clean_keep, control_1_cl_keep)
met2 = combine_control_new(met2_trans_clean_keep, control_2_cl_keep)

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

binary_model <- function(data, calorie_cut_points = c(800,7000), k_range=c(-12,12),nutrient="calories", ref = "bmin"){
  #cut the sample to a reasonable range
  variable <- paste("sum", nutrient, sep = "_")
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
  data_use$after.f = factor(data_use$after)
  if(!is.na(ref)){data_use$after.f <- relevel(data_use$after.f, ref = ref)}
  data_use$new_id.f <- factor(data_use$new_id)
  data_use$timeunit.f <- factor(data_use$timeunit)
  model <- paste("lm <- lm(log(", variable, "+ .1) ~ after.f + new_id.f + timeunit.f , data = data_use)", sep = "")
  eval(parse(text = model))
  return(lm)
}

resid_model <- function(data, nutrient = "calories", calorie_cut_points = c(800, 7000), sdcut = 2,  
                        k_range =c(-6, -2, 2, 6), time_ref = "-2"){
  #cut the sample to a reasonable range
  variable <- paste("sum", nutrient, sep = "_")
  total_cal <- calorie_cut_points*28
  data_use <- data[data$sum_calories <= total_cal[2] & data$sum_calories >= total_cal[1], ]
  #do <- paste("data <- data[data$", variable, "< mean(data$", variable, ") + 3*sd(data$", 
  #            variable, "), ]", sep = "")
  #eval(parse(text = do))
  
  #convert k values to bmin and bmax out of k_range
  bmin <- data_use$k < k_range[1]
  bmax <- data_use$k > k_range[4]
  data_use$k[bmin] <- "bmin"
  data_use$k[bmax] <- "bmax"
  
  #assign variable for 5 time ranges
  data_use$time <- ifelse(data_use$k %in% k_range[2]:k_range[3], 0, 
                          ifelse(data_use$k %in% (k_range[3]+1):k_range[4], 1,
                                 ifelse(data_use$k %in% k_range[1]:(k_range[2]-1), -1,
                                        ifelse(data_use$k == "bmax", 2, -2))))
  
  # Cut out all values for a specific panelid that are far above or below that panelid's average during the study period
  do <- paste("sd_s <- data_use %>% group_by(new_id) %>% summarize(upper = mean(", variable, ") + ",
              sdcut, "*sd(", variable, "), lower = mean(",variable, ") - ", sdcut, "*sd(",
              variable, "))", sep = "")
  eval(parse(text = do))
  data_use <- merge(data_use, sd_s, by = "new_id", all.x = T)
  do <- paste("keep <- data_use$", variable, " <= data_use$upper & data_use$", variable, 
              " >= data_use$lower", sep = "")
  eval(parse(text = do))
  data_use <- data_use[keep, ]
  
  #run the model
  data_use$time.f <- factor(data_use$time)
  if(!is.na(time_ref)){data_use$time.f <- relevel(data_use$time.f, ref = time_ref)}
  data_use$new_id.f <- factor(data_use$new_id)
  data_use$timeunit.f <- factor(data_use$timeunit)
  model <- paste("lm <- lm( ", variable, " ~ time.f + new_id.f + timeunit.f , data = data_use)", sep = "")
  eval(parse(text = model))
  return(lm)
}


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
