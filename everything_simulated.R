#everything simulated
#### ALL THE FUNCTIONS YOU COULD EVER DREAM OF ####
make_models <- function(control_avg, nutrients = c("calories","carb","sugar"), timeunits = 28:92){
  model_list <- list()
  for(n in nutrients){
    for(t in timeunits){ #full timeunit range
      sub <- control_avg[control_avg[["timeunit"]] == t, grepl(n,names(control_avg))]
      name <- paste(n,"_",t,sep="")
      string <- paste(name," <- lm(sum_",n, "~ mean_",n,", data = sub)",
                      sep="")
      model_list[[name]] <- eval(parse(text=string))
    }
  }
  return(model_list)
}

simulate_new <- function(mod, nsim=1, seed=NULL, newdata, ...) {
  pred <- predict(mod, newdata = newdata)
  mod$fitted.values <- pred
  sim <- simulate(object=mod, nsim=nsim, seed=seed)
  for(j in 1: length(sim)){
    for(i in 1:length(sim)[[j]]){
      while(sim[i, j] < 0){
        sim[i,j] <- 0#simulate(object=mod, nsim=1, seed=seed+as.integer(sim[i]))
      }
    }
  }
  
  return(sim)
}

sim_individual <- function(mods, nutrients, timeunits = 30:90, nsim=1, seed=NULL,Metformin = FALSE){
  #official avg cal model is gamma(6,8400)
  #official avg sugar model is gamma(4, 900)
  #official avg carb model is gamma(5,1040)
  nutrient_means <- rep(NA,9)
  names(nutrient_means) <- c("fat","carb","sugar","sodium","sat_fat","protein","fiber","cholesterol","calories")
  set.seed(seed)
  #nutrient_means[["sugar"]] <- rgamma(n=1,shape=4,scale=900)
  #nutrient_means[["carb"]] <- rgamma(n=1,shape=5,scale=1040)
  
  if(Metformin){nutrient_means[["calories"]] <- rgamma(n=1,shape = 6.5,scale=8363.636)
  }else{        nutrient_means[["calories"]] <- rgamma(n=1,shape = 6,scale=8400)}
  
  ret <- data.frame(matrix(ncol=2*length(nutrients)+2))
  names(ret) <- c("new_id","timeunit",paste("sum_",nutrients,sep=""),paste("mean_",nutrients,sep=""))
  
  
  #system.time(mods <- make_models(control_avg,nutrients,timeunits))
  for(j in 1:length(nutrients)){
    n <- nutrients[j]
    mean_nutrient <- paste("mean_",n,sep="")
    for(i in 1:length(timeunits)){
      t <- timeunits[i]
      mod_name <- paste(n,"_",t,sep="")
      nd <- list()
      nd[[mean_nutrient]] <- nutrient_means[[n]] #newdata gets a random mean from a pre-specified distribution of means
      nd <- as.data.frame(nd)
      sum <- simulate_new(mod = mods[[mod_name]],nsim=1,newdata = nd)
      ret[i,j+2] <- sum #these are the predicted sum columns
    }
    ret[[mean_nutrient]] <- nutrient_means[[n]]
  }
  ret[["timeunit"]] <- timeunits
  return(ret)
}

sim_df <- function(mods, nutrients, timeunits = 30:90, num_indivs = 1, nsim=1, seed=NULL, Metformin = FALSE){
  ret <- data.frame(matrix(ncol=2*length(nutrients)+2))
  names(ret) <- c("new_id","timeunit",paste("sum_",nutrients,sep=""),paste("mean_",nutrients,sep=""))
  
  #mods <- make_models(control_avg,nutrients,timeunits)
  for(i in 1:num_indivs){
    if(!is.null(seed)){seed <- seed + 1}
    rbind_me <- sim_individual(mods, nutrients, timeunits, nsim, seed)
    rbind_me[["new_id"]] <- -i
    ret <- rbind(ret,rbind_me)
  }
  ret <- ret[-1,] #the first row is going to be a row of NA's always
  return(ret)
}

combine_control_new<- function(trans_cl, control_cl){
  control_cl$k <- "control"
  control_cl$k_zero_timeunit <- "control"
  control_cl <- control_cl[! control_cl$new_id %in% trans_cl$new_id, ] # keep only control individuals that are not in our treatment group
  trans_cl_m <- trans_cl[, colnames(trans_cl) %in% colnames(control_cl)]
  control_cl <- control_cl[, colnames(control_cl) %in% colnames(trans_cl_m)]
  Trans_cl <- rbind(trans_cl_m, control_cl)
  return(Trans_cl)
}

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
  model <- paste("lm <- lm(log(", variable, "+ .1) ~ time.f + new_id.f + timeunit.f , data = data_use)", sep = "")
  eval(parse(text = model))
  return(lm)
}

middle_model <- function(data, nutrient = "calories", calorie_cut_points = c(800, 7000), sdcut = 2,  
                         k_range =c(-2, 3), time_ref = "-1"){
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
  model <- paste("lm <- lm(log(", variable, "+ .1) ~ time.f + new_id.f + timeunit.f , data = data_use)", sep = "")
  eval(parse(text = model))
  return(lm)
}

no_k_model <- function(data, nutrient = "calories", calorie_cut_points = c(800, 7000), sdcut = 2,  
                       k_range =c(-2, 3), time_ref = "-1"){
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
  data_use$new_id.f <- factor(data_use$new_id)
  data_use$timeunit.f <- factor(data_use$timeunit)
  model <- paste("lm <- lm(log(", variable, "+ .1) ~ new_id.f + timeunit.f , data = data_use)", sep = "")
  eval(parse(text = model))
  return(lm)
}

#### CODE ####
AIC_all <- data.frame(df = NA, AIC = NA, model = NA, iter = NA)
BIC_all <- data.frame(df = NA, BIC = NA, model = NA, iter = NA)
anova_all <- data.frame(bin = rep(NA,50), sum = rep(NA,50), multi  = rep(NA,50), mid = rep(NA,50))

n_met <- length(unique(met$new_id)) #met data frame is from met1_trans_clean_keep.csv
mods <- make_models(met_avg, nutrients = "calories",timeunits = 30:90)
sim_control <- read.csv("sim_control.csv")

for(i in 1:50){
  sim_met <- sim_df(mods, nutrients = "calories", timeunits = 30:90, num_indivs = n_met,Metformin=TRUE)
  ids <- unique(sim_met$new_id)
  k_key <- data.frame(new_id = ids, k_zero_timeunit = sample( 54:89, length(ids), replace = T))
  sim_met <- merge(sim_met, k_key,by = "new_id")
  sim_met$k <- sim_met$timeunit - sim_met$k_zero_timeunit
  met1 = combine_control_new(sim_met, sim_control)

  met1_c_binary <- binary_model(data=met1)
  met1_c_sum <- sum_model_new(data=met1)
  met1_c_multi <- multirange_model(data=met1)
  met1_c_middle <- middle_model(data=met1)
  met1_c_no_k <- no_k_model(data=met1)

  AIC_df <- AIC(met1_c_binary,met1_c_sum,met1_c_multi,met1_c_middle, met1_c_no_k)
  BIC_df <- BIC(met1_c_binary,met1_c_sum,met1_c_multi,met1_c_middle, met1_c_no_k)

  BIC_df$model <-rownames(BIC_df)
  BIC_df$iter <- i
  BIC_all <- rbind(BIC_all, BIC_df)

  AIC_df$model <-rownames(AIC_df)
  AIC_df$iter <- i
  AIC_all <- rbind(AIC_all, AIC_df)

  anova_all$bin[i]   <- anova(met1_c_no_k, met1_c_binary)[["Pr(>F)"]][2]
  anova_all$sum[i]   <- anova(met1_c_no_k, met1_c_sum)[["Pr(>F)"]][2]
  anova_all$multi[i] <- anova(met1_c_no_k, met1_c_multi)[["Pr(>F)"]][2]
  anova_all$mid[i]   <- anova(met1_c_no_k, met1_c_middle)[["Pr(>F)"]][2]
  print(i)
}
write.csv(AIC_all, "AIC_all.csv")
write.csv(BIC_all, "BIC_all.csv")
write.csv(anova_all, "anova_all.csv")
save.image()

