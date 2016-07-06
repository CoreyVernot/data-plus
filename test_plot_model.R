# plot model for beta blocker
# add confidence interval
library(ggplot2)
library(dplyr)
graphData <- function(nutrient = "calories", data, k_range = c(-6, 6), calorie_cut_points = c(800, 7000), all_sd_cut  = 2, all_k_only = T, sam = F, main = NA, ylab = NA){
  variable <- paste("sum", nutrient, sep = "_")
  total_cal <- calorie_cut_points*28
  data<- data[data$sum_calories <= total_cal[2] & data$sum_calories >= total_cal[1], ]
  # "data <- data[data$sum_calories< mean(data$sum_calories) + 2*sd(data$sum_calories), ]"
  do <- paste("data <- data[data$", variable, "< mean(data$", variable, ") + 2*sd(data$", 
              variable, "), ]", sep = "")
  eval(parse(text = do))
  data <- data[data$k >= k_range[1] & data$k <= k_range[2], ]
  data <- na.omit(data)
  seq <- seq(k_range[1], k_range[2])
  pids <- unique(data$new_id)
  keep <- rep(NA, length(pids))
  for(i in 1:length(keep)){
    d <- data[data$new_id == pids[i], ]
    k <- unique(d$k)
    logical <- seq %in% k
    keep[i] <- Reduce("&", logical)
  }
  if(all_k_only){ pids <- pids[keep]}
  data <- data[data$new_id %in% pids, ]
  # "data_k <- data %>% group_by(k) %>% summarise(nutrient = mean(sum_calories), sd = sd(sum_calories)/sqrt(n()), n = n())"
  make_data_k <- paste(
    "data_k <- data %>% group_by(k) %>% summarise(nutrient = mean(",
    variable, ")", ", sd = sd(", variable, ")/sqrt(n()), n = n())" , sep = "")
  eval(parse(text = make_data_k))
  data_k$upper <- data_k$nutrient + data_k$sd*1.96
  data_k$lower <- data_k$nutrient - data_k$sd*1.96
  if(sam){
    nut_0 <- data_k$nutrient[data_k$k == 0]
    up_0 <- data_k$upper[data_k$k == 0]
    low_0 <- data_k$low[data_k$k == 0]
    data_k$nutrient <- (data_k$nutrient - nut_0)/nut_0*100
    data_k$upper <- (data_k$upper - up_0)/up_0*100
    data_k$lower <- (data_k$lower - low_0)/low_0*100
  }
  if(is.na(ylab)){ylab <- nutrient}
  if(is.na(main)){main <- paste(nutrient, "over time")}
  plot(NA, xlim = k_range, ylim = c(min(data_k$lower), max(data_k$upper)),
       xlab = "Timeunits after first prescription", ylab = ylab, 
       main = main)
  lines(seq, data_k$nutrient, lwd = 2)
  if(!sam){lines(seq, data_k$upper, lty = 2)}
  if(!sam){lines(seq, data_k$lower, lty = 2)}
  abline(h = data_k$nutrient[data_k$k == 0] )
  cat(paste("number of people for k_range: ", paste(as.character(k_range), collapse = " , "),
            "=", as.character(length(pids))))
}


sum_model <- function(data, nutrient = "calories", calorie_cut_points = c(800, 7000), sdcut = 2,  k_range =c(-12, 12), k_ref = "-1", tu_per_id = 10){
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


bb_sum <- sum_model(data = bb1_trans_clean_keep)
bb2_sum <- sum_model(data = bb2_trans_clean_keep)

plot_model <- function(data, k_vals = as.character(-6:6)){
  coef <- data.frame(summary(data)$coefficients)
  colnames(coef) <- c("estimate", "SE", "t_value", "p")
  rownames <- paste("k.f", k_vals, sep = "")
  plot_table <- coef[ rownames(coef) %in% rownames, c(1,2)]
  plot_table$k_level <- rownames(plot_table)
  plot_table$k <- plot_table$k_level %>% gsub("k.f", "", .) %>% as.numeric()
  ggplot(plot_table, aes(x= k , y= estimate)) + 
    geom_errorbar(aes(ymin=estimate-1.96*SE, ymax= estimate + 1.96*SE, colour = "error_bar"), width=.3) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 0)
}

plot_model(data = bb_sum)
plot_model(data = bb2_sum)
