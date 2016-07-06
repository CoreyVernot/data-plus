#graph data

graphData_per <- function(nutrient = "fat", data, k_range = c(-6, 6), calorie_cut_points = c(800, 7000), all_sd_cut  = 2, all_k_only = T, sam = F, main = NA, ylab = NA){
  variable <- paste(nutrient, "per", "cal", sep = "_")
  total_cal <- calorie_cut_points*28
  data<- data[data$sum_calories <= total_cal[2] & data$sum_calories >= total_cal[1], ]
  do <- paste("data <- data[data$", variable, "< mean(data$", variable, ") + ", all_sd_cut, "*sd(data$", 
              variable, "), ]", sep = "")
  eval(parse(text = do))
  data <- data[data$k >= k_range[1] & data$k <= k_range[2], ]
  data <- na.omit(data)
  seq <- seq(k_range[1], k_range[2])
  pids <- unique(data$panelid)
  keep <- rep(NA, length(pids))
  for(i in 1:length(keep)){
    d <- data[data$panelid == pids[i], ]
    k <- unique(d$k)
    logical <- seq %in% k
    keep[i] <- Reduce("&", logical)
  }
  if(all_k_only){ pids <- pids[keep]}
  data <- data[data$panelid %in% pids, ]
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

graphData <- function(nutrient = "fat", data, k_range = c(-6, 6), calorie_cut_points = c(800, 7000), all_sd_cut  = 2, all_k_only = T, sam = F, main = NA, ylab = NA){
  variable <- paste("sum", nutrient, sep = "_")
  total_cal <- calorie_cut_points*28
  data<- data[data$sum_calories <= total_cal[2] & data$sum_calories >= total_cal[1], ]
  do <- paste("data <- data[data$", variable, "< mean(data$", variable, ") + 2*sd(data$", 
              variable, "), ]", sep = "")
  eval(parse(text = do))
  data <- data[data$k >= k_range[1] & data$k <= k_range[2], ]
  data <- na.omit(data)
  seq <- seq(k_range[1], k_range[2])
  pids <- unique(data$panelid)
  keep <- rep(NA, length(pids))
  for(i in 1:length(keep)){
    d <- data[data$panelid == pids[i], ]
    k <- unique(d$k)
    logical <- seq %in% k
    keep[i] <- Reduce("&", logical)
  }
  if(all_k_only){ pids <- pids[keep]}
  data <- data[data$panelid %in% pids, ]
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


#graph data

graphLogData <- function(nutrient = "fat", data, k_range = c(-6, 6), all_k_only = T, sam = F){
  
  data <- data[data$k >= k_range[1] & data$k <= k_range[2], ]
  data <- na.omit(data)
  seq <- seq(k_range[1], k_range[2])
  pids <- unique(data$panelid)
  keep <- rep(NA, length(pids))
  for(i in 1:length(keep)){
    d <- data[data$panelid == pids[i], ]
    k <- unique(d$k)
    logical <- seq %in% k
    keep[i] <- Reduce("&", logical)
  }
  if(all_k_only){ pids <- pids[keep]}
  data <- data[data$panelid %in% pids, ]
  variable <- paste("sum",nutrient, sep = "_")
  make_data_k <- paste(
    "data_k <- data %>% group_by(k) %>% summarise(nutrient = mean(log(",
    variable, " + .1))", ", sd = sd(log(", variable, " + .1))/sqrt(n()), n = n())" , sep = "")
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
  
  plot(NA, xlim = k_range, ylim = c(min(data_k$lower), max(data_k$upper)),
       xlab = "Timeunits after first prescription", ylab = nutrient, 
       main = paste(nutrient, "over time"))
  lines(seq, data_k$nutrient, lwd = 2)
  if(!sam){lines(seq, data_k$upper, lty = 2)}
  if(!sam){lines(seq, data_k$lower, lty = 2)}
  abline(h = data_k$nutrient[data_k$k == 0] )
  cat(paste("number of people for k_range: ", paste(as.character(k_range), collapse = " , "),
            "=", as.character(length(pids))))
}
