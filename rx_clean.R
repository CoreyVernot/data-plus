#Load Rx Data




make_rx_hist <- function(rx_id, brands, iri_week_dir = "/Users/corey/Desktop/Data+/data"){
  library(dplyr)
  rx_brand <- rx_id[rx_id$Rx_Brand %in% brands, ]
  setwd(iri_week_dir)
  iri_week <- read.csv("iri_week.csv")
  iri_week$Start.Date <- iri_week$Start.Date %>% as.character() %>% as.Date(, format = "%Y-%m-%d")
  iri_week$End.Date <- iri_week$End.Date %>% as.character() %>% as.Date(, format = "%Y-%m-%d")
  rx_hist <- merge(rx_brand, iri_week, by.x = "Week", by.y = "IRI.Week")
  colnames(rx_hist)[colnames(rx_hist) == "Start.Date"] <- "min_fill" 
  colnames(rx_hist)[colnames(rx_hist) == "End.Date"] <- "max_fill" 
  
  
  rx_hist <- rx_hist[ , c("Week", "new_id", "min_fill", "max_fill", "RxDays", "Rx_Brand" )]
  rx_hist <- rx_hist[order(rx_hist$Week), ]
  rx_hist <- rx_hist[order(rx_hist$new_id), ]
  
  rx_hist$min_stop0 <- rx_hist$max_stop0 <- rx_hist$min_start <- rx_hist$max_start <- 
    rx_hist$min_stop <- rx_hist$max_stop <- as.Date("3000-01-01", fomrat = "%Y-%m-%d" )
  rx_hist$new_refill <- NA
  rx_hist$new_refill[1] <- "new"
  rx_hist$min_start[1] <- rx_hist$min_fill[1]
  rx_hist$max_start[1] <- rx_hist$max_fill[1]
  rx_hist$min_stop[1] <- rx_hist$min_start[1] + rx_hist$RxDays[1]
  rx_hist$max_stop[1] <- rx_hist$max_start[1] + rx_hist$RxDays[1]
  
  for(i in 2:nrow(rx_hist)){
    rx_hist$min_stop0[i] <- rx_hist$min_stop[i-1]
    rx_hist$max_stop0[i] <- rx_hist$max_stop[i-1]
    
    if( rx_hist$min_fill[i] > rx_hist$max_stop0[i]){
      rx_hist$new_refill[i] <- "new"
    }else{ rx_hist$new_refill[i] <- "refill"}
    
    if(rx_hist$new_refill[i] == "new"){
      rx_hist$min_start[i] <- rx_hist$min_fill[i]
      rx_hist$max_start[i] <- rx_hist$max_fill[i]
    }else{rx_hist$min_start[i] <- rx_hist$min_stop0[i]
          rx_hist$max_start[i] <- rx_hist$max_stop0[i]
    }
    rx_hist$min_stop[i] <- rx_hist$min_start[i] + rx_hist$RxDays[i]
    rx_hist$max_stop[i] <- rx_hist$max_start[i] + rx_hist$RxDays[i]
  }
  return(rx_hist)
}

days_sure_taking <- function(rx, ids, brands, take_name = "taking"){
  
  rx_brand <- rx[rx$Rx_Brand %in% brands, ]
  days_take_f <- data.frame(new_id = c(), day = c(), taking = c())
  for(i in 1:length(ids)){
    id <- ids[i]
    rx_id <- rx[rx$new_id == id, ]
    rx_hist <- make_rx_hist(rx_id, brands)
    days <- seq.Date(from = min(rx_hist$min_fill), to = max(rx_hist$max_fill), by = 1)
    gaps <- rx_hist %>% filter(min_fill > max_stop0)
    zero_days <- c()
    for(i in 1:nrow(gaps)){
      seq <- seq.Date(gaps$max_stop0[i], gaps$min_fill[i] - 1, by = 1)
      zero_days <- c(zero_days, seq)
    }
    days_take <- data.frame(day = days, taking = 1)
    days_take$taking[days_take$day %in% zero_days] <- 0
    days_take$new_id <- id
    days_take_f <- rbind(days_take_f, days_take)
  }
  
  colnames(days_take_f)[colnames(days_take_f) == "taking"] <- take_name
  return(days_take_f)
}




#Call make_rx_history for every panelid in our dataset