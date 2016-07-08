#Load Rx Data


setwd("/Users/corey/Desktop/Data+/data")
rx <- read.csv("rx_keep.csv")

brands <- "METFORMIN (ALL)"
make_rx_history <- function(rx, brands, iri_week_dir = "/Users/corey/Desktop/Data+/data", no_na_ids = T){
  library(dplyr)
  rx_brand <- rx[rx$Rx_Brand %in% brands, ]
  setwd(iri_week_dir)
  iri_week <- read.csv("iri_week.csv")
  iri_week$Start.Date <- iri_week$Start.Date %>% as.character() %>% as.Date(, format = "%Y-%m-%d")
  iri_week$End.Date <- iri_week$End.Date %>% as.character() %>% as.Date(, format = "%Y-%m-%d")
  rx_hist <- merge(rx_brand, iri_week, by.x = "Week", by.y = "IRI.Week")
  colnames(rx_hist)[colnames(rx_hist) == "Start.Date"] <- "min_fill" 
  colnames(rx_hist)[colnames(rx_hist) == "End.Date"] <- "max_fill" 
  
  if(no_na_ids){
    prop_na <- rx_hist %>% group_by(new_id) %>% summarize(mean_na = mean(is.na(RxDays)))
    use_ids <- prop_na$new_id[prop_na$mean_na == 0]
    rx_hist <- rx_hist[rx_hist$new_id %in% use_ids, ]
  }
  
  rx_hist <- rx_hist[ , c("Week", "new_id", "min_fill", "max_fill", "RxDays", "Rx_Brand" )]
  rx_hist <- rx_hist[order(rx_hist$Week), ]
  rx_hist <- rx_hist[order(rx_hist$new_id), ]
  
  rx_hist$min_stop0 <- rx_hist$max_stop0 <- rx_hist$new_refill <- rx_hist$min_start <- rx_hist$max_start <- min_stop <- rx_hist$max_stop <- NA
  rx_hist$new_refill[1] <- "new"
  rx_hist$
}