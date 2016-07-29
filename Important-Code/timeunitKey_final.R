#creating timeunit to date key
timeunitKey <- function(){
  IRI_timeunit <- read.csv("IRI_timeunit.csv")
  IRI_timeunit$start <- IRI_timeunit$start %>% as.character() %>% as.Date(format = "%Y-%m-%d")
  IRI_timeunit$end <- IRI_timeunit$end %>% as.character() %>% as.Date(format = "%Y-%m-%d")
  
  date <- seq.Date(IRI_timeunit$start[1], IRI_timeunit$end[1], by = 1)
  timeunit1 <- rep(IRI_timeunit$timeunit[1], length(date))
  for(i in 2:nrow(IRI_timeunit)){
    a <- seq.Date(IRI_timeunit$start[i], IRI_timeunit$end[i], by = 1)
    date <- c(date, a)
    timeunit1 <- c(timeunit1, rep(IRI_timeunit$timeunit[i], length(a)))
  }
  key_timeunit <- data.frame(timeunit = timeunit1, date = date)
  return(key_timeunit)
}
