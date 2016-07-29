getRx <- function(years = c(10,11,12), server = "SQLServer_IRI" ){
  ch <- odbcConnect(server)
  rx <- data.frame()
  allRx <- paste("Rx",years,sep="") #a vector of all the data table names
  for(i in 1:length(years)){
    rx_i <- sqlFetch(ch,allRx[i])
    rx <- rbind(rx, rx_i)
    #for multiple years, the data tables are stacked on each other and returned as one data frame
  }
  return(rx)
}
