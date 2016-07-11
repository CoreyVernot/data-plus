


setwd("/Users/corey/Desktop/Data+/data")
rx <- read.csv("rx_keep.csv")

brandMet <- c("Fortamet", "Glucophage", "Glumetza", "Riomet", "Obimet",
              "Dianben", "Diaformin", "Siofor", "Metfogamma", "Janumet",
              "Kazano", "Invokamet", "Xigduo", "Synjardy", "Metaglip" ,
              "Jentaduo" , "Actoplus", "Prandimet", "Avandamet", "Kombiglyze", 
              "Glumetza", "Metformin")


metIDs <- getNewIDs(brandMet, rx = rx_keep)

ids <- metIDs$IDs[[1]]
Brands <- metIDs$Brands[[1]]
days_take <- days_sure_taking(rx_keep, ids = ids, brands = Brands, iri_week_dir = "D:/Duke Grad/2016 Summer/keep", take_name = "taking_metformin")


for(i in 1:length(ids)){
  print(i)
  id <- ids[i]
  rx_id <- rx[rx$new_id == id, ]
  rx_hist <- make_rx_hist(rx_id, brands = brands, iri_week_dir = "D:/Duke Grad/2016 Summer/keep")
}



