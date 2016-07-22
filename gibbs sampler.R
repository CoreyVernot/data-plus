
iri_week <- read.csv("IRI_week.csv")
rx_w <- merge(rx, iri_week,by.x = "Week", by.y = "IRI.Week")
rx_w$Start.Date <- rx_w$Start.Date %>% as.character() %>% as.Date(format = "%Y-%m-%d")
rx_w$End.Date <- rx_w$End.Date %>% as.character() %>% as.Date(format = "%Y-%m-%d")

brandsMet <- c("Fortamet", "Glucophage", "Glumetza", "Riomet", "Obimet",
            "Dianben", "Diaformin", "Siofor", "Metfogamma", "Janumet",
            "Kazano", "Invokamet", "Xigduo", "Synjardy", "Metaglip" ,
            "Jentaduo" , "Actoplus", "Prandimet", "Avandamet", "Kombiglyze", 
            "Glumetza", "Metformin")

panelids1 <- getNewIDs(brandsMet, rx = rx, new = F, HHSizes = c(1))
BrandsMet <- panelids1$Brands[[1]]
ids1 <- panelids1$IDs[[1]]
#Subset ids to only ids without missing metformin prescriptions
rx_m <- rx_w[rx_w$new_id %in% ids1 & rx_w$Rx_Brand %in% BrandsMet, ]
rx_na <- rx_m %>% group_by(new_id) %>% summarise(mean_na = mean(is.na(RxDays)))
ids <- rx_na$new_id[rx_na$mean_na == 0]
d <- as.Date("3000-01-01", format <- "%Y-%m-%d")
rx_range <- data.frame(new_id = ids, min_day = rep(d, length(ids)), max_day = rep(d, length(ids)))
for(i in 1:length(ids)){
  id <- ids[i]
  rx_id <- rx_w[rx_w$new_id == id, ]
  rx_range$min_day[i] <- min(rx_id$Start.Date)
  rx_range$max_day[i] <- max(rx_id$End.Date)
  print(i)
}

rx_m <- merge(rx_m, rx_range, by = "new_id")
 
gibbs <- function(rx_m, id, a = 57, b = 3, iter= 100){
  rx_id_master <- rx_m[rx_m$new_id == id, ]
  t <- as.list(rep(NA, iter))
  b_n <- a_n <- rep(NA, iter)
  a_n[1] <- a; b_n[1] <- b
  pt <- rep(NA, iter)
  for(i in 1:iter){
    print(i)
    rx_id <- rx_id_master
    rx_id$fill_day <- as.Date("3000-01-01", format <- "%Y-%m-%d")
      for(k in 1:nrow(rx_id)){
        rx_id$fill_day[k] <- sample(seq(rx_id$Start.Date[k], rx_id$End.Date[k], 1), 1)
      }
    rx_take <- data.frame(day = seq(rx_id$min_day[1], rx_id$max_day[1], 1), supply = NA)
    rx_id$fill <- 1
    rx_id <- rx_id[ , c("fill", "RxDays", "fill_day", "New_Refill_Sample")]
    rx_take <- merge(rx_take, rx_id, by.x = "day", by.y = "fill_day", all.x = T, )
    rx_take$fill[is.na(rx_take$fill)] <- rx_take$RxDays[is.na(rx_take$fill)] <- 0
    
    #If the first prescription observed is a refill, cut off all data prior to first refill. If first rx observed is "New", assign taking before first rx = 0
    first <- min(rx_take$day[rx_take$fill == 1])
    if(rx_take$New_Refill_Sample[rx_take$day == first] == "New"){
      rx_take$supply[rx_take$day < first] <- 0
    }else{
      rx_take <- rx_take[! rx_take$day < first, ]
    }
    
    index <- which(rx_take$day >=first) #the index for all days after and including first drug Rx
    take <- rep(NA, nrow(rx_take))
    
    pt[i] <- a/(a+b)
    rx_take$supply[index[1]] <- rx_take$RxDays[index[1]]
    take[index[1]] <- rbinom(1, 1, pt[i]) 
  
    for(j in index[-1]){
      rx_take$supply[j] <- rx_take$supply[j-1] + rx_take$RxDays[j] - take[j-1]
      if(rx_take$supply[j] > 0){
        take[j] <- rbinom(1, 1, pt[i]) 
      }else{ take[j] <- 0}
    }
    take <- data.frame(take, day = rx_take$day)
    colnames(take)[1] <- paste("take_", i, sep = "")
    t[[i]] <- take
    n_have <- sum(rx_take$supply[index] > 0)
    n_take <- sum(take[1])
    a_n[i + 1] <-  a + n_take; b_n[i + 1] <- b + n_have - n_take
    print(i)
  }
  take_f <- Reduce(function(...) merge(...), t)
  take_f$mean_take <- rowMeans(take_f[-1])
  to_return <- list(new_id = id, take_f = take_f, pt = pt, a_n = a_n, b_n = b_n)
  return(to_return)
}


gibbs4 <- gibbs(rx_m = rx_m, id = rx_m$new_id[1], iter = 1000, a = 54, b = 6)
gibbs5 <- gibbs(rx_m = rx_m, id = rx_m$new_id[1], iter = 1000, a = 18, b = 2)
gibbs3 <- gibbs(rx_m = rx_m, id = rx_m$new_id[1], iter = 1000, a = 1, b = 1)
gibbs6 <- gibbs(rx_m = rx_m, id = rx_m$new_id[1], iter = 10000, a = 1, b = 1)

pt1 <- gibbs1$pt # a = 57, b = 3
pt2 <- gibbs2$pt # a = 19, b = 1
pt3 <- gibbs3$pt # a = 1, b = 1
pt4 <- gibbs4$pt #a = 54, b = 6
pt5 <- gibbs5$pt #a = 18, b = 2
pt6 <- gibbs6$pt
take_f <- gibbs_ex$take_f
a_n <- gibbs_ex
plot(mean_take ~ day, data = take_f)
iter  <- 1:500
iter2 <- 1:1000
par(mfrow = c(3,1))
plot(pt1 ~ iter2, pch = 20, cex = .5)
plot(pt2 ~ iter2, pch = 20, cex = .5)
plot(pt3 ~ iter2, pch = 20, cex = .5)

mean(pt1)
mean(pt4)
mean(pt2)
mean(pt5)
mean(pt6)
mean(pt3)

plot(mean_take ~ day, data = gibbs1$take_f)
plot(mean_take ~ day, data = gibbs2$take_f)
plot(mean_take~ day, data = gibbs4$take_f)
