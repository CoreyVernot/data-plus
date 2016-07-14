setwd("C:/Users/Nathaniel Brown/workspace/BECR/")

filename <- "bb1_trans_clean_quant_tu.RData"

get_quantile_list <- function(filename){
  x1 <- load(filename)
  x2 <- eval(parse(text=x1))
  return(x2)
}
#examples:
#bb2_trans_clean_quant_tu[["calories"]][["tu_29"]][["n"]] = 506
#bb2_trans_clean_quant_tu[["calories"]][["tu_29"]][["quantile"]][["5%"]] = 618.5546

quantile_list <- get_quantile_list(filename)


data <- read.csv("bb2_trans_clean_keep.csv")
tu_quant <- function(timeunit, nutrient, trans_clean_keep){
  data_use <- trans_clean_keep[trans_clean_keep$timeunit == timeunit, ]
  n <- length(unique(data_use$new_id))
  variable <- paste("sum", nutrient, sep = "_")
  do <- paste("quant <- quantile(data_use$", variable, ", probs = seq(0, 1, .05), type = 5)")
  eval(parse(text = do))
  return(list(quantile = quant, n = n, descr = paste(nutrient, "at timeunit",timeunit)))
}

tu_quant_list <- tu_quant(66,"calories",data)


tu_quant_cdf <- function(nutrient,timeunit, trans_clean_keep){
  tu_quant_list <- tu_quant(timeunit,nutrient,trans_clean_keep)
  all_x <- seq(0,95,5)
  all_y <- rep(NA,length(all_x))
  for(i in 1:length(all_x)){
    x_i <- all_x[i]
    all_y[i] <- tu_quant_list[["quantile"]][[paste(x_i,"%",sep="")]]
  }
  (plot(all_x,all_y,xlab = "quantiles",ylab = strsplit(tu_quant_list[["descr"]],split = " ")[[1]][1], main = quantile_list[["descr"]],type="l"))
}

tu_quant_pdf <- function(nutrient,timeunit, trans_clean_keep){
  tu_quant_list <- tu_quant(timeunit,nutrient,trans_clean_keep)
  hist(tu_quant_list[["quantile"]],breaks=20)
}

tu_quant_cdf("fiber",61, data)

tu_quant_pdf("fiber",61, data)



lines(probz,qbeta(probz,.001,.01)*500)

x <- quantile_list[["quantile"]][2:20]/(quantile_list[["quantile"]][20])
fitdistr(x,"logistic",list(location=1,scale=1)) 
plot(1:19,dlogis(x=1:19,location=.473,scale=.12944))
plot(quantile_list$quantile,type="h")

?fitdistr()
library(MASS)
