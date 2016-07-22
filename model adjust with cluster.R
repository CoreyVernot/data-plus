library(plm)
library(lmtest)
library(sandwich)

#try variance covariance matrix under clustered SEs
#credit to "http://www.r-bloggers.com/easy-clustered-standard-errors-in-r/"
cluster_lm <-function(model, cluster)
{
  require(multiwayvcov)
  require(lmtest)
  vcovCL<-cluster.vcov(model, cluster)
  
  coef<-coeftest(model, vcovCL)
  w<-waldtest(model, vcov = vcovCL, test = "F")
  ci<-get_confint(model, vcovCL)
  
  return(list(coef, w, ci))
}

met1_bi <- binary_model(met1)
#create data_use: make sure using the same number of rows as original data####
data = met1_resid
calorie_cut_points = c(800,7000)
k_range=c(-6,6)
nutrient="calories"
variable <- paste("sum", nutrient, sep = "_")
total_cal <- calorie_cut_points*28
data_use <- data[data$sum_calories <= total_cal[2] & data$sum_calories >= total_cal[1], ]
do <- paste("data_use <- data_use[data_use$", variable, "< mean(data_use$", variable, ") + 3*sd(data_use$", 
            variable, "), ]", sep = "")
eval(parse(text = do))


#adjust for variance and covariance####
a<-cluster.vcov(met1_bi, data_use$new_id)
coeftest(met1_bi)
coeftest(met1_bi, a)
get_confint<-function(model, vcovCL){
  t<-qt(.975, model$df.residual)
  ct<-coeftest(model, vcovCL)
  est<-cbind(ct[,1], ct[,1]-t*ct[,2], ct[,1]+t*ct[,2])
  colnames(est)<-c("Estimate","LowerCI","UpperCI")
  return(est)
}
c <- get_confint(met1_bi, a)
waldtest(met1_bi, vcov = a, test = "F") ##not working!!

#test with residualized model####

sum_resid_model_new <- function(data, nutrient = "calories", calorie_cut_points = c(800, 7000), k_range =c(-6, 6), k_ref = "-1"){
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
  model <- paste("lm <- lm( ", variable, " ~ k.f + new_id.f , data = data_use)", sep = "")
  eval(parse(text = model))
  return(lm)
}

met1_sum_resid <- sum_resid_model_new(met1_resid)
a<-cluster.vcov(met1_sum_resid, data_use$new_id)
coeftest(met1_sum_resid)
coeftest(met1_sum_resid, a)
waldtest(met1_sum_resid, vcov = a, test = "F") ##not working!!
