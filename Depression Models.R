library(dplyr)
library(ggplot2)
setwd("~/Desktop/Data+/data")


snri_cl <- read.csv("SNRI_trans_clean_keep.csv")
ssri_cl <- read.csv("SSRI_trans_clean_keep.csv")
tca_cl <- read.csv("TCA_trans_clean_keep.csv")
control_1 <- read.csv("control_1_cl_keep.csv")

snri_cl <- combine_control_new(snri_cl, control_1)
ssri_cl <- combine_control_new(ssri_cl, control_1)
tca_cl <- combine_control_new(tca_cl, control_1)


#Write Models####
snri_1_matt_carb <- sum_model_new(snri_cl, k_ref = "bmin", k_range =c(-6, 6))
snri_1_matt_cal <- sum_model_new(snri_cl, nutrient = "calories", k_ref = "bmin", k_range =c(-6, 6))
snri_1_matt_sugar <- sum_model_new(snri_cl, nutrient = "sugar", k_ref = "bmin", k_range =c(-6, 6))
snri_1_matt_fat <- sum_model_new(snri_cl, nutrient = "fat", k_ref = "bmin", k_range =c(-6, 6))


ssri_1_matt_carb <- sum_model_new(ssri_cl, k_ref = "bmin", k_range =c(-6, 6))
ssri_1_matt_cal <- sum_model_new(ssri_cl, nutrient = "calories", k_ref = "bmin", k_range =c(-6, 6))
ssri_1_matt_sugar <- sum_model_new(ssri_cl, nutrient = "sugar", k_ref = "bmin", k_range =c(-6, 6))
ssri_1_matt_fat <- sum_model_new(ssri_cl, nutrient = "fat", k_ref = "bmin", k_range =c(-6, 6))


tca_1_matt_carb <- sum_model_new(tca_cl, k_ref = "bmin", k_range =c(-6, 6))
tca_1_matt_cal <- sum_model_new(tca_cl, nutrient = "calories", k_ref = "bmin", k_range =c(-6, 6))
tca_1_matt_sugar <- sum_model_new(tca_cl, nutrient = "sugar", k_ref = "bmin", k_range =c(-6, 6))
tca_1_matt_fat <- sum_model_new(tca_cl, nutrient = "fat", k_ref = "bmin", k_range =c(-6, 6))

#Save Models ####
setwd("~/Desktop/Data+/Models/dep_models")
save(snri_1_matt_carb, file = "snri_1_matt_carb.RData")
save(snri_1_matt_cal, file = "snri_1_matt_cal.RData")
save(snri_1_matt_sugar, file = "snri_1_matt_sugar.RData")
save(snri_1_matt_fat, file = "snri_1_matt_fat.RData")

save(ssri_1_matt_carb, file = "ssri_1_matt_carb.RData")
save(ssri_1_matt_cal, file = "ssri_1_matt_cal.RData")
save(ssri_1_matt_sugar, file = "ssri_1_matt_sugar.RData")
save(ssri_1_matt_fat, file = "ssri_1_matt_fat.RData")

save(tca_1_matt_carb, file = "tca_1_matt_carb.RData")
save(tca_1_matt_cal, file = "tca_1_matt_cal.RData")
save(tca_1_matt_sugar, file = "tca_1_matt_sugar.RData")
save(tca_1_matt_fat, file = "tca_1_matt_fat.RData")

names <- c("snri_1_matt_carb", "snri_1_matt_cal", "snri_1_matt_sugar", "snri_1_matt_fat",
  "ssri_1_matt_carb", "ssri_1_matt_cal", "ssri_1_matt_sugar", "ssri_1_matt_fat",
  "tca_1_matt_carb", "tca_1_matt_cal", "tca_1_matt_sugar", "tca_1_matt_fat")


pdf("dep_models_plot12.pdf")

for( i in 1:length(names)){
  print(i)
  mod <- names[i]
  do <- paste("load('", mod, ".RData')" , sep="")
  eval(parse(text=do))
  do <- paste( "print(plot_model(", mod, ", title = '",mod,  "', k_vals = as.character(-6:6) ))", sep = "")
  eval(parse(text=do))
  
  do <- paste("rm(",mod,")",sep="")
  eval(parse(text=do))
}

dev.off()



par(mfrow = c(2,1))
hist(coef[id,1], xlim = c(-1.5, 1))
hist(coef[timeunit,1], xlim = c(-1.5, 1))
hist(coef[k,1], xlim = c(-1.5, 1))


#Std. Error of coef
hist(coef[id,2], xlim = c(.0, .1), breaks = 1000)
hist(coef[timeunit,2], xlim = c(.0, .1))
hist(coef[k,2], xlim = c(.0, .1))

setwd("~/Desktop/Data+/Models/bb_models")
load("bb_1_matt_carb.RData")
coef <- summary(tca_1_matt_fat)$coefficients
timeunit <- grep ("timeunit", rownames(coef))
id <- grep("new_id", rownames(coef))
k <- grep("k", rownames(coef))
