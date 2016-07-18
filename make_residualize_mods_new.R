#Make new model to residualize outcomes
control_1 <- read.csv("control_1_cl_keep.csv")
control_2<- read.csv("control_2_cl_keep.csv")

control_1$timeunit <- factor(control_1$timeunit)
control_2$timeunit <- factor(control_2$timeunit)

res_mod_1_carb <- lm(sum_carb ~ timeunit, data = control_1)
res_mod_1_calories <- lm(sum_calories ~ timeunit, data = control_1)
res_mod_1_sugar <- lm(sum_sugar ~ timeunit, data = control_1)
res_mod_2_carb <- lm(sum_carb ~ timeunit, data = control_2)
res_mod_2_calories <- lm(sum_calories ~ timeunit, data = control_2)
res_mod_2_sugar <- lm(sum_sugar ~ timeunit, data = control_2)

setwd("/Users/corey/Desktop/Data+/Models/met_models")
save(res_mod_1_carb, file = "res_mod_1_carb.RData")
save(res_mod_1_calories, file = "res_mod_1_calories.RData")
save(res_mod_2_carb, file = "res_mod_2_carb.RData")
save(res_mod_2_calories, file = "res_mod_2_calories.RData")
save(res_mod_1_sugar, file = "res_mod_1_sugar.RData")
save(res_mod_2_sugar, file = "res_mod_2_sugar.RData")

