data <- trans_cl

residualize <- function(data, hhsize = 1,
                        nutrients = c("fat", "sat_fat", "calories", "cholesterol", "carb", "sugar", "sodium"),
                        model_dir =  "Z:/InternWorkspace"){
  data_resid <- data[  , c("panelid", "timeunit", "k", "k_zero_timeunit")]
  data_resid$timeunit <- factor(data_resid$timeunit)
  for(i in 1:length(nutrients)){
    current_dir <- getwd()
    setwd(model_dir)
    nut <- nutrients[i]
    model <- paste("res_mod_", hhsize, "_", nut, sep = "")
    mod_name <- paste(model, ".RData", sep = "")
    load(mod_name)
    variable <- paste("sum", nut, sep = "_")
    do <- paste("data_resid$", variable, "<- data$", variable, "- predict( ", model, ", newdata = data_resid)", sep = "")
    eval(parse(text = do))
  }
  data_resid$residual <- TRUE
  data_resid$k <- as.character(data_resid$k)
  data_resid$real_calories <- data$sum_calories
  setwd(current_dir)
  return(data_resid)
}

mod_name

control_cl$timeunit <- factor(control_cl$timeunit)
res_mod_1_carb <- lm(sum_carb ~ timeunit, data = control_cl)
save(res_mod_2_carb, file = "res_mod_2_carb.RData")


