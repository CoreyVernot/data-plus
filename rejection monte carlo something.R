

#Rejection sampling illustration

x <- seq(-3, 3, .01)
norm <- dnorm(x)
plot(norm ~ x, pch = NA)
lines(norm ~ x)
control <- read.csv("control_1_cl_keep.csv")
names(control)

hist((log(control$sum_sugar)))
nutrients <- c("fat", "carb", "sugar","sodium","sat_fat","protein","fiber","cholesterol","calories")

avg_list <- list()
for(n in nutrients){
  avg_list[[n]] <- data.frame(unique(control$new_id),NA)
  names(avg_list[[n]]) <- c("id","avg")
}

avg_nutrients <- function(control,nutrients = c("fat", "carb", "sugar","sodium","sat_fat","protein","fiber","cholesterol","calories")){
  control_avg <- data.frame(matrix(ncol=length(nutrients)))
  names(control_avg) <- nutrients

  string <- "control_avg <- control %>% group_by(new_id) %>% summarize("
  for(i in 1:length(nutrients)){
    n = nutrients[i]
    if(i == length(nutrients))
      add <- paste("mean_",n,"=","mean(sum_",n,", na.rm=T))",sep="") #the last one is slightly different than the others
    else{
      add <- paste("mean_",n,"=","mean(sum_",n,", na.rm=T),",sep="")
    }
    string <- paste(string,add)
  }
  eval(parse(text=string))
  ret <- (merge(control,control_avg,by="new_id",all=T))
  return(ret)
}
#tentative arbitrary bins by 10,000 calories per month

bin_nutrients <- function(control,nutrients = c("fat", "carb", "sugar","sodium","sat_fat","protein","fiber","cholesterol","calories"),bin_lengths = rep(10000,length(nutrients))){ #nutrients and bin_lengths should correspond by index
  control_avg <- avg_nutrients(control,nutrients)
  mean_cols <- c("new_id")
  for(col in names(control_avg)){
    if(grepl("mean",col))
      mean_cols <- c(mean_cols,col)
  }
  sub <- control_avg[,names(control_avg) %in% mean_cols]
  sub <- sub[!duplicated(sub[["new_id"]]),]
  
  mean_cols <- mean_cols[mean_cols != "new_id"] 
  #take out id from the cols we're removing from control. The goal is to be mutually exclusive except for new_id for faster computations
  control_avg <- control_avg[, !names(control_avg) %in% c(mean_cols,"X","X.x","X.y")]

  for(i in 1:length(nutrients)){
    bin_length <- bin_lengths[i]
    mean_nutrient <- paste("mean_",nutrients[i],sep="")
    max_bin <- sub[[mean_nutrient]][order(sub[[mean_nutrient]], decreasing = T)][1]
    temp <- ceiling(max_bin/bin_length)*bin_length
    if(temp < max_bin){max_bin <- temp + bin_length}else{max_bin <- temp}
    #always round up to the next bin; right bound is inclusive
    
    
    prev <- 0
    bin_nutrient <- paste("bin_",nutrients[i],sep="")
    sub[[bin_nutrient]] <- NA
    for(bin in seq(bin_length,max_bin,bin_length)){
      cat(nutrients[i],"\t",bin,"\n")
      for(j in 1:dim(sub)[1]){
        avg <- sub[[mean_nutrient]][j]
        if(is.na(sub[[bin_nutrient]][j]) & avg <= bin){
          sub[[bin_nutrient]][j] <- bin
        }
      }
      prev <- bin
    }
    
  }#end big for loop
  print("done with big for loop!!!")
  
  ret <- merge(control_avg,sub,by=c("new_id"))
  return(ret)
}#end function

#control_avg2 <- data.frame(control_avg[order(control_avg[["bin_calories"]]),])
  
  
#determining good bins
control_avg <- avg_nutrients(control)
par(mfrow=c(3,3))
for(n in nutrients){
  m <- paste("mean","_",n,sep="")
  hist(control_avg[[m]],main=m)
}
