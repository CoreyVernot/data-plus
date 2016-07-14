
path <- "C:\\Users\\Nathaniel Brown\\Documents\\GitHub\\data-plus\\"
setwd(path)

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

bin_nutrients <- function(control,nutrients = c("fat", "carb", "sugar","sodium","sat_fat","protein","fiber","cholesterol","calories"),
                                  bin_lengths = c(500, 5000,    2000,   50000,   1000,     2000,     1000,   2000,         50000)){ #nutrients and bin_lengths should correspond by index
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

plot_by_bins <- function(control_avg_bin){
  bins  <- names(control_avg_bin)[grepl("bin_", names(control_avg_bin))]
  means <- names(control_avg_bin)[grepl("mean_",names(control_avg_bin))]
  nutrients <- gsub("bin_","",bins)
  for(each_bin in bins){
    for(each_val in unique(control_avg_bin[[each_bin]])){
      print(each_bin);print(each_val)
      sub <- control_avg_bin[control_avg_bin$bin_calories == each_val,]
      hist(sub[[each_bin]])
      
    }
  }
      
      
  #hist of nut within nut bin
  #and
  #scatterplot of mean_nut vs timeunit within nut bin
}

#### DON'T RUN THESE UNLESS ABSOLUTELY NECESSARY ####
control <- read.csv("C:\\Users\\Nathaniel Brown\\workspace\\BECR\\control_1_cl_keep.csv")
control_avg <- avg_nutrients(control)
system.time(control_avg_bin <- bin_nutrients(control))
#### DON'T ####

test <- plot(control_avg$mean_sugar[control_avg$timeunit == 68] ~ control_avg$sum_sugar[control_avg$timeunit==68])
par(mfrow=c(2,2))
plot(test)
par(mfrow=c(1,1))
#determining good bin sizes
par(mfrow=c(3,3))
for(n in nutrients){
  m <- paste("mean","_",n,sep="")
  hist(control_avg[[m]],main=m)
}
#nutrients <- c("fat", "carb", "sugar","sodium","sat_fat","protein","fiber","cholesterol","calories")
#bins      <- c(2e3/4, 0.5e5,  1e5/5,  2e5/4,   1000,     1e5/5,    1000,   1e5/5,        1e6/5    )
#these are just the sizes of the bins that default histograms give us


#### DIST OF AVG CALORIES ####
hist(control_avg$mean_cal)
d <- (density(control_avg$mean_cal))

# mode of gamma = (shape -1)*scale --> mode = 50000 -> shape = 500, scale = 100?
abline(v = 42000, col = "red")
mode = 42000
shape = 6
scale = mode/(shape -1)
x <- 0:300000

plot(d)
lines(dgamma(x, shape =  shape, scale = scale) ~ x, col = "blue")
#official avg cal model is gamma(6,8400)

#### DIST OF AVG SUGAR ####
hist(control_avg$mean_sugar)
d <- (density(control_avg$mean_sugar))

mode <- 2700
abline(v = mode, col = "red")
shape = 4
scale = mode/(shape -1)
x <- 0:300000

plot(d)
lines(dgamma(x, shape =  shape, scale = scale) ~ x, col = "blue")
#official sugar model is gamma(4, 900)

#### DIST OF AVG CARBS ####
hist(control_avg$mean_carb)
d <- (density(control_avg$mean_carb))

mode <- 5200
abline(v = mode, col = "red")
shape = 5
scale = mode/(shape -1)
x <- 0:300000

plot(d)
lines(dgamma(x, shape =  shape, scale = scale) ~ x, col = "blue")
#official carb model is gamma(5,1040)


save.image()
