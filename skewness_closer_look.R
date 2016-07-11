library(moments)
install.packages("moments")

# distribution closer look: bb1 and bb2####
getwd()

options("scipen"=100, "digits"=4)
hist(bb1_trans_clean_id$mean_calories, main = "bb1", xlab = "mean_cal", xlim = range(0,100000), ylab = "count", ylim = range(0,80), plot = TRUE)
hist(bb1_trans_clean_id$var_calories, main = "bb1", xlab = "var_cal", ylab = "count")
hist(bb1_trans_clean_id$skewness_calories, main = "bb1", xlab = "skew_cal", ylab = "count")
hist(bb1_trans_clean_id$kurtosis_calories, main = "bb1", xlab = "kurtosis_cal", ylab = "count")

library(ggplot2)
ggplot(bb1_trans_clean_id, aes(x = mean_calories)) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = .5, binwidth = 5000) + 
  geom_density(colour = 'blue') + xlab(expression(bold('mean_cal_distribution'))) + 
  ylab(expression(bold('Density'))) # + xlim(22400, 210000)

ggplot(bb1_trans_clean_id, aes(x = var_calories)) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = .5, binwidth = 500000000) + 
  geom_density(colour = 'blue') + xlab(expression(bold('mean_cal_distribution'))) + 
  ylab(expression(bold('Density')))

bb1_trans_clean_id$sd_calories <- sqrt(bb1_trans_clean_id$var_calories)
ggplot(bb1_trans_clean_id, aes(x = sd_calories)) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = .5, binwidth = 5000) + 
  geom_density(colour = 'blue') + xlab(expression(bold('mean_cal_distribution'))) + 
  ylab(expression(bold('Density')))

ggplot(bb1_trans_clean_id, aes(x = skewness_calories)) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = .5, binwidth = 0.3) + 
  geom_density(colour = 'blue') + xlab(expression(bold('mean_cal_distribution'))) + 
  ylab(expression(bold('Density')))

ggplot(bb1_trans_clean_id, aes(x = kurtosis_calories)) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = .5, binwidth = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('mean_cal_distribution'))) + 
  ylab(expression(bold('Density')))

#test normal distibution's skewness and kurtosis
n.sample <- rnorm(n = 10000, mean = 55, sd = 4.5)
skewness(n.sample)
#[1] -0.01127
kurtosis(n.sample)
#[1] -0.08881

ggplot(bb1_trans_clean_keep, aes(x = sum_calories)) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = .5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('sum_cal_distribution'))) + 
  ylab(expression(bold('Density')))
skewness(bb1_trans_clean_keep$sum_calories)
skewness(bb1_trans_clean_keep$sum_calories, na.rm = FALSE, type = 3) #less skewed in type 3, which is the same as unspecified

ggplot(bb2_trans_clean_keep, aes(x = sum_calories)) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = .5, binwidth = 500) + 
  geom_density(colour = 'blue') + xlab(expression(bold('sum_cal_distribution'))) + 
  ylab(expression(bold('Density'))) + xlim(0,500000)
skewness(bb2_trans_clean_keep$sum_calories)
skewness(bb2_trans_clean_keep$sum_calories, na.rm = FALSE, type = 3) #type 1 to 3 are the same as unspecified

#summary
#very right-skewed and heavy-weighted on tails for all nutrients in bb1
#right-skewed and heavy-weighted on tails for all nutrients in bb2 (not as significant as displayed in bb1)


# distribution closer look: met1 and met2####
hist(met1_trans_clean_id$skewness_calories, main = "met1", xlab = "skew_cal", ylab = "count")
hist(met1_trans_clean_id$kurtosis_calories, main = "met1", xlab = "kurtosis_cal", ylab = "count")
hist(met2_trans_clean_id$skewness_calories, main = "met2", xlab = "skew_cal", ylab = "count")
hist(met2_trans_clean_id$kurtosis_calories, main = "met2", xlab = "kurtosis_cal", ylab = "count")

#summary
#mostly right-skewed and heavy-weighted on tails for all nutrients

#distribution closer look: SNRI and SSRI and TCA####
hist(SSRI_trans_clean_id$skewness_calories, main = "SSRI", xlab = "skew_cal", ylab = "count")
hist(SSRI_trans_clean_id$kurtosis_calories, main = "SSRI", xlab = "kurtosis_cal", ylab = "count")
hist(SNRI_trans_clean_id$skewness_calories, main = "SNRI", xlab = "skew_cal", ylab = "count")
hist(SNRI_trans_clean_id$kurtosis_calories, main = "SNRI", xlab = "kurtosis_cal", ylab = "count")
hist(TCA_trans_clean_id$skewness_calories, main = "TCA", xlab = "skew_cal", ylab = "count")
hist(TCA_trans_clean_id$kurtosis_calories, main = "TCA", xlab = "kurtosis_cal", ylab = "count")

#summary
#skewness usually range between (-0.5) and 7.5
#kurtosis usually range between 1.5 and 60
#SSRI
#somewhat right-skewed and weight more (the range vary widely from individuals) on tails
#SNRI
#somewhat right-skewed and weight more (the range vary widely from individuals) on tails
#TCA
#somewhat right-skewed and weight more (the range vary widely from individuals) on tails

#in the above results, those with left skewed are not many in the datasets and mostly slightly-left-skewed


# distribution closer look: control1 and control2####
hist(control_1_cl_id$skewness_calories, main = "control1", xlab = "skew_cal", ylab = "count")
hist(control_1_cl_id$kurtosis_calories, main = "control1", xlab = "kurtosis_cal", ylab = "count")
hist(control_2_cl_id$skewness_calories, main = "control2", xlab = "skew_cal", ylab = "count")
hist(control_2_cl_id$kurtosis_calories, main = "control2", xlab = "kurtosis_cal", ylab = "count")

#summary
#control 1
  #cal:skewness(mostly between 0 and 2), kurtosis(mostly between 0 to 10, very few big values and the biggest is larger than 60)
  #sugar:skewness(mostly between 0 and 6, concentrating between 0 and 2), kurtosis(mostly between 0 and 25, few big values from 30 to 45)
  #carb:skewness(mostly between 0 and 3, concentrating between 0 and 2, some very large values from 4 to 8), kurtosis(mostly between 0 and 20, very few large values)
  #cholesterol:skewness(mostly between 0 and 4, concentrating between 0 and 2, some very large values from 4 to 8), kurtosis(mostly between 0 and 20, very few large values, and some over 60)
  #protein:skewness(centered around 1 but have some notable negative values between -2 to 0, also some large values around 8), kurtosis(mostly between 0 and 20, very few large values, and some over 60)
  #fat:(mostly between 0 and 3, concentrating between 0 and 2, some very large values from 4 to 8), kurtosis(mostly between 0 and 20, very few large values)
  #sodium:(very different distribution from other nutrients)
    #skewness(flat bell shape around 0 and 4 and a smooth decrease afterwards), kurtosis(highest value between 0 and 10 and then decreasing like a bowl shape afterwards)
  #sat fat:skewness(centered around 1 and mostly between 0 and 2, some uniform distribution after 4), kurtosis(mostly below 5 and some notable values between 5 to 30, more uniform distribution afterwards)
  #fiber:(different distribution from other nutrients)
    #skewness(centered around 1 and mostly between -1 and 3, but notable values between 4 and 8), kurtosis(mostly below 5 and some notable values between 5 to 30 and after 50)
#control 2 (only stated significant differences from control 1)
  #sugar:skewness(more like a bell shape with long right tails), kurtosis(few large values over 60)
  #carb:skewness(more like a tall bell ranging between -2 to 4, and the highest value is around 0.5)
  #cholesterol:skewness(very much like the distribution from control 2 carb), and so does the kurtosis
  #protein: skewness(very much like the distribution from control 2 carb), and so does the kurtosis
  #fat: skewness(very much like the distribution from control 2 carb), and so does the kurtosis
  #sodium: skewness(a thinner bell shape than control 1)
  #sat fat:skewness(similar distribution, with more negative values)
