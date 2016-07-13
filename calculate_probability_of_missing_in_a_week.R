
a <- 4
pcalc = function(days_missed, a){
  #b <- a*log(max(days_missed),base = 8)
  b <- a*max(days_missed)/7
  for(i in 1:length(days_missed)){w[i] = (1/(1+days_missed[i]))^b}
  probs <- c()
  for(j in 1:length(days_missed)){probs[j] = w[j]/sum(w)}
  names(probs) = 1:7
  return(probs)
}

numday = 
missed = c(numday, numday, numday -1:5)
pcalc(missed, a = 3)
s = sum(c(1/21, 1/21, 1/20, 1/19, 1/18, 1/17, 1/16))
(1/16)/s




b <- take_diabetes[take_diabetes$taking_met == 1, ]
c <- b%>% group_by(new_id) %>% summarize(min = min(day), max = max(day))
d <- merge(take_diabetes,c, by = "new_id")
d$missed <- d$day < d$max & d$day > d$min & d$taking_met == 0
e <- d %>% group_by(new_id) %>% summarize(missed = sum(missed))
hist(e$missed[e$missed < 20])
abline(v = mean(e$missed))
abline(v = median(e$missed))
