
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



# calculate the probability of actual taking in the study period####
#condition 1:refill after the end of prescription
start <- 1
end <- 30
refill <- 35
ps <- 0.5 #the probability that the person misses the drug condition on having the drug at the time
p <- 0 #the probability that the person has the supply at the time
#timeunit 1:29
pt = 1- ps #the probability that the person takes the drug at the time
#timeunit 30
pt = (1-ps)*p = (1-ps)*(1-dbinom(0, size = 29, prob = ps))
#timeunit 31
pt = (1-ps)*(1 - pbinom(1, size = 29, prob = ps) + dbinom(1, size = 29, prob = ps)*ps)
#timeunit 32

#timeunit 33

#timeunit 34 (5 days after end date)
pt = (1-ps)*(pbinom())




L <- 29 #Rx Days
R <- 34 #NUmber of days before the next refill
ph <- function(j,L,ps){ #j = days after the end of prescription, ps = probability of missing the drug given having the prescription
  i <- 0:(j-1)
  sum_terms <- (dbinom(j-1-i, L, ps)*(1-pbinom(i, j - 1, ps)))
  ph <- (1-pbinom(j-1, L, ps) + sum(sum_terms))
  return(ph)
}


ph(5, 29, .05)

#condition 2:refill before the end of prescription




