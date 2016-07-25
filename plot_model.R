#Corey edit 4
#Corey edit 3
#Corey edit 2
#Corey edit 1
#Nathianiel edit
#Nathaniel edit 2
#GuanWun edit
#GuanWun edit 2
#NATHANIEL FINALLY LEARNED HOW TO USE GITHUB (somewhat...)
data(mtcars)
colnames(mtcars)

class(mtcars$gear)
length(unique(mtcars$hp))

k_vals <- c(rep("bmin", 10 ), -12:12, rep("bmax", 10))
means <- c(rep(2, 10), rep(1.75, 12), 1, rep(0.5, 12), rep(1.5, 10))
d <- data.frame(k = k_vals, mean = means)
data <- d[sample(1:nrow(d), 100000, replace = T), ]
data$sum_calories = rnorm(data$mean)

k.f <- factor(data$k)
model <- lm(sum_calories ~ k.f, data = data)


plot_model <- function(model, k_vals = as.character(-6:6), title = "na", mult = 1.645, use_group = F, group = as.character(-1:3)){
  library(ggplot2)
  coef <- data.frame(summary(model)$coefficients)
  colnames(coef) <- c("estimate", "SE", "t_value", "p")
  rownames <- paste("k.f", k_vals, sep = "")
  plot_table <- coef[ rownames(coef) %in% rownames, c(1,2)]
  k_ref <- rownames[!rownames %in% rownames(plot_table)]
  add <- data.frame(estimate = 0, SE = NA)
  rownames(add) <- k_ref
  plot_table <- rbind(plot_table, add)
  plot_table$k_level <- rownames(plot_table)
  plot_table$k <- plot_table$k_level %>% gsub("k.f", "", .) %>% as.numeric()
  group_names <- paste("k.f", group, sep = "")
  plot_table$group <- plot_table$k_level %in% group_names
  g <- ggplot(plot_table, aes(x= k , y= estimate)) + 
    geom_line() +
    geom_point() +
    ggtitle(title)+
    geom_abline(intercept = 0, slope = 0)
  if(use_group){
    g <- g +geom_errorbar(aes(ymin=estimate-mult*SE, ymax= estimate + mult*SE, colour = group), width=.3)
  }else{ g <- g + geom_errorbar(aes(ymin=estimate-mult*SE, ymax= estimate + mult*SE), width=.3)}
    
  return(g)
}



m_bmin = 20
m_neg = 20
m_zero = 16
m_pos = 13
m_bmax <- 15

k_vals <- c(rep("bmin", 10 ), -12:12, rep("bmax", 10))
means <- c(rep(m_bmin, 10), rep(m_neg, 12), m_zero, rep(m_pos, 12), rep(m_bmax, 10))
d <- data.frame(k = k_vals, mean = means)
data <- d[sample(1:nrow(d), 2000, replace = T), ]
data$sum_calories = rnorm(mean = data$mean, sd = 7, n = nrow(data))
k.f <- factor(data$k)
levels(k.f) <- c("0", -12:12, "bmin", "bmax")
model <- lm(sum_calories ~ k.f, data = data)

plot_model(model = model)
