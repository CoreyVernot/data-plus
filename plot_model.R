#Corey edit 3
#Corey edit 2
#Corey edit 1
#Nathianiel edit
#Nathaniel edit 2
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


plot_model <- function(model, k_vals = as.character(-6:6)){
  library(ggplot2)
  coef <- data.frame(summary(model)$coefficients)
  colnames(coef) <- c("estimate", "SE", "t_value", "p")
  rownames <- paste("k.f", k_vals, sep = "")
  plot_table <- coef[ rownames(coef) %in% rownames, c(1,2)]
  plot_table$k_level <- rownames(plot_table)
  plot_table$k <- plot_table$k_level %>% gsub("k.f", "", .) %>% as.numeric()
  ggplot(plot_table, aes(x= k , y= estimate)) + 
    geom_errorbar(aes(ymin=estimate-1.96*SE, ymax= estimate + 1.96*SE, colour = "error_bar"), width=.3) +
    geom_line() +
    geom_point()
}

m_bmin = 20
m_neg = 20
m_zero = 16
m_pos = 13
m_bmax <- 15

means <- c(rep(m_bmin, 10), rep(m_neg, 12), m_zero, rep(m_pos, 12), rep(m_bmax, 10))
d <- data.frame(k = k_vals, mean = means)
data <- d[sample(1:nrow(d), 1000, replace = T), ]
data$sum_calories = rnorm(mean = data$mean, sd = 7, n = nrow(data))
k.f <- factor(data$k)
levels(k.f) <- c("0", -12:12, "bmin", "bmax")
model <- lm(sum_calories ~ k.f, data = data)

plot_mod(model = model)
