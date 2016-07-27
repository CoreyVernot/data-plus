install.packages('ggplot2')
install.packages('RColorBrewer')
install.packages('ggthemes')

plot_model_mid <- function(model = NA, coef = NA, type = "time", vals = as.character(-6:6), title = "na", mult = 1.645, use_group = T, 
                       group = as.character(-1:3), xlim = NA, ylim = NA){
  library(ggplot2)
  library(RColorBrewer)
  library(ggthemes)
  library(dplyr)
  if(is.na(coef) & is.na(model)){cat("Warning: you have to give either a model or coefficient matrix")}
  if(is.na(coef)){coef <- data.frame(summary(model)$coefficients)}
  colnames(coef) <- c("estimate", "SE", "t_value", "p")
  if(! type %in% c("k", "time")){cat("Warning: type must be either 'k' or 'time")}
  factor <- paste(type, "f", sep = ".") # factor is of format "k.f
  rownames <- paste(factor, vals, sep = "")# rownames is now vector of format 'k.f-12'
  plot_table <- coef[ rownames(coef) %in% rownames, c(1,2)]
  ref <- rownames[!rownames %in% rownames(plot_table)]
  add <- data.frame(estimate = 0, SE = NA)
  rownames(add) <- ref
  plot_table <- rbind(plot_table, add)
  plot_table$level <- rownames(plot_table)
  plot_table$'k_group' <- plot_table$level %>% gsub(factor, "", .)
  
  plot_table$k_group[plot_table$k_group == "-1"] <- "K < -1"
  plot_table$k_group[plot_table$k_group == "0"] <- "-2 < K < 4"
  plot_table$k_group[plot_table$k_group == "1"] <- "K > 3"
  plot_table$k_group <- factor(plot_table$k_group, levels = c("K < -1", "-2 < K < 4", "K > 3"))
    
  
  group_names <- paste(factor, group, sep = "")
  plot_table$group <- plot_table$level %in% group_names
  if(is.na(ylim)){ylim <- c( min(plot_table$estimate) - mult*max(plot_table$SE), 
                             max(plot_table$estimate) + mult*max(plot_table$SE))}
  
  myColors <- brewer.pal(5,"Set1")
  names(myColors) <- levels(plot_table$group)
  colScale <- scale_colour_manual(name = "grp",values = myColors)
  
  
  g <- ggplot(plot_table, aes(x= k_group , y= estimate)) + 
    stat_summary(fun.y=sum, geom="line") +
    geom_point() +
    ggtitle(title)+
    geom_abline(intercept = 0, slope = 0) + 
    ylim(ylim[1], ylim[2]) + 
    theme_tufte() + theme(legend.position = "none", axis.text=element_text(size=13),
                          axis.title=element_text(size=17), title= element_text(size = 18))
  #geom_lab()
  if(use_group){
    g <- g +geom_errorbar(aes(ymin=estimate-mult*SE, ymax= estimate + mult*SE, colour = group), width=.3) +
      colScale
  }else{ 
    g <- g + geom_errorbar(aes(ymin=estimate-mult*SE, ymax= estimate + mult*SE), width=.3) +
      colScale
  }
  
  return(g)
}

plot_model <- function(model = NA, coef = NA, type = "time", vals = as.character(-6:6), title = "na", mult = 1.645, use_group = T, 
                       group = as.character(-1:3), xlim = NA, ylim = NA){
  library(ggplot2)
  library(RColorBrewer)
  library(ggthemes)
  library(dplyr)
  if(is.na(coef) & is.na(model)){cat("Warning: you have to give either a model or coefficient matrix")}
  if(is.na(coef)){coef <- data.frame(summary(model)$coefficients)}
  colnames(coef) <- c("estimate", "SE", "t_value", "p")
  if(! type %in% c("k", "time")){cat("Warning: type must be either 'k' or 'time")}
  factor <- paste(type, "f", sep = ".") # factor is of format "k.f
  rownames <- paste(factor, vals, sep = "")# rownames is now vector of format 'k.f-12'
  plot_table <- coef[ rownames(coef) %in% rownames, c(1,2)]
  ref <- rownames[!rownames %in% rownames(plot_table)]
  add <- data.frame(estimate = 0, SE = NA)
  rownames(add) <- ref
  plot_table <- rbind(plot_table, add)
  plot_table$level <- rownames(plot_table)
  plot_table$k <- plot_table$level %>% gsub(factor, "", .) %>% as.numeric()
  group_names <- paste(factor, group, sep = "")
  plot_table$group <- plot_table$level %in% group_names
  if(is.na(ylim)){ylim <- c( min(plot_table$estimate) - mult*max(plot_table$SE), 
                             max(plot_table$estimate) + mult*max(plot_table$SE))}
  
  myColors <- brewer.pal(5,"Set1")
  names(myColors) <- levels(plot_table$group)
  colScale <- scale_colour_manual(name = "grp",values = myColors)
  
  
  g <- ggplot(plot_table, aes(x= k , y= estimate)) + 
    geom_line() +
    geom_point() +
    ggtitle(title)+
    geom_abline(intercept = 0, slope = 0) + 
    ylim(ylim[1], ylim[2]) + 
    theme_tufte() + theme(legend.position = "none", axis.text=element_text(size=13),
                          axis.title=element_text(size=17), title= element_text(size = 18))
  #ggeom_lab()
  if(use_group){
    g <- g +geom_errorbar(aes(ymin=estimate-mult*SE, ymax= estimate + mult*SE, colour = group), width=.3) +
      colScale
  }else{ 
    g <- g + geom_errorbar(aes(ymin=estimate-mult*SE, ymax= estimate + mult*SE), width=.3) +
      colScale
      }
    
  return(g)
}

load("mods_for_poster_coef.RData")
mods <- mods_for_poster_coef
coef_mid1_cal <- mods$coef_mid1_cal
coef_mid2_cal <- mods$coef_mid2_cal
coef_sum1_cal <- mods$coef_sum1_cal
coef_sum2_cal <- mods$coef_sum2_cal


# Fixing data frames to be set with reference value -6
add <- data.frame(estimate = 0, 'SE' = mean(coef_sum1_cal[grep("k.f", rownames(coef_sum1_cal)), 2]),
                  't_value' = NA, 'p' = NA)
rownames(add) <- "k.f-1"
coef_sum1_cal <- coef_sum1_cal[1:nrow(coef_sum1_cal), c(1:4)]
colnames(coef_sum1_cal) <- c("estimate", "SE", "t_value", "p")
coef_sum1_cal <- rbind(coef_sum1_cal, add)
coef_sum1_cal$estimate <- coef_sum1_cal$estimate - coef_sum1_cal$estimate[rownames(coef_sum1_cal) == "k.f-6"]
coef_sum1_cal <- coef_sum1_cal[ rownames(coef_sum1_cal) != "k.f-6", ]

add <- data.frame(estimate = 0, 'SE' = mean(coef_sum2_cal[grep("k.f", rownames(coef_sum2_cal)), 2]),
                  't_value' = NA, 'p' = NA)
rownames(add) <- "k.f-1"
coef_sum2_cal <- coef_sum2_cal[1:nrow(coef_sum2_cal), c(1:4)]
colnames(coef_sum2_cal) <- c("estimate", "SE", "t_value", "p")
coef_sum2_cal <- rbind(coef_sum2_cal, add)
coef_sum2_cal$estimate <- coef_sum2_cal$estimate - coef_sum2_cal$estimate[rownames(coef_sum2_cal) == "k.f-6"]
coef_sum2_cal <- coef_sum2_cal[ rownames(coef_sum2_cal) != "k.f-6", ]

p1 <- plot_model(coef = coef_mid1_cal, type = "time", vals = as.character(-1:1), title = "Model Coefficients on ", group = "0" )
p2 <- plot_model(coef = coef_mid2_cal, type = "time", vals = as.character(-1:1), title = "Model Coefficients on ", group = "0" )
p3 <- plot_model(coef = coef_sum1_cal, type = "k", vals = as.character(-6:6), title = "Model Coefficients on " )
p4 <- plot_model(coef = coef_sum2_cal, type = "k", vals = as.character(-6:6), title = "Model Coefficients on ")

multiplot(p3, p1, p4, p2, cols = 2)

#### Economist Version
plot_model <- function(model = NA, coef = NA, type = "time", vals = as.character(-6:6), title = "na", mult = 1.645, use_group = T, 
                       group = as.character(-1:3), xlim = NA, ylim = NA){
  library(ggplot2)
  library(RColorBrewer)
  library(ggthemes)
  library(dplyr)
  if(is.na(coef) & is.na(model)){cat("Warning: you have to give either a model or coefficient matrix")}
  if(is.na(coef)){coef <- data.frame(summary(model)$coefficients)}
  colnames(coef) <- c("estimate", "SE", "t_value", "p")
  if(! type %in% c("k", "time")){cat("Warning: type must be either 'k' or 'time")}
  factor <- paste(type, "f", sep = ".") # factor is of format "k.f
  rownames <- paste(factor, vals, sep = "")# rownames is now vector of format 'k.f-12'
  plot_table <- coef[ rownames(coef) %in% rownames, c(1,2)]
  ref <- rownames[!rownames %in% rownames(plot_table)]
  add <- data.frame(estimate = 0, SE = NA)
  rownames(add) <- ref
  plot_table <- rbind(plot_table, add)
  plot_table$level <- rownames(plot_table)
  plot_table$k <- plot_table$level %>% gsub(factor, "", .) %>% as.numeric()
  group_names <- paste(factor, group, sep = "")
  plot_table$group <- plot_table$level %in% group_names
  if(is.na(ylim)){ylim <- c( min(plot_table$estimate) - mult*max(plot_table$SE), 
                             max(plot_table$estimate) + mult*max(plot_table$SE))}
  
  myColors <- brewer.pal(5,"Set1")
  names(myColors) <- levels(plot_table$group)
  colScale <- scale_colour_manual(name = "grp",values = myColors)
  
  
  g <- ggplot(plot_table, aes(x= k , y= estimate)) + 
    geom_line() +
    geom_point() +
    ggtitle(title)+
    geom_abline(intercept = 0, slope = 0) + 
    ylim(ylim[1], ylim[2]) + 
    theme_economist() + theme(legend.position = "none")
  #geom_lab()
  if(use_group){
    g <- g +geom_errorbar(aes(ymin=estimate-mult*SE, ymax= estimate + mult*SE, colour = group), width=.3) +
      colScale
  }else{ 
    g <- g + geom_errorbar(aes(ymin=estimate-mult*SE, ymax= estimate + mult*SE), width=.3) +
      colScale
  }
  
  return(g)
}

p1 <- plot_model_mid(coef = coef_mid1_cal, type = "time", vals = as.character(-1:1), 
                     title = "", group = "0" )
p2 <- plot_model_mid(coef = coef_mid2_cal, type = "time", vals = as.character(-1:1), 
                     title = "", group = "0" )
p3 <- plot_model(coef = coef_sum1_cal, type = "k", vals = as.character(-6:6),
                 title = "Single Person Housholds") # Corey Start Here
p4 <- plot_model(coef = coef_sum2_cal, type = "k", vals = as.character(-6:6), 
                 title = "2 Person Households")

multiplot(p3, p1, p4, p2, cols = 2)

p1 + stat_summary(fun.y = mean, geom="line")
