met1_matrix = matrix( c(468805.3, 468811.9, 468800.2, 468800.6, 468812.0,
                        470341.0, 470467.3, 470346.8, 470325.4, 470315.0,
                        468805.1, 468819.2, 468800.9, 468800.6, 468812.0,
                        470340.7, 470540.0, 470347.4, 470325.4, 470315.0,
                        468805.9, 468828.9, 468803.9, 468800.6, 468812.0,
                        470341.5, 470615.1, 470350.4, 470325.4, 470315.0), nrow=5, ncol=6) 
#rownames: met1_bi, met1_sum, met1_multi, met1_mid, met1_nok
#colnames: AIC_6, BIC_6, AIC_9, BIC_9, AIC_12, BIC_12
rownames(met1_matrix) <- c("met1_bi", "met1_sum", "met1_multi", "met1_mid", "met1_nok")
colnames(met1_matrix) <- c("AIC_6", "BIC_6", "AIC_9", "BIC_9", "AIC_12", "BIC_12")

met2_matrix <- matrix(c(1011451, 1011467, 1011451, 1011448, 1011447,
                        1015487, 1015630, 1015499, 1015473, 1015449,
                        1011447, 1011474, 1011448, 1011448, 1011447,
                        1015484, 1015706, 1015496, 1015473, 1015449,
                        1011449, 1011483, 1011449, 1011451, 1011447,
                        1015486, 1015784, 1015497, 1015476, 1015449),nrow=5, ncol=6)
#rownames: met2_bi, met2_sum, met2_multi, met2_mid, met2_nok
#colnames: AIC_6, BIC_6, AIC_9, BIC_9, AIC_12, BIC_12
rownames(met2_matrix) <- c("met2_bi", "met2_sum", "met2_multi", "met2_mid", "met2_nok")
colnames(met2_matrix) <- c("AIC_6", "BIC_6", "AIC_9", "BIC_9", "AIC_12", "BIC_12")

model_comparison <- rbind.data.frame(met1_matrix, met2_matrix)

model_comparison$anova_nok_6 <- NA
model_comparison$anova_nok_9 <- NA
model_comparison$anova_nok_12 <- NA

model_comparison <- model_comparison[, c("AIC_6", "BIC_6", "anova_nok_6", 
                                         "AIC_9", "BIC_9", "anova_nok_9",
                                         "AIC_12", "BIC_12", "anova_nok_12")]
model_comparison["met1_mid","anova_nok_6"] <- "0.0004555 ***"
model_comparison["met2_bi", "anova_nok_6"] <- 0.1829
model_comparison["met2_mid", "anova_nok_6"] <- 0.1829
model_comparison["met1_mid","anova_nok_9"] <- "0.0004555 ***"
model_comparison["met2_bi", "anova_nok_9"] <- 0.111
model_comparison["met2_mid", "anova_nok_9"] <- 0.1829
model_comparison["met2_multi", "anova_nok_9"] <- 0.114
model_comparison["met1_bi", "anova_nok_12"] <- "0.006854 **"
model_comparison["met1_mid", "anova_nok_12"] <- "0.0004555 ***"
model_comparison["met1_multi", "anova_nok_12"] <- "0.002857 **"
model_comparison["met1_sum", "anova_nok_12"] <- 0.1102
model_comparison["met2_bi", "anova_nok_12"] <- 0.2396
model_comparison["met2_mid", "anova_nok_12"] <- 0.7055
model_comparison["met2_multi", "anova_nok_12"] <- 0.1718
model_comparison["met2_sum", "anova_nok_12"] <- 0.9299
