#*****************MODELS GO HERE*************
models <- list()
#*******************************************


####################################      METFORMIN      ##################################

brands <- c("Fortamet", "Glucophage", "Glumetza", "Riomet", "Obimet",
            "Dianben", "Diaformin", "Siofor", "Metfogamma", "Janumet",
            "Kazano", "Invokamet", "Xigduo", "Synjardy", "Metaglip" ,
            "Jentaduo" , "Actoplus", "Prandimet", "Avandamet", "Kombiglyze", 
            "Glumetza", "Metformin")

##### HHSIZE = 1 #####
panelids <- getPanelIDs(brands, rx = rx, demo = demo, new = T)
ids <- panelids$IDs[[1]]
Brands <- panelids$Brands[[1]]
setwd("Z:/InternWorkspace/Data")
trans <- read.csv("newMetforminTrans.csv")
trans_new <- trans[trans$panelid %in% ids, ]
trans_met_1_cl <- cleanData(trans_new, rx = rx, brands = Brands)
control_cl <- read.csv("trans_control_1_cl.csv")
Trans_met_1_cl <- combine_control(trans_met_1_cl, control_cl)


#types of models- matt = matts  sam's original model, res = residualized model w/ k values, 

models$met_1_matt_carb <- sum_model(Trans_met_1_cl)
models$met_1_matt_cal <- sum_model(Trans_met_1_cl, nutrient = "calories")
models$met_1_matt_sugar <- sum_model(Trans_met_1_cl, nutrient = "sugar")
models$met_1_matt_fat <- sum_model(Trans_met_1_cl, nutrient = "fat")

##### HHSIZE = 2 ####
trans2 <- read.csv("met2_trans.csv")
panelids <- getPanelIDs(brands, rx = rx, demo = demo, new = T, HHSizes = 2)
ids <- panelids$IDs[[1]]
Brands <- panelids$Brands[[1]]
trans2_new <- trans2[trans2$panelid %in% ids, ]
trans_met_2_cl <- cleanData(trans2_new, rx = rx, brands = Brands)
control2_cl <- read.csv("trans_control_2_cl.csv")
Trans_met_2_cl <- combine_control(trans_met_2_cl, control2_cl)

models$met_2_matt_carb <- sum_model(Trans_met_2_cl)
models$met_2_matt_cal <- sum_model(Trans_met_2_cl, nutrient = "calories")
models$met_2_matt_sugar <- sum_model(Trans_met_2_cl, nutrient = "sugar")
models$met_2_matt_fat <- sum_model(Trans_met_2_cl, nutrient = "fat")

##### HHSize = 1, residualized ####
trans_met_1_cl_resid <- residualize(trans_met_1_cl)
models$met_1_resid_carb <- sum_resid_model(trans_met_1_cl_resid)
models$met_1_resid_cal <- sum_resid_model(trans_met_1_cl_resid, nutrient = "calories")
models$met_1_resid_sugar <- sum_resid_model(trans_met_1_cl_resid, nutrient = "sugar")
models$met_1_resid_fat <- sum_resid_model(trans_met_1_cl_resid, nutrient = "fat")

##### HHSize = 2, residualized ####
trans_met_2_cl_resid <- residualize(trans_met_2_cl, hhsize = 2)
models$met_2_resid_carb <- sum_resid_model(trans_met_2_cl_resid)
models$met_2_resid_cal <- sum_resid_model(trans_met_2_cl_resid, nutrient = "calories")
models$met_2_resid_sugar <- sum_resid_model(trans_met_2_cl_resid, nutrient = "sugar")
models$met_2_resid_fat <- sum_resid_model(trans_met_2_cl_resid, nutrient = "fat")


#### Taking ####
rx_met <- impute("Z:/InternWorkspace/Predicting Metformin", "rxdays_random_forest.RData", rx = rx, brands = Brands)
panelids <- getPanelIDs(brands, rx = rx, demo = demo, new = F)
ids_met_1_all <- panelids$IDs[[1]]
Brands <- panelids$Brands[[1]]
taking <- cleanData_impute(rx = rx_met, panelids = ids_met_1_all, Brands = Brands, after_first_rx = T)

trans_met_1_cl_all <- cleanData(trans, rx = rx, brands = Brands)


####### More Stuff #######

par(mfrow = c(2,3))
graphData(nutrient = "carb", data = trans_sum, k_range = c(-12, 12),  all_k_only = F, main = "Carbohydrates vs Prescription Date", ylab = "Grams Carbohydrates")
graphData(nutrient = "sugar", data = trans_sum, k_range = c(-12, 12),  all_k_only = F, main = "Sugar vs Prescription Date", ylab = "Grams Sugar")
graphData(nutrient = "calories", data = trans_sum, k_range = c(-12, 12),  all_k_only = F, main = "Calories vs Prescription Date", ylab = "Calories")
graphData(nutrient = "fat", data = trans_sum, k_range = c(-12, 12),  all_k_only = F, main = "Fat vs Prescription Date", ylab = "Grams Fat")


graphData(nutrient = "carb", data = trans_sum, k_range = c(-12, 12),  all_k_only = T)
graphData(nutrient = "sugar", data = trans_sum, k_range = c(-12, 12),  all_k_only = T)
graphData(nutrient = "calories", data = trans_sum, k_range = c(-12, 12),  all_k_only = T)

graphData_per("sugar", data = trans_cl, k_range = c(-12, 12),  all_k_only = F, main = "Sugar/Calorie vs Prescription Date", ylab = "Grams Sugar/Calorie")

carb = sam_model(data = trans_sum, nutrient = "carb", k_range = c(-6, 6), sdcut = 2)
carb_coef <- round(summary(carb)$coefficients, 4)

hist(carb_coef[2:14,1], xlim = c(-1, 1))
hist(carb_coef[15:115,1], xlim = c(-1, 1))
hist(carb_coef[116:179,1], xlim = c(-1, 1))

sugar <- sam_model(data = trans_sum, nutrient = "carb", k_range = c(-6, 6), sdcut = 2)
sugar_coef <- round(summary(sugar)$coefficients, 4)

carb12 <- sam_model(data = trans_sum, k_range = c(-12, 12))
coef12 <- round(summary(carb12)$coefficients, 4)

plot(number_pid$n ~ number_pid$n, ylim = c(0, 700), main = "Number of PanelIDS by Timeunit")

colnames(trans_sum)

trans_sum_k <- trans_sum %>% group_by(k_zero_timeunit, k, timeunit) %>% summarise(sum_carb = mean(sum_carb))
trans_sum_k <- trans_sum_k[trans_sum_k$k %in% seq(-12, 12), ]

g <- ggplot(data = trans_sum_k, aes(x = timeunit, y = sum_carb, colour = k_zero_timeunit) )

g + geom_line()


####################################      Beta-Blockers      ##################################
bb_models <- list()
brands <- c("Acebutolol Hcl"           ,       "Acebutolol"      ,                "Atenolol"                   ,    
        "Atenolol-Chlorthalidone"       ,  "betapace af"            ,         "betapace"                       ,
        "Betaxolol hcl"                  , "betaxolol"                ,       "bisoprolol fumarate"            ,
        "bisoprolol-hydrochlorothiazide",  "carvedilol"                ,      "carteol"                        ,
        "coreg"                        ,   "coreg cr"                    ,    "corgard"                        ,
        "dorzolamide-timolol"          ,   "inderal la"                   ,   "inderal"                        ,
        "innopran xl"                  ,   "labetalol hcl"               ,    "levatol"                        ,
        "lopressor"                    ,   "lopressor hct"               ,    "metroporolol succinate"         ,
        "metroprolol tartate"          ,   "metroprolol"                 ,    "metroprolol-hydrochlorothiazide",
        "nadolol"                      ,   "nadolol-bendroflumethiazide" ,    "pindolol"                       ,
        "sectral"                      ,   "sotalol"                     ,    "sotalol af"                     ,
        "tenoretic 50"                 ,   "tenoretic"                   ,    "tenormin"                       ,
        "timolol maleate"               ,  "timolol"                      ,   "toprol xl"                      ,
        "toprol"                       ,   "ziac")

##### HHSIZE = 1 #####
panelids <- getPanelIDs(brands, rx = rx, demo = demo, new = T)
ids <- panelids$IDs[[1]]
Brands <- panelids$Brands[[1]]
setwd("Z:/InternWorkspace/Data")
trans <- read.csv("bb1_trans.csv")
trans_new <- trans[trans$panelid %in% ids, ]
trans_bb_1_cl <- cleanData(trans_new, rx = rx, brands = Brands)
control_cl <- read.csv("trans_control_1_cl.csv")
Trans_bb_1_cl <- combine_control(trans_bb_1_cl, control_cl)


#types of models- matt = matts  sam's original model, res = residualized model w/ k values, 

bb_models$bb_1_matt_carb <- sum_model(Trans_bb_1_cl)
bb_models$bb_1_matt_cal <- sum_model(Trans_bb_1_cl, nutrient = "calories")
bb_models$bb_1_matt_sugar <- sum_model(Trans_bb_1_cl, nutrient = "sugar")
bb_models$bb_1_matt_fat <- sum_model(Trans_bb_1_cl, nutrient = "fat")

bb_models$bb_1_matt_sodium <- sum_model(Trans_bb_1_cl, nutrient = "sodium")
bb_models$bb_1_matt_chol <- sum_model(Trans_bb_1_cl, nutrient = "cholesterol")

##### HHSIZE = 2 ####
trans2 <- read.csv("bb2_trans.csv")
panelids <- getPanelIDs(brands, rx = rx, demo = demo, new = T, HHSizes = 2)
ids <- panelids$IDs[[1]]
Brands <- panelids$Brands[[1]]
trans2_new <- trans2[trans2$panelid %in% ids, ]
trans_bb_2_cl <- cleanData(trans2_new, rx = rx, brands = Brands)
control2_cl <- read.csv("trans_control_2_cl.csv")
Trans_bb_2_cl <- combine_control(trans_bb_2_cl, control2_cl)

bb_models$bb_2_matt_carb <- sum_model(Trans_bb_2_cl)
bb_models$bb_2_matt_cal <- sum_model(Trans_bb_2_cl, nutrient = "calories")
bb_models$bb_2_matt_sugar <- sum_model(Trans_bb_2_cl, nutrient = "sugar")
bb_models$bb_2_matt_fat <- sum_model(Trans_bb_2_cl, nutrient = "fat")

bb_models$bb_2_matt_sodium <- sum_model(Trans_bb_2_cl, nutrient = "sodium")
bb_models$bb_2_matt_chol <- sum_model(Trans_bb_2_cl, nutrient = "cholesterol")

##### HHSize = 1, residualized ####
trans_bb_1_cl_resid <- residualize(trans_bb_1_cl)
bb_models$bb_1_resid_carb <- sum_resid_model(trans_bb_1_cl_resid)
bb_models$bb_1_resid_cal <- sum_resid_model(trans_bb_1_cl_resid, nutrient = "calories")
bb_models$bb_1_resid_sugar <- sum_resid_model(trans_bb_1_cl_resid, nutrient = "sugar")
bb_models$bb_1_resid_fat <- sum_resid_model(trans_bb_1_cl_resid, nutrient = "fat")
bb_models$bb_1_resid_sodium <- sum_resid_model(trans_bb_1_cl_resid, nutrient = "sodium")
bb_models$bb_1_resid_chol <- sum_resid_model(trans_bb_1_cl_resid, nutrient = "cholesterol")

##### HHSize = 2, residualized ####
trans_bb_2_cl_resid <- residualize(trans_bb_2_cl, hhsize = 2)
bb_models$bb_2_resid_carb <- sum_resid_model(trans_bb_2_cl_resid)
bb_models$bb_2_resid_cal <- sum_resid_model(trans_bb_2_cl_resid, nutrient = "calories")
bb_models$bb_2_resid_sugar <- sum_resid_model(trans_bb_2_cl_resid, nutrient = "sugar")
bb_models$bb_2_resid_fat <- sum_resid_model(trans_bb_2_cl_resid, nutrient = "fat")
bb_models$bb_2_resid_sodium <- sum_resid_model(trans_bb_2_cl_resid, nutrient = "sodium")
bb_models$bb_2_resid_chol <- sum_resid_model(trans_bb_2_cl_resid, nutrient = "cholesterol")

save(bb_models, file="bb_models.Rdata")



####################################      Depression      ##################################
dep_models <- list()
brandSSRI <- c("Zoloft", "Prozac", "Celexa", "Lexapro", "Paxil", "Pexeva",
               "Luvox", "Oleptro", "Luvox CR", "Paxil CR", "Cipralex", "Sarafem",
               "Prozac Weekly", "Brisdelle", "Selfemra", "Rapiflux", "Citalopram", 
               "Sertraline", "Fluoxetine", "Escitalopram", "Paroxetine", "Fluvoxamine",
               "Trazodone", "Fluvoxamine CR", "Paroxetine CR", "Citalopram HBR",
               "Sertraline (ALL)", "Paroxetine (All)", "Prozac (All)", "Paxil (All)",
               "Clonazepam")

#set of drugs that has SNRIs(Serotonin and Norepinephrine Reuptake Inhibitors) in them
brandSNRI <- c("Pristiq", "Cymbalta", "Effexor", "Effexor XR", "Savella", "Fetzima",
               "Desvenlafaxine", "Duloxetine", "Venlafaxine", "Venlafaxine XR",
               "Milnacipran", "Levomilnacipran", "Effexor (All)")


#set of drugs that has TCAs(Tricyclic Antidepressants) in them
brandTCA <- c("Elavil", "Norpramin", "Sinequan", "Tofranil", "Pamelor", 
              "Anafranil", "Ludiomil", "Surmontil", "Vivactil", "Endep",
              "Levate", "Asendin", "Pertofrane", "Prothiaden", "Thaden",
              "Adapin", "Sinequan", "Gamanil", "Lomont", "Deprilept",
              "Ludiomil", "Psymion", "Bolvidon", "Norval", "Tolvan",
              "Pamelor", "Amitriptyline", "Desipramine", "Doxepine",
              "Imipramine", "Nortriptyline", "Amoxapine", "Clomipramine",
              "Maprotiline", "Trimipramine", "Protriptyline", "Nortriptyline (All)")


##### HHSIZE = 1 #####
panelidsSSRI <- getPanelIDs(brandSSRI, rx = rx, demo = demo, new = T)
panelidsSNRI <- getPanelIDs(brandSNRI, rx = rx, demo = demo, new = T)
panelidsTCA <- getPanelIDs(brandTCA, rx = rx, demo = demo, new = T)
idsSSRI <- panelidsSSRI$IDs[[1]]
idsSNRI <- panelidsSNRI$IDs[[1]]
idsTCA <- panelidsTCA$IDs[[1]]
BrandSSRI <- panelidsSSRI$Brands[[1]]
BrandSNRI <- panelidsSNRI$Brands[[1]]
BrandTCA <- panelidsTCA$Brands[[1]]

setwd("Z:/InternWorkspace/Data")
trans <- read.csv("trans_depression_1.csv")
trans_newSSRI <- trans[trans$panelid %in% idsSSRI, ]
trans_newSNRI <- trans[trans$panelid %in% idsSNRI, ]
trans_newTCA <- trans[trans$panelid %in% idsTCA, ]
trans_ssri_1_cl <- cleanData(trans_newSSRI, rx = rx, brands = BrandSSRI)
trans_snri_1_cl <- cleanData(trans_newSNRI, rx = rx, brands = BrandSNRI)
trans_tca_1_cl <- cleanData(trans_newTCA, rx = rx, brands = BrandTCA)
control_cl <- read.csv("trans_control_1_cl.csv")
Trans_ssri_1_cl <- combine_control(trans_ssri_1_cl, control_cl)
Trans_snri_1_cl <- combine_control(trans_snri_1_cl, control_cl)
Trans_tca_1_cl <- combine_control(trans_tca_1_cl, control_cl)

#types of models- matt = matts  sam's original model, res = residualized model w/ k values, 

dep_models$ssri_1_matt_carb <- sum_model(Trans_ssri_1_cl)
dep_models$ssri_1_matt_cal <- sum_model(Trans_ssri_1_cl, nutrient = "calories")
dep_models$ssri_1_matt_sugar <- sum_model(Trans_ssri_1_cl, nutrient = "sugar")
dep_models$ssri_1_matt_fat <- sum_model(Trans_ssri_1_cl, nutrient = "fat")

dep_models$snri_1_matt_carb <- sum_model(Trans_snri_1_cl)
dep_models$snri_1_matt_cal <- sum_model(Trans_snri_1_cl, nutrient = "calories")
dep_models$snri_1_matt_sugar <- sum_model(Trans_snri_1_cl, nutrient = "sugar")
dep_models$snri_1_matt_fat <- sum_model(Trans_snri_1_cl, nutrient = "fat")

dep_models$tca_1_matt_carb <- sum_model(Trans_tca_1_cl)
dep_models$tca_1_matt_cal <- sum_model(Trans_tca_1_cl, nutrient = "calories")
dep_models$tca_1_matt_sugar <- sum_model(Trans_tca_1_cl, nutrient = "sugar")
dep_models$tca_1_matt_fat <- sum_model(Trans_tca_1_cl, nutrient = "fat")
##### HHSIZE = 2 #### 
#no hhsize2 trans info for depression


##### HHSize = 1, residualized ####

trans_ssri_1_cl_resid <- residualize(trans_ssri_1_cl)
trans_snri_1_cl_resid <- residualize(trans_snri_1_cl)
trans_tca_1_cl_resid <- residualize(trans_tca_1_cl)

dep_models$ssri_1_resid_carb <- sum_resid_model(trans_ssri_1_cl_resid)
dep_models$ssri_1_resid_cal <- sum_resid_model(trans_ssri_1_cl_resid, nutrient = "calories")
dep_models$ssri_1_resid_sugar <- sum_resid_model(trans_ssri_1_cl_resid, nutrient = "sugar")
dep_models$ssri_1_resid_fat <- sum_resid_model(trans_ssri_1_cl_resid, nutrient = "fat")

dep_models$snri_1_resid_carb <- sum_resid_model(trans_snri_1_cl_resid)
dep_models$snri_1_resid_cal <- sum_resid_model(trans_snri_1_cl_resid, nutrient = "calories")
dep_models$snri_1_resid_sugar <- sum_resid_model(trans_snri_1_cl_resid, nutrient = "sugar")
dep_models$snri_1_resid_fat <- sum_resid_model(trans_snri_1_cl_resid, nutrient = "fat")

dep_models$tca_1_resid_carb <- sum_resid_model(trans_tca_1_cl_resid)
dep_models$tca_1_resid_cal <- sum_resid_model(trans_tca_1_cl_resid, nutrient = "calories")
dep_models$tca_1_resid_sugar <- sum_resid_model(trans_tca_1_cl_resid, nutrient = "sugar")
dep_models$tca_1_resid_fat <- sum_resid_model(trans_tca_1_cl_resid, nutrient = "fat")
##### HHSize = 2, residualized ####

save(dep_models, file="dep_models.Rdata")

