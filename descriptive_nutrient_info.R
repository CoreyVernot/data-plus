##descriptive info

####load data####
rx <- read.csv("rx.csv")
demo <- read.csv("demo.csv")

brandSSRI <- c("Zoloft", "Prozac", "Celexa", "Lexapro", "Paxil", "Pexeva",
               "Luvox", "Oleptro", "Luvox CR", "Paxil CR", "Cipralex", "Sarafem",
               "Prozac Weekly", "Brisdelle", "Selfemra", "Rapiflux", "Citalopram", 
               "Sertraline", "Fluoxetine", "Escitalopram", "Paroxetine", "Fluvoxamine",
               "Trazodone", "Fluvoxamine CR", "Paroxetine CR", "Citalopram HBR",
               "Sertraline (ALL)", "Paroxetine (All)", "Prozac (All)", "Paxil (All)",
               "Clonazepam")

brandSNRI <- c("Pristiq", "Cymbalta", "Effexor", "Effexor XR", "Savella", "Fetzima",
               "Desvenlafaxine", "Duloxetine", "Venlafaxine", "Venlafaxine XR",
               "Milnacipran", "Levomilnacipran", "Effexor (All)")

brandTCA <- c("Elavil", "Norpramin", "Sinequan", "Tofranil", "Pamelor", 
              "Anafranil", "Ludiomil", "Surmontil", "Vivactil", "Endep",
              "Levate", "Asendin", "Pertofrane", "Prothiaden", "Thaden",
              "Adapin", "Sinequan", "Gamanil", "Lomont", "Deprilept",
              "Ludiomil", "Psymion", "Bolvidon", "Norval", "Tolvan",
              "Pamelor", "Amitriptyline", "Desipramine", "Doxepine",
              "Imipramine", "Nortriptyline", "Amoxapine", "Clomipramine",
              "Maprotiline", "Trimipramine", "Protriptyline", "Nortriptyline (All)")

brandMet <- c("Fortamet", "Glucophage", "Glumetza", "Riomet", "Obimet",
            "Dianben", "Diaformin", "Siofor", "Metfogamma", "Janumet",
            "Kazano", "Invokamet", "Xigduo", "Synjardy", "Metaglip" ,
            "Jentaduo" , "Actoplus", "Prandimet", "Avandamet", "Kombiglyze", 
            "Glumetza", "Metformin")

brandBB <- c("Acebutolol Hcl"           ,       "Acebutolol"      ,                "Atenolol"                   ,    
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


#metformin, beta blocker, SSRI, SNRI, TCA
bb1_trans <- read.csv("bb1_trans.csv")
bb2_trans <- read.csv("bb2_trans.csv")
met1_trans <- read.csv("newMetforminTrans.csv")
met2_trans <- read.csv("met2_trans.csv")
depression_trans <- read.csv("trans_depression_1.csv")

bb1_panelids <- getPanelIDs(brandBB, rx = rx, demo = demo, new = T)
bb1_ids <- bb1_panelids$IDs[[1]]
bb1_Brands <- bb1_panelids$Brands[[1]]
bb1_trans_new <- bb1_trans[bb1_trans$panelid %in% bb1_ids, ]
bb2_panelids <- getPanelIDs(brandBB, rx = rx, demo = demo, new = T, HHSizes = 2)
bb2_ids <- bb2_panelids$IDs[[1]]
bb2_Brands <- bb2_panelids$Brands[[1]]
bb2_trans_new <- bb2_trans[bb2_trans$panelid %in% bb2_ids, ]

met1_panelids <- getPanelIDs(brandMet, rx = rx, demo = demo, new = T)
met1_ids <- met1_panelids$IDs[[1]]
met1_Brands <- met1_panelids$Brands[[1]]
met1_trans_new <- met1_trans[met1_trans$panelid %in% met1_ids, ]
met2_panelids <- getPanelIDs(brandMet, rx = rx, demo = demo, new = T, HHSizes = 2)
met2_ids <- met2_panelids$IDs[[1]]
met2_Brands <- met2_panelids$Brands[[1]]
met2_trans_new <- met2_trans[met2_trans$panelid %in% met2_ids, ]

SSRI_panelids <- getPanelIDs(brandSSRI, rx = rx, demo = demo, new = T)
SNRI_panelids <- getPanelIDs(brandSNRI, rx = rx, demo = demo, new = T)
TCA_panelids <- getPanelIDs(brandTCA, rx = rx, demo = demo, new = T)
SSRI_ids <- SSRI_panelids$IDs[[1]]
SNRI_ids <- SNRI_panelids$IDs[[1]]
TCA_ids <- TCA_panelids$IDs[[1]]
SSRI_Brand <- SSRI_panelids$Brands[[1]]
SNRI_Brand <- SNRI_panelids$Brands[[1]]
TCA_Brand <- TCA_panelids$Brands[[1]]
SSRI_trans_new <- depression_trans[depression_trans$panelid %in% SSRI_ids, ]
SNRI_trans_new <- depression_trans[depression_trans$panelid %in% SNRI_ids, ]
TCA_trans_new <- depression_trans[depression_trans$panelid %in% TCA_ids, ]


bb1_trans_clean <- cleanData(bb1_trans_new, rx=rx, brands = bb_Brands)
bb2_trans_clean <- cleanData(bb2_trans_new, rx=rx, brands = bb_Brands)
met1_trans_clean <- cleanData(met1_trans_new, rx=rx, brands = met_Brands)
met2_trans_clean <- cleanData(met2_trans_new, rx=rx, brands = met_Brands)
SSRI_trans_clean <- cleanData(SSRI_trans_new, rx=rx, brands = SSRI_Brand)
SNRI_trans_clean <- cleanData(SNRI_trans_new, rx=rx, brands = SNRI_Brand)
TCA_trans_clean <- cleanData(TCA_trans_new, rx=rx, brands = TCA_Brand)

####summary description####
nut_descr_id <- function(clean_trans, id_name = "panelid",
                      nutrient = c("calories", "sugar", "carb", "cholesterol", "protein", "fat", "sodium", "sat_fat", "fiber")){
  do_mean <- rep(NA, length(nutrient))
  do_variance <- do_skewness <- do_kurtosis <- do_mean
  for(i in 1:length(nutrient)){
    sum <- paste("sum_", nutrient[i], sep = "")
    mean <- paste("mean_", nutrient[i], sep = "")
    variance <- paste("var_", nutrient[i], sep = "")
    skewness <- paste("skewness_", nutrient[i], sep = "")
    kurtosis <- paste("kurtosis_", nutrient[i], sep = "")
    
    do_mean[i] <- paste(mean," = ", "mean(", sum, ", na.rm = T)", sep = "")
    do_variance[i] <- paste(variance, " = ", "var(", sum, ", na.rm = T)", sep = "")
    do_skewness[i] <- paste(skewness, " = ", "skewness(", sum, ", na.rm = T)", sep = "")
    do_kurtosis[i] <- paste(kurtosis, " = ", "kurtosis(", sum, ", na.rm = T)", sep = "")
  }
  
  do_all <- c(do_mean, do_variance, do_skewness, do_kurtosis)
  do_all_sum <- paste(do_all, collapse = " , ")
  do <- paste("description <- clean_trans %>% group_by(", id_name, ") %>% summarise(", do_all_sum, ")", sep = "")
  eval(parse(text = do))

  return(description)
}

nut_descr_time <- function(clean_trans,
                         nutrient = c("calories", "sugar", "carb", "cholesterol", "protein", "fat", "sodium", "sat_fat", "fiber")){
  do_mean <- rep(NA, length(nutrient))
  do_variance <- do_skewness <- do_kurtosis <- do_mean
  for(i in 1:length(nutrient)){
    sum <- paste("sum_", nutrient[i], sep = "")
    mean <- paste("mean_", nutrient[i], sep = "")
    variance <- paste("var_", nutrient[i], sep = "")
    skewness <- paste("skewness_", nutrient[i], sep = "")
    kurtosis <- paste("kurtosis_", nutrient[i], sep = "")
    
    do_mean[i] <- paste(mean," = ", "mean(", sum, ", na.rm = T)", sep = "")
    do_variance[i] <- paste(variance, " = ", "var(", sum, ", na.rm = T)", sep = "")
    do_skewness[i] <- paste(skewness, " = ", "skewness(", sum, ", na.rm = T)", sep = "")
    do_kurtosis[i] <- paste(kurtosis, " = ", "kurtosis(", sum, ", na.rm = T)", sep = "")
  }
  
  do_all <- c(do_mean, do_variance, do_skewness, do_kurtosis)
  do_all_sum <- paste(do_all, collapse = " , ")
  do <- paste("description <- clean_trans %>% group_by(timeunit) %>% summarise(", do_all_sum, ")", sep = "")
  eval(parse(text = do))
  
  return(description)
}

desc_nut_all <- function(nutrients =c("calories", "sugar", "carb", "cholesterol", "protein", "fat", "sodium", "sat_fat", "fiber"), 
                         data){
  desc <- list()
  for( j in 1:length(nutrients)){
    do <- paste("desc$", nutrients[j], "<- desc_nut(nutrient = '", nutrient, "' , data = data)", sep = "")
    eval(parse(text = do))
  }
  return(desc)
}


desc_nut <- function(nutrient, data){
  desc <- list()
  tu <- unique(data$timeunit)
  for(i in 1:length(tu)){
    d_name <- paste("desc$tu_", tu[i], sep = "")
    command <- paste("tu_quant(", tu[i], ", nutrient = '", nutrient, "', data = data)", sep = "" )
    do <- paste(d_name, "<-", command, sep = "")
    eval(parse(text = do))
  }
  return(desc)
}

tu_quant <- function(timeunit, nutrient, data){
  data_use <- data[data$timeunit == timeunit, ]
  n = length(unique(data$panelid))
  variable <- paste("sum", nutrient, sep = "_")
  do <- paste("quant <- quantile(data$", variable, ", probs = seq(0, 1, .05), type = 5)")
  eval(parse(text = do))
  return(list(quantile = quant, n = n))
}









quantile(a, probs = seq(0,1, 0.05), type = 5)
####distribution description####
percent_descr_time <- function(clean_trans, 
                          nutrient = c("calories", "sugar", "carb", "cholesterol", "protein", "fat", "sodium", "sat_fat", "fiber")){
  descr <- as.list(rep(NA, length(nutrient)))
  for(j in 1:length(nutrient)){
    
    descr <- as.list(rep(NA, length(unique(clean_trans$timeunit)) ))
    for(i in 1:length(descr))
  }
}

control_1_cl <- read.csv("trans_control_1_cl.csv")
control_2_cl <- read.csv("trans_control_2_cl.csv")
datasets <- c("met1_trans_clean", "met2_trans_clean", "SSRI_trans_clean", "SNRI_trans_clean", "TCA_trans_clean",
              "bb1_trans_clean", "bb2_trans_clean", "control_1_cl", "control_2_cl" )
for(i in 1:length(datasets)){
  
  
  name <- datasets[i]
  name_keep <- paste(name, "_keep", sep = "" )
  file <- paste(name, ".csv", sep = "")
  id_name <- paste(name, "_id", sep = "")
  time_name <- paste(name, "_time", sep = "")
  
  do  <- paste(name_keep, " <- de_id(", name, " , id_key = id_key )", sep = "")#
  eval(parse(text = do))
  do <- paste("write.csv(", name_keep, ", file = '", name_keep, ".csv')", sep = "" )
  eval(parse(text = do))
  
  do <- paste(id_name, "<- nut_descr_id( ", name_keep,", id_name = 'new_id')",  sep = "")
  eval(parse(text = do))
  do <- paste("write.csv(", id_name, ", file = '", id_name, ".csv')", sep = "" )
  eval(parse(text = do))
  
  do <- paste(time_name, "<- nut_descr_time( ", name_keep,")",  sep = "")
  eval(parse(text = do))
  do <- paste("write.csv(", time_name, ", file = '", time_name, ".csv')", sep = "" )
  eval(parse(text = do))

  quant_name <- paste(name, "quant_tu", sep = "_")
  do = paste( quant_name, "<- desc_nut_all(data =", name, ")" )
  eval(parse(text = do))
  
  do <- paste("save(", quant_name, ", file = '", quant_name, ".RData')", sep = "")
  eval(parse(text = do))
}







