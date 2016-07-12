#test plot_rx_clean

Al_In <- getNewIDs(Alphaglucosidase_Inhibitors, rx = rx_keep, HHSizes = c(1,2))
#Drug brands in database matching drug set 1:
#  PRECOSE    
#n = 0
GLP <- getNewIDs(GLP_1_Receptor_Agonists, rx = rx_keep, HHSizes = c(1,2))
#Drug brands in database matching drug set 1:
#  BYETTA    
#n = 22 
Met <- getNewIDs(Metformin, rx = rx_keep, HHSizes = c(1,2))
#Drug brands in database matching drug set 1:
#  METFORMIN (ALL)    JANUMET    GLUCOPHAGE (ALL)    AVANDAMET    METFORMIN    GLUCOPHAGE    
#n = 390 
SGLT2 <- getNewIDs(SGLT2_inhibitors, rx = rx_keep, HHSizes = c(1,2))
#Drug brands in database matching drug set 1:
#n = 0 

Insulin_Sec <- getNewIDs(Insulin_Secretagogues, rx = rx_keep, HHSizes = c(1,2))
#Drug brands in database matching drug set 1:
#  GLIPIZIDE (ALL)    GLYBURIDE    GLIMEPIRIDE    PRANDIN    MICRONASE    GLUCOTROL XL    STARLIX    AMARYL    GLIPIZIDE    GLYNASE    DIABETA    
#n = 233 
Insulin <- getNewIDs(Insulin, rx = rx_keep, HHSizes = c(1,2))
#Drug brands in database matching drug set 1:
#  NOVOLOG 70/30    LANTUS    NOVOLIN (ALL)    NOVOLOG (ALL)    HUMALOG (ALL)    HUMULIN (ALL)    LEVEMIR    HUMALOG    NOVOLIN N    APIDRA    NOVOLOG (ASPART)    HUMULIN 70/30    HUMULIN N    HUMALOG MIX 75/25    NOVOLIN R    
#n = 211 
Thia <- getNewIDs(Thiazolidinediones, rx = rx_keep, HHSizes = c(1,2))
#Drug brands in database matching drug set 1:
#  AVANDIA    ACTOS    
#n = 100 
Sulf <- getNewIDs(Sulfonylureas, rx = rx_keep, HHSizes = c(1,2))
#Drug brands in database matching drug set 1:
#  GLIPIZIDE (ALL)    GLYBURIDE    GLIMEPIRIDE    MICRONASE    GLUCOTROL XL    AMARYL    GLIPIZIDE    GLYNASE    DIABETA    
#n = 226 

DPPIV <- getNewIDs(DPPIV, rx = rx_keep, HHSizes = c(1,2))
#Drug brands in database matching drug set 1:
#  JANUVIA    TRADJENTA    
#n = 77 

weightLoss <- c(GLP_1_Receptor_Agonists,Metformin)
weightLoss_id <- unique(c(GLP$IDs[[1]], Met$IDs[[1]]))

weightGain <- c(Insulin_Secretagogues,Insulin,Thiazolidinediones,Sulfonylureas)
weightGain_id <- unique(c(Insulin_Sec$IDs[[1]], Insulin$IDs[[1]], Thia$IDs[[1]], Sulf$IDs[[1]]))

overlap <- weightLoss_id[weightLoss_id %in% weightGain_id]
